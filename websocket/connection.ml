open Affect
open Caravan
open Caravan.Private
module Mtime = Affect_unix.Mtime

type message = Text of string | Binary of string

exception Closed of { code : int; reason : string }

type t = {
  flow : Flow.t;
  r : Buf_reader.t;
  w : Buf_writer.t;
  writer : Semaphore.t; (* one frame at a time on [w] *)
  reader : Semaphore.t; (* one fiber at a time reads frames *)
  protocol : string option;
  max_message : int;
  close_timeout : Mtime.Span.t;
  write_timeout : Mtime.Span.t;
  read_timeout : Mtime.Span.t;
  idle_timeout : Mtime.Span.t option;
  mutable in_frame : bool; (* a byte of the frame came: with [reader] *)
  deadlines : Mutex.t; (* the deadlines change one at a time *)
  close_at : Mtime.t option Atomic.t; (* the deadline of [close] *)
  mutable read_at : Mtime.t option; (* the read deadline, with [deadlines] *)
  mutable write_at : Mtime.t option; (* the write deadline, with [deadlines] *)
  closing : exn option Atomic.t; (* we sent a Close frame: what [send] raises *)
  closed : exn Cell.Once.t; (* the first [Closed] exception *)
}

(* RFC 6455 §7.4: the codes that a Close frame can contain. *)
let is_permitted code =
  (code >= 1000 && code <= 1003) || (code >= 1007 && code <= 1014)
  || (code >= 3000 && code <= 4999)

let protocol t = t.protocol

let earlier a b =
  match (a, b) with
  | Some x, Some y -> if Mtime.is_earlier x ~than:y then a else b
  | d, None | None, d -> d

(* The deadlines change with [t.deadlines]: thus a deadline from a read or
   a write and the deadline of [close] cannot replace each other. The
   critical sections do not suspend the fiber. *)

(* Sets the read deadline to the earlier of the time [span] after the call
   and the deadline of [close]. With no [span], only the deadline of
   [close] applies. *)
let set_read_deadline t span =
  Mutex.protect t.deadlines (fun () ->
      let d = Option.bind span (fun span -> Mtime.add_span (Mtime.now ()) span) in
      t.read_at <- earlier d (Atomic.get t.close_at);
      Flow.set_read_deadline t.flow t.read_at)

let set_write_deadline t span =
  Mutex.protect t.deadlines (fun () ->
      t.write_at <- earlier (Mtime.add_span (Mtime.now ()) span) (Atomic.get t.close_at);
      Flow.set_write_deadline t.flow t.write_at)

(* The deadline of [close] stops also the reads and writes that wait at
   this time, but it does not make their deadlines later. *)
let set_close_deadline t d =
  Mutex.protect t.deadlines (fun () ->
      Atomic.set t.close_at d;
      t.read_at <- earlier t.read_at d;
      t.write_at <- earlier t.write_at d;
      Flow.set_read_deadline t.flow t.read_at;
      Flow.set_write_deadline t.flow t.write_at)

(* The reads of [t.r]. Inside a frame, each read gets the deadline
   [read_timeout] after its start. Thus a slow frame with no long pause
   can take more time than [read_timeout]. *)
let read_bytes t buf off len =
  if t.in_frame then set_read_deadline t (Some t.read_timeout);
  let n = Flow.read t.flow buf off len in
  (* A byte of the frame came, or the input ended: then [Frame.read] stops
     with End_of_file. *)
  t.in_frame <- true;
  n

let make ?protocol ?(max_message = 8 * 1024 * 1024) ?(close_timeout = Mtime.Span.(5 * s))
    ?(write_timeout = Mtime.Span.(30 * s)) ?(read_timeout = Mtime.Span.(30 * s)) ?idle_timeout
    flow =
  if max_message < 0 then invalid_arg "Caravan_websocket: max_message is negative";
  (* [read_bytes] is necessary to make [r], and it needs the connection. *)
  let self = ref None in
  let read buf off len = read_bytes (Option.get !self) buf off len in
  (* The first [recv] allocates the 4 KiB buffer of [r]. Small frames then
     need one read of the flow, not one for each part of the frame. *)
  let t =
    { flow; r = Buf_reader.make read; w = Buf_writer.make (Flow.write flow);
      writer = Semaphore.make 1; reader = Semaphore.make 1; protocol; max_message; close_timeout;
      write_timeout; read_timeout; idle_timeout; in_frame = false; deadlines = Mutex.create ();
      close_at = Atomic.make None; read_at = None; write_at = None; closing = Atomic.make None;
      closed = Cell.Once.make () }
  in
  self := Some t;
  t

let is_closing t = Atomic.get t.closing <> None || Cell.Once.is_set t.closed

let check_closed t = if Cell.Once.is_set t.closed then raise (Cell.Once.get t.closed)

(* If the connection is open, this function keeps [code] and [reason] as its
   [Closed] exception. Then it closes the flow and gives the exception that
   the connection keeps. *)
let shut t code reason =
  Cell.Once.try_set t.closed (Closed { code; reason });
  Flow.close t.flow;
  Cell.Once.get t.closed

(* An exception in the middle of a frame, for example a cancel, puts the
   stream out of step. Thus the connection closes with 1006, and the
   exception goes on. Call it first in the handler, for the backtrace. *)
let abort t e =
  let bt = Printexc.get_raw_backtrace () in
  ignore (shut t 1006 "");
  Printexc.raise_with_backtrace e bt

(* After our Close frame, no data frame and no Pong can go (RFC 6455
   §5.5.1). The check is in the semaphore, thus a frame cannot go after a
   Close frame that a different fiber wrote first. *)
let write_frame t (frame : Frame.t) =
  Semaphore.with_acquired t.writer (fun () ->
      check_closed t;
      match (frame.opcode, Atomic.get t.closing) with
      | Pong, Some _ -> ()
      | (Text | Binary | Continuation | Ping), Some e -> raise e
      | _ -> (
          try
            set_write_deadline t t.write_timeout;
            Frame.write t.w frame;
            Buf_writer.flush t.w
          with
          | Flow.Closed | Flow.Timeout -> raise (shut t 1006 "")
          | e -> abort t e))

let send t msg =
  let frame opcode payload = { Frame.fin = true; opcode; payload } in
  match msg with
  | Text s ->
      if not (String.is_valid_utf_8 s)
      then invalid_arg "Caravan_websocket.send: text is not valid UTF-8";
      write_frame t (frame Text s)
  | Binary s -> write_frame t (frame Binary s)

let ping t data =
  if String.length data > 125 then invalid_arg "Caravan_websocket.ping: more than 125 bytes";
  write_frame t { fin = true; opcode = Ping; payload = data }

(* [true] if we did not send a Close frame: then the caller sends it, and
   [send] raises [exn]. *)
let claim_close t exn = Atomic.compare_and_set t.closing None (Some exn)

(* A different fiber can close the flow before this write. Thus we ignore
   a write error. *)
let write_close t payload =
  try write_frame t { fin = true; opcode = Close; payload } with Closed _ -> ()

let code_payload code =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 code;
  Bytes.unsafe_to_string b

(* A close of a socket with unread input sends a reset, and a reset can
   delete our Close frame before the peer reads it. Thus we send the end of
   our stream first. Then we discard the input until the peer closes, for
   at most the close timeout and 1 MiB (RFC 9112 §9.6 does the same for
   HTTP). Only the fiber that reads frames can call it. *)
let linger t =
  set_read_deadline t (Some t.close_timeout);
  let buf = Bytes.create 4096 in
  let rec discard left =
    if left > 0
    then match Flow.read t.flow buf 0 (Int.min left (Bytes.length buf)) with
    | 0 -> ()
    | n -> discard (left - n)
  in
  (* A different fiber can close the flow at any time. *)
  try
    Flow.shutdown_send t.flow;
    discard (1024 * 1024)
  with Flow.Closed | Flow.Timeout -> ()

(* The peer does not obey the protocol, or it is idle. We send a Close
   frame with [code] if we did not send one, then close the connection. If
   [discard], we linger first. An idle peer leaves no unread input, thus an
   idle timeout does not linger. *)
let fail ?(discard = true) t code reason =
  if claim_close t (Closed { code; reason })
  then
    (* A cancel in the wait to write or in the linger closes the
       connection too. *)
    Fun.protect
      ~finally:(fun () -> ignore (shut t code reason))
      (fun () ->
         write_close t (code_payload code);
         if discard then linger t);
  raise (shut t code reason)

(* The peer sent a Close frame. If we did not send one, we send a Close
   frame with the same code (RFC 6455 §5.5.1). Then we close the
   connection. *)
let peer_closed t payload =
  let code, reason =
    match String.length payload with
    | 0 -> (1005, "")
    | 1 -> fail t 1002 "close frame of 1 byte"
    | n ->
        let code = String.get_uint16_be payload 0 and reason = String.sub payload 2 (n - 2) in
        if not (is_permitted code) then fail t 1002 (Printf.sprintf "close code %d" code);
        if not (String.is_valid_utf_8 reason) then fail t 1007 "close reason is not UTF-8";
        (code, reason)
  in
  let echo = String.sub payload 0 (Int.min 2 (String.length payload)) in
  if claim_close t (Closed { code; reason }) then write_close t echo;
  raise (shut t code reason)

let after_close_deadline t =
  match Atomic.get t.close_at with
  | Some c -> not (Mtime.is_earlier (Mtime.now ()) ~than:c)
  | None -> false

let message t (op : Frame.opcode) payload =
  if op <> Text then Binary payload
  else if String.is_valid_utf_8 payload then Text payload
  else fail t 1007 "text is not UTF-8"

(* Adds [s] to the [len] bytes of the message buffer [b]. The capacity
   doubles, but it never goes over [max_message]: [Frame.read] makes sure
   that the message fits. *)
let append t (b, len) s =
  let n = String.length s in
  let b =
    if len + n <= Bytes.length b
    then b
    else
    let cap = Int.min t.max_message (Int.max (len + n) (2 * Bytes.length b)) in
    Bytes.extend b 0 (cap - Bytes.length b)
  in
  Bytes.blit_string s 0 b len n;
  (b, len + n)

(* Reads frames until a full message. [part] is the opcode, the buffer and
   the length of a fragmented message, if one is open. The buffer starts as
   the first payload, without a copy. Control frames between fragments
   do not count in the size of the message. After we sent a Close frame,
   this function discards data frames and sends no Pong: it ends only with
   [Closed]. *)
let rec next t part =
  let size = match part with None -> 0 | Some (_, (_, len)) -> len in
  (* The idle timeout applies until the first byte of the frame. The bytes
     of the frame can be in [t.r] at this time. *)
  t.in_frame <- Buf_reader.buffered t.r > 0;
  if not t.in_frame then set_read_deadline t t.idle_timeout;
  let start = Buf_reader.consumed t.r in
  match Frame.read t.r ~masked:true ~max:(t.max_message - size) with
  | exception Frame.Error (Protocol_error reason) -> fail t 1002 reason
  | exception Frame.Error Too_big -> fail t 1009 "message is too large"
  | exception Flow.Timeout when Atomic.get t.closing = None ->
      if t.in_frame then fail t 1001 "read timeout" else fail ~discard:false t 1001 "idle timeout"
  | exception (End_of_file | Flow.Closed | Flow.Timeout) -> raise (shut t 1006 "")
  (* Between two messages, an exception before the first byte of a frame,
     for example a cancel, leaves the stream in step. *)
  | exception e when part = None && Buf_reader.consumed t.r = start -> raise e
  | exception e -> abort t e
  | { fin; opcode; payload } -> (
      match (opcode, part) with
      | Close, _ -> peer_closed t payload
      | _ when Atomic.get t.closing <> None ->
          (* A read that finds bytes does not wait, thus its deadline does
             not stop a peer that sends without a stop. *)
          if after_close_deadline t then raise (shut t 1006 "") else next t part
      | Ping, _ -> (
          (* An exception before the Pong goes, for example a cancel in
             the wait for the writer, loses the fragments of [part]. *)
          match write_frame t { fin = true; opcode = Pong; payload } with
          | () -> next t part
          | exception e when part <> None -> abort t e)
      | Pong, _ -> next t part
      | (Text | Binary), Some _ -> fail t 1002 "new message inside a fragmented message"
      | Continuation, None -> fail t 1002 "continuation frame without a message"
      | (Text | Binary), None when fin -> message t opcode payload
      | (Text | Binary), None ->
          (* [Frame.read] gives a new string: nothing else has it. *)
          next t (Some (opcode, (Bytes.unsafe_of_string payload, String.length payload)))
      | Continuation, Some (op, data) ->
          let ((b, len) as data) = append t data payload in
          if not fin
          then next t (Some (op, data))
          else if len = Bytes.length b
          then message t op (Bytes.unsafe_to_string b)
          else message t op (Bytes.sub_string b 0 len))

let recv t =
  Semaphore.with_acquired t.reader (fun () ->
      check_closed t;
      next t None)

let close ?(code = 1000) ?(reason = "") t =
  if not (is_permitted code) then invalid_arg "Caravan_websocket.close: code not permitted";
  if String.length reason > 123 || not (String.is_valid_utf_8 reason)
  then invalid_arg "Caravan_websocket.close: reason is not UTF-8 of at most 123 bytes";
  if (not (Cell.Once.is_set t.closed)) && claim_close t (Closed { code; reason })
  then begin
    set_close_deadline t (Mtime.add_span (Mtime.now ()) t.close_timeout);
    write_close t (code_payload code ^ reason);
    (* A fiber in [recv] reads the peer's Close frame. If no fiber reads,
       we read it. *)
    let read = Semaphore.acquire' t.reader `Read
    and finished = Action.map (fun _ -> `Finished) (Cell.Once.get' t.closed) in
    match Action.invoke (Action.choose [ finished; read ]) with
    | `Finished -> ()
    | `Read ->
        Fun.protect ~finally:(fun () -> Semaphore.release t.reader) (fun () ->
            try check_closed t; ignore (next t None) with Closed _ -> ())
  end
