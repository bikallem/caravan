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
  closing : exn option Atomic.t; (* we sent a Close frame: what [send] raises *)
  closed : exn option Atomic.t; (* the first [Closed] exception *)
  finished : unit Cell.Once.t; (* set with [closed] *)
}

let make ?protocol ?(max_message = 8 * 1024 * 1024) ?(close_timeout = Mtime.Span.(5 * s)) flow =
  { flow; r = Buf_reader.of_flow flow; w = Buf_writer.make (Flow.write flow);
    writer = Semaphore.make 1; reader = Semaphore.make 1; protocol; max_message; close_timeout;
    closing = Atomic.make None; closed = Atomic.make None; finished = Cell.Once.make () }

(* RFC 6455 §7.4: the codes that a Close frame can contain. *)
let is_permitted code =
  (code >= 1000 && code <= 1003) || (code >= 1007 && code <= 1014)
  || (code >= 3000 && code <= 4999)

let protocol t = t.protocol
let check_closed t = Option.iter raise (Atomic.get t.closed)

let check_open t =
  check_closed t;
  Option.iter raise (Atomic.get t.closing)

(* If the connection is open, this function keeps [code] and [reason] as its
   [Closed] exception. Then it closes the flow and gives the exception that
   the connection keeps. *)
let shut t code reason =
  ignore (Atomic.compare_and_set t.closed None (Some (Closed { code; reason })));
  Flow.close t.flow;
  Cell.Once.try_set t.finished ();
  Option.get (Atomic.get t.closed)

let write_frame t frame =
  Semaphore.with_acquired t.writer (fun () ->
      try
        Frame.write t.w frame;
        Buf_writer.flush t.w
      with Flow.Closed | Flow.Timeout -> raise (shut t 1006 ""))

let send t msg =
  check_open t;
  let frame opcode payload = { Frame.fin = true; opcode; payload } in
  match msg with
  | Text s ->
      if not (String.is_valid_utf_8 s)
      then invalid_arg "Caravan_websocket.send: text is not valid UTF-8";
      write_frame t (frame Text s)
  | Binary s -> write_frame t (frame Binary s)

let ping t data =
  if String.length data > 125 then invalid_arg "Caravan_websocket.ping: more than 125 bytes";
  check_open t;
  write_frame t { fin = true; opcode = Ping; payload = data }

(* [true] if we did not send a Close frame: then the caller sends it, and
   [send] raises [exn]. *)
let claim_close t exn = Atomic.compare_and_set t.closing None (Some exn)

(* The flow can be closed at this time. Thus, we ignore a write error. *)
let write_close t payload =
  try write_frame t { fin = true; opcode = Close; payload } with Closed _ -> ()

let code_payload code =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 code;
  Bytes.unsafe_to_string b

(* Sends a Close frame with [code] if we did not send one, then closes the
   connection. *)
let fail t code reason =
  if claim_close t (Closed { code; reason }) then write_close t (code_payload code);
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

let message t (op : Frame.opcode) payload =
  if op <> Text then Binary payload
  else if String.is_valid_utf_8 payload then Text payload
  else fail t 1007 "text is not UTF-8"

(* Reads frames until a full message. [part] is the opcode and the data of
   a fragmented message, if one is open. Control frames between fragments
   do not count in the size of the message. After we sent a Close frame,
   this function discards data frames and sends no Pong: it ends only with
   [Closed]. *)
let rec next t part =
  let size = match part with None -> 0 | Some (_, b) -> Buffer.length b in
  match Frame.read t.r ~masked:true ~max:(Int.max 125 (t.max_message - size)) with
  | exception Frame.Error (Protocol_error reason) -> fail t 1002 reason
  | exception Frame.Error Too_big -> fail t 1009 "message is too large"
  | exception (End_of_file | Flow.Closed | Flow.Timeout) -> raise (shut t 1006 "")
  | { fin; opcode; payload } -> (
      match (opcode, part) with
      | Close, _ -> peer_closed t payload
      | _ when Atomic.get t.closing <> None -> next t part
      | Ping, _ ->
          write_frame t { fin = true; opcode = Pong; payload };
          next t part
      | Pong, _ -> next t part
      | _ when (not (Frame.is_control opcode)) && size + String.length payload > t.max_message ->
          fail t 1009 "message is too large"
      | (Text | Binary), Some _ -> fail t 1002 "new message inside a fragmented message"
      | Continuation, None -> fail t 1002 "continuation frame without a message"
      | (Text | Binary), None when fin -> message t opcode payload
      | (Text | Binary), None ->
          let b = Buffer.create (2 * String.length payload) in
          Buffer.add_string b payload;
          next t (Some (opcode, b))
      | Continuation, Some (op, b) ->
          Buffer.add_string b payload;
          if fin then message t op (Buffer.contents b) else next t part)

let recv t =
  Semaphore.with_acquired t.reader (fun () ->
      check_closed t;
      next t None)

let close ?(code = 1000) ?(reason = "") t =
  if not (is_permitted code) then invalid_arg "Caravan_websocket.close: code not permitted";
  if String.length reason > 123 || not (String.is_valid_utf_8 reason)
  then invalid_arg "Caravan_websocket.close: reason is not UTF-8 of at most 123 bytes";
  if Atomic.get t.closed = None && claim_close t (Closed { code; reason })
  then begin
    (* The deadlines reach also the fibers that read or write at this
       time. *)
    let deadline = Mtime.add_span (Mtime.now ()) t.close_timeout in
    Flow.set_write_deadline t.flow deadline;
    Flow.set_read_deadline t.flow deadline;
    write_close t (code_payload code ^ reason);
    (* A fiber in [recv] reads the peer's Close frame. If no fiber reads,
       we read it. *)
    let read = Semaphore.acquire' t.reader `Read
    and finished = Action.map (fun () -> `Finished) (Cell.Once.get' t.finished) in
    match Action.invoke (Action.choose [ finished; read ]) with
    | `Finished -> ()
    | `Read ->
        Fun.protect ~finally:(fun () -> Semaphore.release t.reader) (fun () ->
            try check_closed t; ignore (next t None) with Closed _ -> ())
  end
