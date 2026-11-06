open Weft
open Caravan
open Caravan.Private

type message = Text of string | Binary of string

exception Closed of {code: int; reason: string}

type t =
  { flow: Flow.t
  ; r: Buf_reader.t
  ; w: Buf_writer.t
  ; writer: Fiber.Semaphore.t (* one frame at a time on [w] *)
  ; reader: Fiber.Semaphore.t (* one fiber at a time reads frames *)
  ; protocol: string option
  ; max_message: int
  ; close_timeout: Span.t
  ; read_timeout: Span.t option (* [Some] of the read timeout, made once *)
  ; idle_timeout: Span.t option
  ; mutable in_frame: bool (* a byte of the frame came: with [reader] *)
  ; close_at: Clock.mono option Atomic.t (* the deadline of [close], set once *)
  ; closing: exn option Atomic.t (* we sent a Close frame: what [send] raises *)
  ; closed: exn Ivar.t (* the first [Closed] exception *) }

(* RFC 6455 §7.4: the codes that a Close frame can contain. *)
let is_permitted code =
  (code >= 1000 && code <= 1003)
  || (code >= 1007 && code <= 1014)
  || (code >= 3000 && code <= 4999)

let protocol t = t.protocol

(* The flow has a timeout for each read and each write that waits, and an
   absolute deadline for each direction. A read or a write that waits ends at
   the earlier of the two. The connection sets the read timeout in the fiber
   that reads, and the write timeout one time in [make]. Only
   [set_close_deadline] sets the absolute deadlines. Thus no lock is necessary.

   The first [close], or the first linger of [fail], sets the close deadline.
   Only the fiber that wins the compare-and-set sets the two absolute deadlines
   of the flow, thus each deadline of the flow has one writer. A later close
   never gives an earlier deadline, because all closes use [close_timeout] on a
   monotonic clock. The approximation: if two closes run at the same time, the
   first compare-and-set wins, also if the other close read the clock a little
   earlier. The deadline also stops the reads and writes that wait at this
   time. *)
let set_close_deadline t =
  let d = Deadline.from_now t.close_timeout in
  if Atomic.compare_and_set t.close_at None d then begin
    Flow.set_read_deadline t.flow d ;
    Flow.set_write_deadline t.flow d
  end

(* The reads of [t.r]. Each read of the flow sets the timeout of its wait. Until
   the first byte of a frame, the timeout is [idle_timeout]. Inside a frame,
   each read that waits gets the timeout [read_timeout] from the start of its
   wait. Thus a slow frame with no long pause can take more time than
   [read_timeout]. *)
let read_bytes t buf off len =
  Flow.set_read_timeout t.flow
    (if t.in_frame then t.read_timeout else t.idle_timeout) ;
  let n = Flow.read t.flow buf off len in
  (* A byte of the frame came, or the input ended: then [Frame.read] stops with
     End_of_file. *)
  t.in_frame <- true ;
  n

let make ?protocol ?(max_message = 8 * 1024 * 1024) ?(close_timeout = Span.s 5)
    ?(write_timeout = Span.s 30) ?(read_timeout = Span.s 30) ?idle_timeout flow
    =
  if max_message < 0 then
    invalid_arg "Caravan_websocket: max_message is negative" ;
  (* Each write that waits gets the write timeout. A frame takes one or two
     writes of the flow. *)
  Flow.set_write_timeout flow (Some write_timeout) ;
  (* [read_bytes] is necessary to make [r], and it needs the connection. *)
  let self = ref None in
  let read buf off len = read_bytes (Option.get !self) buf off len in
  (* The first [recv] allocates the 4 KiB buffer of [r]. Small frames then need
     one read of the flow, not one for each part of the frame. *)
  let t =
    { flow
    ; r= Buf_reader.make read
    ; w= Buf_writer.make (Flow.write flow)
    ; writer= Fiber.Semaphore.create 1
    ; reader= Fiber.Semaphore.create 1
    ; protocol
    ; max_message
    ; close_timeout
    ; read_timeout= Some read_timeout
    ; idle_timeout
    ; in_frame= false [@mutate off "next sets in_frame before each read of r"]
    ; close_at= Atomic.make None
    ; closing= Atomic.make None
    ; closed= Ivar.create () }
  in
  self := Some t ;
  t

(* [Sys.opaque_identity] stops a compiler that inlines a constant code and
   reason from making one static [Closed] value for all connections: then the
   physical compare of [is_own_closed] would fail. *)
let closed_exn code reason = Closed {code= Sys.opaque_identity code; reason}

let is_closing t =
  Atomic.get t.closing <> None || Option.is_some (Ivar.peek t.closed)

let check_closed t = Option.iter raise (Ivar.peek t.closed)

(* The connection raises only two [Closed] values: the exception of [closing]
   and that of [closed]. Thus a physical compare tells the [Closed] of [t] from
   that of a different connection, also with the same code and reason. Each of
   these values comes from [closed_exn]: a new block for each connection. *)
let is_own_closed t e =
  (match Atomic.get t.closing with Some x -> x == e | None -> false)
  || match Ivar.peek t.closed with Some x -> x == e | None -> false

(* If the connection is open, this function keeps [code] and [reason] as its
   [Closed] exception. Then it closes the flow and gives the exception that the
   connection keeps. *)
let shut t code reason =
  ignore (Ivar.try_fill t.closed (closed_exn code reason)) ;
  Flow.close t.flow ;
  Option.get (Ivar.peek t.closed)

(* An exception in the middle of a frame, for example a cancel, puts the stream
   out of step. Thus the connection closes with 1006, and the exception goes on.
   Call it first in the handler, for the backtrace. *)
let abort t e =
  let bt = Printexc.get_raw_backtrace () in
  ignore (shut t 1006 "") ;
  Printexc.raise_with_backtrace e bt

(* After our Close frame, no data frame and no Pong can go (RFC 6455 §5.5.1).
   The check is in the semaphore, thus a frame cannot go after a Close frame
   that a different fiber wrote first. *)
let write_frame t (frame : Frame.t) =
  Fiber.Semaphore.use t.writer (fun () ->
      check_closed t ;
      match (frame.opcode, Atomic.get t.closing) with
      | Pong, Some _ ->
          ()
      | (Text | Binary | Continuation | Ping), Some e ->
          raise e
      | _ -> (
        try Frame.write t.w frame ; Buf_writer.flush t.w with
        | Flow.Closed | Flow.Timeout ->
            raise (shut t 1006 "")
        | e ->
            abort t e ) )

let send t msg =
  let frame opcode payload = {Frame.fin= true; opcode; payload} in
  match msg with
  | Text s ->
      if not (String.is_valid_utf_8 s) then
        invalid_arg "Caravan_websocket.send: text is not valid UTF-8" ;
      write_frame t (frame Text s)
  | Binary s ->
      write_frame t (frame Binary s)

let ping t data =
  if String.length data > 125 then
    invalid_arg "Caravan_websocket.ping: more than 125 bytes" ;
  write_frame t {fin= true; opcode= Ping; payload= data}

(* [true] if we did not send a Close frame: then the caller sends it, and [send]
   raises [exn]. *)
let claim_close t exn = Atomic.compare_and_set t.closing None (Some exn)

(* A different fiber can close the flow before this write. Thus we ignore a
   write error. *)
let write_close t payload =
  try write_frame t {fin= true; opcode= Close; payload} with Closed _ -> ()

let code_payload code =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 code ;
  Bytes.unsafe_to_string b

(* A reset can delete our Close frame before the peer reads it. Thus we linger,
   for at most the close timeout and 1 MiB. The linger ends at the close
   deadline: the deadline of the first close, if one, else the time
   [close_timeout] from now. [Flow.linger] removes the read timeout. Only the
   fiber that reads frames can call it. *)
let linger t =
  set_close_deadline t ;
  Flow.linger t.flow ~max:(1024 * 1024)

(* The peer does not obey the protocol, or it is idle. We send a Close frame
   with [code] if we did not send one, then close the connection. If [discard],
   we linger first. An idle peer leaves no unread input, thus an idle timeout
   does not linger. *)
let fail ?(discard = true) t code reason =
  if claim_close t (closed_exn code reason) then
    (* A cancel in the wait to write or in the linger closes the connection
       too. *)
    Fun.protect
      ~finally:(fun () -> ignore (shut t code reason))
      (fun () ->
        write_close t (code_payload code) ;
        if discard then linger t ) ;
  raise (shut t code reason)

(* The peer sent a Close frame. If we did not send one, we send a Close frame
   with the same code (RFC 6455 §5.5.1). Then we close the connection. *)
let peer_closed t payload =
  let code, reason =
    match String.length payload with
    | 0 ->
        (1005, "")
    | 1 ->
        fail t 1002 "close frame of 1 byte"
    | n ->
        let code = String.get_uint16_be payload 0
        and reason = String.sub payload 2 (n - 2) in
        if not (is_permitted code) then
          fail t 1002 (Printf.sprintf "close code %d" code) ;
        if not (String.is_valid_utf_8 reason) then
          fail t 1007 "close reason is not UTF-8" ;
        (code, reason)
  in
  let echo = String.sub payload 0 (Int.min 2 (String.length payload)) in
  if claim_close t (closed_exn code reason) then
    (* A cancel in the wait to write closes the connection too. *)
    Fun.protect
      ~finally:(fun () -> ignore (shut t code reason))
      (fun () -> write_close t echo) ;
  raise (shut t code reason)

let after_close_deadline t = Deadline.has_passed (Atomic.get t.close_at)

(* Raised by the UTF-8 check of a part of a text message. *)
exception Bad_text

(* Adds [s] to the [len] bytes of the message buffer [b]. The capacity doubles,
   but it never goes over [max_message]: [Frame.read] makes sure that the
   message fits. *)
let append t (b, len) s =
  let n = String.length s in
  let b =
    if len + n <= Bytes.length b then b
    else
      let cap =
        Int.min t.max_message
          (Int.max (len + n)
             ( (2
               [@mutate off "a capacity: another growth gives the same message"]
               ) * Bytes.length b ) )
      in
      Bytes.extend b 0 (cap - Bytes.length b)
  in
  Bytes.blit_string s 0 b len n ;
  (b, len + n)

(* Reads frames until a full message. [part] is the opcode, the buffer and the
   length of a fragmented message, if one is open, and the bytes of an
   incomplete UTF-8 sequence at its end. The buffer starts as the first payload,
   without a copy. Control frames between fragments do not count in the size of
   the message. After we sent a Close frame, this function discards data frames
   and sends no Pong: it ends only with [Closed]. *)
let rec next t part =
  let size = match part with None -> 0 | Some (_, (_, len), _) -> len in
  (* The idle timeout applies until the first byte of the frame. The bytes of
     the frame can be in [t.r] at this time. *)
  t.in_frame <- Buf_reader.buffered t.r > 0 ;
  let start = Buf_reader.consumed t.r in
  (* The UTF-8 of a text message, checked for each part of a frame as it
     arrives, after the [pending] bytes of an incomplete sequence (RFC 6455
     §8.1: we fail at the first incorrect byte, not at the end of the frame). A
     binary message has no check. While the connection closes, data frames are
     dropped, and they get no check. *)
  let pending =
    ref (match part with Some (Frame.Text, _, pending) -> pending | _ -> "")
  in
  let on_data (op : Frame.opcode) buf off len =
    if Atomic.get t.closing = None then
      match (op, part) with
      | Text, None | Continuation, Some (Text, _, _) -> (
        match Utf8.continue_bytes !pending buf off len with
        | p ->
            pending := p
        | exception Utf8.Invalid ->
            raise Bad_text )
      | _ ->
          ()
  in
  match Frame.read ~on_data t.r ~masked:true ~max:(t.max_message - size) with
  | exception Bad_text ->
      fail t 1007 "text is not UTF-8"
  | exception Frame.Error (Protocol_error reason) ->
      fail t 1002 reason
  | exception Frame.Error Too_big ->
      fail t 1009 "message is too large"
  | exception Flow.Timeout when Atomic.get t.closing = None ->
      if t.in_frame then fail t 1001 "read timeout"
      else fail ~discard:false t 1001 "idle timeout"
  | exception (End_of_file | Flow.Closed | Flow.Timeout) ->
      raise (shut t 1006 "")
  (* Between two messages, an exception before the first byte of a frame, for
     example a cancel, leaves the stream in step. *)
  | exception e when part = None && Buf_reader.consumed t.r = start ->
      raise e
  | exception e ->
      abort t e
  | {fin; opcode; payload} -> (
    match (opcode, part) with
    | Close, _ ->
        peer_closed t payload
    | _ when Atomic.get t.closing <> None ->
        (* A read that finds bytes does not wait, thus its deadline does not
           stop a peer that sends without a stop. *)
        if after_close_deadline t then raise (shut t 1006 "") else next t part
    | Ping, _ -> (
      (* An exception before the Pong goes, for example a cancel in the wait for
         the writer, loses the fragments of [part]. *)
      match write_frame t {fin= true; opcode= Pong; payload} with
      | () ->
          next t part
      | exception e when part <> None ->
          abort t e )
    | Pong, _ ->
        next t part
    | (Text | Binary), Some _ ->
        fail t 1002 "new message inside a fragmented message"
    | Continuation, None ->
        fail t 1002 "continuation frame without a message"
    | (Text | Binary), None when fin ->
        if opcode = Binary then Binary payload
        else if !pending <> "" then fail t 1007 "text is not UTF-8"
        else Text payload
    | (Text | Binary), None ->
        let pending = !pending in
        (* [Frame.read] gives a new string: nothing else has it. *)
        next t
          (Some
             ( opcode
             , (Bytes.unsafe_of_string payload, String.length payload)
             , pending ) )
    | Continuation, Some (op, data, _) ->
        let pending = !pending in
        if fin && pending <> "" then fail t 1007 "text is not UTF-8" ;
        let ((b, len) as data) = append t data payload in
        if not fin then next t (Some (op, data, pending))
        else begin
          (* The fragments are correct UTF-8: no check again. *)
          let s =
            if len = Bytes.length b then Bytes.unsafe_to_string b
            else Bytes.sub_string b 0 len
          in
          if op = Text then Text s else Binary s
        end )

let recv t =
  Fiber.Semaphore.use t.reader (fun () -> check_closed t ; next t None)

(* If a close started (a Close frame went or waits to go), waits until the
   connection closes. A fiber in [recv] reads the peer's Close frame. If no
   fiber reads, we read it. [close] sets the close deadline, and [fail] and
   [peer_closed] end with [shut], thus the wait has a limit. If the connection
   is closed, [finished] is ready at once. The sync takes the permit of the
   reader only if it selects [read]. *)
let wait_closed t =
  if Atomic.get t.closing <> None then begin
    let read = wrap (Fiber.Semaphore.acquire t.reader) (fun () -> `Read)
    and finished = wrap (Ivar.read t.closed) (fun _ -> `Finished) in
    (* [finished] ends the wait without the reader. With [read] alone, the wait
       also ends. A close of the flow wakes the read of the holder of the
       reader, and the holder then releases the reader. *)
    match sync (choose ([finished; read] [@mutate off "the same end"])) with
    | `Finished ->
        ()
    | `Read ->
        Fun.protect
          ~finally:(fun () -> Fiber.Semaphore.release t.reader)
          (fun () ->
            try
              (* [closing] is set, thus [next] drops each data frame. A read of
                 the closed flow raises [Closed]. *)
              begin[@mutate off "the same result"]
                check_closed t
              end ;
              ignore (next t None)
            with Closed _ -> () )
  end

let close ?(code = 1000) ?(reason = "") t =
  if not (is_permitted code) then
    invalid_arg "Caravan_websocket.close: code not permitted" ;
  if String.length reason > 123 || not (String.is_valid_utf_8 reason) then
    invalid_arg
      "Caravan_websocket.close: reason is not UTF-8 of at most 123 bytes" ;
  if Option.is_none (Ivar.peek t.closed) then begin
    (* Also if a different fiber started the close, for example to reply to the
       peer: we wait at most the close timeout. *)
    set_close_deadline t ;
    if claim_close t (closed_exn code reason) then
      write_close t (code_payload code ^ reason)
  end ;
  wait_closed t
