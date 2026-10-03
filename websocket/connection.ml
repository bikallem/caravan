open Affect
open Caravan
open Caravan.Private

type message = Text of string | Binary of string

exception Closed of { code : int; reason : string }

type t = {
  flow : Flow.t;
  r : Buf_reader.t;
  w : Buf_writer.t;
  lock : Semaphore.t; (* one frame at a time on [w] *)
  max_message : int;
  closed : exn option Atomic.t; (* the first [Closed] exception *)
}

let make ?(max_message = 8 * 1024 * 1024) flow =
  { flow; r = Buf_reader.of_flow flow; w = Buf_writer.make (Flow.write flow);
    lock = Semaphore.make 1; max_message; closed = Atomic.make None }

(* RFC 6455 §7.4: the codes that a Close frame can contain. *)
let is_permitted code =
  (code >= 1000 && code <= 1003) || (code >= 1007 && code <= 1014)
  || (code >= 3000 && code <= 4999)

let check_open t = Option.iter raise (Atomic.get t.closed)

(* If the connection is open, this function keeps [code] and [reason] as its
   [Closed] exception. Then it closes the flow and gives the exception that
   the connection keeps. *)
let shut t code reason =
  ignore (Atomic.compare_and_set t.closed None (Some (Closed { code; reason })));
  Flow.close t.flow;
  Option.get (Atomic.get t.closed)

let write_frame t frame =
  Semaphore.with_acquired t.lock (fun () ->
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

(* Sends a Close frame with [code], then closes the connection. The flow
   can be closed at this time. Thus, we ignore a write error. *)
let fail t code reason =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 code;
  (try write_frame t { fin = true; opcode = Close; payload = Bytes.unsafe_to_string b }
  with Closed _ -> ());
  raise (shut t code reason)

(* The peer sent a Close frame. We send a Close frame with the same code
   (RFC 6455 §5.5.1), and then we close the connection. *)
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
  (try write_frame t { fin = true; opcode = Close; payload = echo } with Closed _ -> ());
  raise (shut t code reason)

let message t (op : Frame.opcode) payload =
  if op <> Text then Binary payload
  else if String.is_valid_utf_8 payload then Text payload
  else fail t 1007 "text is not UTF-8"

(* [part] is the opcode and the data of a fragmented message, if one is
   open. Control frames between fragments do not count in the size of the
   message. *)
let recv t =
  check_open t;
  let rec next part =
    let size = match part with None -> 0 | Some (_, b) -> Buffer.length b in
    match Frame.read t.r ~masked:true ~max:(Int.max 125 (t.max_message - size)) with
    | exception Frame.Error (Protocol_error reason) -> fail t 1002 reason
    | exception Frame.Error Too_big -> fail t 1009 "message is too large"
    | exception (End_of_file | Flow.Closed | Flow.Timeout) -> raise (shut t 1006 "")
    | { fin; opcode; payload } -> (
        match (opcode, part) with
        | Ping, _ ->
            write_frame t { fin = true; opcode = Pong; payload };
            next part
        | Pong, _ -> next part
        | Close, _ -> peer_closed t payload
        | _ when (not (Frame.is_control opcode)) && size + String.length payload > t.max_message ->
            fail t 1009 "message is too large"
        | (Text | Binary), Some _ -> fail t 1002 "new message inside a fragmented message"
        | Continuation, None -> fail t 1002 "continuation frame without a message"
        | (Text | Binary), None when fin -> message t opcode payload
        | (Text | Binary), None ->
            let b = Buffer.create (2 * String.length payload) in
            Buffer.add_string b payload;
            next (Some (opcode, b))
        | Continuation, Some (op, b) ->
            Buffer.add_string b payload;
            if fin then message t op (Buffer.contents b) else next part)
  in
  next None
