open Caravan.Private

type opcode = Continuation | Text | Binary | Close | Ping | Pong
type t = { fin : bool; opcode : opcode; payload : string }
type error = Protocol_error of string | Too_big

exception Error of error

let protocol_error msg = raise (Error (Protocol_error msg))

let opcode_of_int = function
| 0 -> Continuation
| 1 -> Text
| 2 -> Binary
| 8 -> Close
| 9 -> Ping
| 10 -> Pong
| n -> protocol_error (Printf.sprintf "reserved opcode %d" n)

let int_of_opcode = function
| Continuation -> 0
| Text -> 1
| Binary -> 2
| Close -> 8
| Ping -> 9
| Pong -> 10

let is_control = function Close | Ping | Pong -> true | Continuation | Text | Binary -> false

(* XORs [len] bytes of [buf] from [off] with the 4-byte [key]. Masking and
   unmasking are the same operation. The loop does 8 bytes at a time with
   the key two times in a 64-bit word, then the remaining bytes. *)
let apply_mask key buf off len =
  let k = Int64.logand (Int64.of_int32 (String.get_int32_le key 0)) 0xffff_ffffL in
  let k = Int64.logor k (Int64.shift_left k 32) in
  let words = len / 8 in
  for w = 0 to words - 1 do
    let i = off + (8 * w) in
    Bytes.set_int64_le buf i (Int64.logxor (Bytes.get_int64_le buf i) k)
  done;
  for i = 8 * words to len - 1 do
    let b = Char.code (Bytes.get buf (off + i)) lxor Char.code key.[i land 3] in
    Bytes.set buf (off + i) (Char.chr b)
  done

let rec really_read r buf off len =
  if len > 0
  then begin
    let n = Buf_reader.read_into r buf off len in
    if n = 0 then raise End_of_file;
    really_read r buf (off + n) (len - n)
  end

(* Reads [len] bytes. The buffer grows as the bytes arrive, thus a peer that
   declares a large length and sends no payload does not take that memory. *)
let read_payload r len =
  let rec loop buf pos =
    if pos = len
    then buf
    else begin
      let buf = if pos < Bytes.length buf then buf else Bytes.extend buf 0 (Int.min pos (len - pos)) in
      let n = Buf_reader.read_into r buf pos (Bytes.length buf - pos) in
      if n = 0 then raise End_of_file;
      loop buf (pos + n)
    end
  in
  loop (Bytes.create (Int.min len 65536)) 0

let read r ~masked ~max =
  let hdr = Bytes.create 8 in
  really_read r hdr 0 2;
  let b0 = Bytes.get_uint8 hdr 0 and b1 = Bytes.get_uint8 hdr 1 in
  if b0 land 0x70 <> 0 then protocol_error "reserved bits set";
  let fin = b0 land 0x80 <> 0 and opcode = opcode_of_int (b0 land 0x0f) in
  if is_control opcode && not fin then protocol_error "fragmented control frame";
  if is_control opcode && b1 land 0x7f > 125
  then protocol_error "control frame longer than 125 bytes";
  if b1 land 0x80 <> 0 <> masked
  then protocol_error (if masked then "unmasked frame" else "masked frame");
  let len =
    match b1 land 0x7f with
    | 126 -> really_read r hdr 0 2; Bytes.get_uint16_be hdr 0
    | 127 ->
        really_read r hdr 0 8;
        let n = Bytes.get_int64_be hdr 0 in
        if Int64.compare n 0L < 0 then protocol_error "length with its top bit set";
        (* Compared before conversion: [n] may not fit an [int]. *)
        if Int64.compare n (Int64.of_int max) > 0 then raise (Error Too_big);
        Int64.to_int n
    | n -> n
  in
  if len > max && not (is_control opcode) then raise (Error Too_big);
  let key = if masked then (really_read r hdr 0 4; Bytes.sub_string hdr 0 4) else "" in
  let payload = read_payload r len in
  if masked then apply_mask key payload 0 len;
  { fin; opcode; payload = Bytes.unsafe_to_string payload }

let write ?mask w { fin; opcode; payload } =
  let len = String.length payload in
  if is_control opcode && ((not fin) || len > 125)
  then invalid_arg "Frame.write: a control frame must be final and at most 125 bytes";
  let hdr = Bytes.create 14 in
  Bytes.set_uint8 hdr 0 ((if fin then 0x80 else 0) lor int_of_opcode opcode);
  let m = if Option.is_some mask then 0x80 else 0 in
  let n =
    if len < 126 then (Bytes.set_uint8 hdr 1 (m lor len); 2)
    else if len < 0x10000
    then (Bytes.set_uint8 hdr 1 (m lor 126); Bytes.set_uint16_be hdr 2 len; 4)
    else (Bytes.set_uint8 hdr 1 (m lor 127); Bytes.set_int64_be hdr 2 (Int64.of_int len); 10)
  in
  match mask with
  | None ->
      Buf_writer.write_bytes w hdr 0 n;
      Buf_writer.write_string w payload
  | Some key ->
      if String.length key <> 4 then invalid_arg "Frame.write: the mask must be 4 bytes";
      Bytes.blit_string key 0 hdr n 4;
      Buf_writer.write_bytes w hdr 0 (n + 4);
      let p = Bytes.of_string payload in
      apply_mask key p 0 len;
      Buf_writer.write_bytes w p 0 len
