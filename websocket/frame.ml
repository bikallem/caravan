module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer

type opcode = Continuation | Text | Binary | Close | Ping | Pong

type t = {fin: bool; opcode: opcode; payload: string}

type error = Protocol_error of string | Too_big

exception Error of error

let protocol_error msg = raise (Error (Protocol_error msg))

let opcode_of_int = function
  | 0 ->
      Continuation
  | 1 ->
      Text
  | 2 ->
      Binary
  | 8 ->
      Close
  | 9 ->
      Ping
  | 10 ->
      Pong
  | n ->
      protocol_error (Printf.sprintf "reserved opcode %d" n)

let int_of_opcode = function
  | Continuation ->
      0
  | Text ->
      1
  | Binary ->
      2
  | Close ->
      8
  | Ping ->
      9
  | Pong ->
      10

let is_control = function
  | Close | Ping | Pong ->
      true
  | Continuation | Text | Binary ->
      false

(* XORs [len] bytes of [buf] from [off] with the 4-byte [key], from the byte
   [phase] of the key: a range that starts at the position [p] of a payload has
   the phase [p land 3]. Masking and unmasking are the same operation. The loop
   does 8 bytes at a time with the key two times in a 64-bit word, then the
   remaining bytes. *)
let apply_mask ?(phase = 0) key buf off len =
  let k =
    Int64.logand (Int64.of_int32 (String.get_int32_le key 0)) 0xffff_ffffL
  in
  (* The byte [j] of the word is the byte [j + phase] of the key: a turn of the
     little-endian word by [phase] bytes. *)
  let k =
    if phase = 0 then k
    else
      let s = 8 * phase in
      Int64.logand
        (Int64.logor
           (Int64.shift_right_logical k s)
           (Int64.shift_left k (32 - s)) )
        0xffff_ffffL
  in
  let k = Int64.logor k (Int64.shift_left k 32) in
  let words =
    len
    / (8 [@mutate off "the byte loop masks the remaining bytes the same way"])
  in
  for w = 0 to words - 1 do
    let i = off + (8 * w) in
    Bytes.set_int64_le buf i (Int64.logxor (Bytes.get_int64_le buf i) k)
  done ;
  for i = 8 * words to len - 1 do
    let b =
      Char.code (Bytes.get buf (off + i))
      lxor Char.code key.[(i + phase) land 3]
    in
    Bytes.set buf (off + i) (Char.chr b)
  done

(* Reads [len] bytes, unmasked with [key] if it is not [""]. The buffer grows as
   the bytes arrive, thus a peer that declares a large length and sends no
   payload does not take that memory. Each part that a read gives goes to
   [on_data] after its unmask, before the next read. *)
let read_payload r ~key ~on_data len =
  let rec loop buf pos =
    if pos = len then buf
    else begin
      let buf =
        if pos < Bytes.length buf then buf
        else Bytes.extend buf 0 (Int.min pos (len - pos))
      in
      let n = Reader.read_into r buf pos (Bytes.length buf - pos) in
      if n = 0 then raise End_of_file ;
      if key <> "" then apply_mask ~phase:(pos land 3) key buf pos n ;
      on_data buf pos n ;
      loop buf (pos + n)
    end
  in
  loop
    (Bytes.create
       (Int.min len (65536 [@mutate off "a capacity: the buffer grows to len"])) )
    0

(* The header: 2 bytes, the extended length (0, 2 or 8 bytes), then the key of
   the mask (4 bytes) if [masked]. [read] checks the length before it waits for
   the key, and consumes the header only when it is complete. Thus a cancel in
   the header consumes nothing. *)
let read ?on_data r ~masked ~max =
  Reader.ensure r 2 ;
  let b0 = Reader.get_uint8 r 0 and b1 = Reader.get_uint8 r 1 in
  if b0 land 0x70 <> 0 then protocol_error "reserved bits set" ;
  let fin = b0 land 0x80 <> 0 and opcode = opcode_of_int (b0 land 0x0f) in
  if is_control opcode && not fin then protocol_error "fragmented control frame" ;
  if is_control opcode && b1 land 0x7f > 125 then
    protocol_error "control frame longer than 125 bytes" ;
  if b1 land 0x80 <> 0 <> masked then
    protocol_error (if masked then "unmasked frame" else "masked frame") ;
  let ext, len =
    match b1 land 0x7f with
    | 126 ->
        Reader.ensure r 4 ;
        (2, Reader.get_uint16_be r 2)
    | 127 ->
        Reader.ensure r 10 ;
        let n = Reader.get_int64_be r 2 in
        if Int64.compare n 0L < 0 then
          protocol_error "length with its top bit set" ;
        (* We compare before the conversion, because [n] can be too large for an
           [int]. *)
        if Int64.compare n (Int64.of_int max) > 0 then raise (Error Too_big) ;
        (8, Int64.to_int n)
    | n ->
        (0, n)
  in
  if len > max && not (is_control opcode) then raise (Error Too_big) ;
  let key =
    if masked then (
      Reader.ensure r (2 + ext + 4) ;
      Reader.sub_string r (2 + ext)
        (4 [@mutate off "apply_mask uses only the first 4 bytes of the key"]) )
    else ""
  in
  Reader.consume r (2 + ext + String.length key) ;
  let on_data =
    match on_data with
    | Some f when not (is_control opcode) ->
        f opcode
    | _ ->
        fun _ _ _ -> ()
  in
  let payload = read_payload r ~key ~on_data len in
  {fin; opcode; payload= Bytes.unsafe_to_string payload}

let write ?mask w {fin; opcode; payload} =
  let len = String.length payload in
  if is_control opcode && ((not fin) || len > 125) then
    invalid_arg
      "Frame.write: a control frame must be final and at most 125 bytes" ;
  ( match mask with
  | Some key when String.length key <> 4 ->
      invalid_arg "Frame.write: the mask must be 4 bytes"
  | _ ->
      () ) ;
  Writer.write_uint8 w ((if fin then 0x80 else 0) lor int_of_opcode opcode) ;
  let m = if Option.is_some mask then 0x80 else 0 in
  if len < 126 then Writer.write_uint8 w (m lor len)
  else if len < 0x10000 then (
    Writer.write_uint8 w (m lor 126) ;
    Writer.write_uint16_be w len )
  else (
    Writer.write_uint8 w (m lor 127) ;
    Writer.write_int64_be w (Int64.of_int len) ) ;
  match mask with
  | None ->
      Writer.write_string w payload
  | Some key ->
      Writer.write_string w key ;
      let p = Bytes.of_string payload in
      apply_mask key p 0 len ;
      Writer.write_bytes w p 0 len
