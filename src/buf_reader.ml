exception Eof
exception Line_too_long
exception Bad_line_ending

type t = {
  read : bytes -> int -> int -> int;
  read_now : bytes -> int -> int -> int option;
  size : int; (* of [buf] once allocated *)
  mutable buf : bytes; (* empty until the first fill *)
  mutable pos : int; (* first buffered byte *)
  mutable len : int; (* number of buffered bytes *)
  mutable eof : bool; (* [read] returned 0 *)
  mutable consumed : int;
}

let make ?(size = 4096) ?read_now read =
  if size <= 0 then invalid_arg "Buf_reader.make: size must be positive";
  let read_now =
    match read_now with Some f -> f | None -> fun buf off len -> Some (read buf off len)
  in
  { read; read_now; size; buf = Bytes.empty; pos = 0; len = 0; eof = false; consumed = 0 }

let of_flow ?size flow = make ?size ~read_now:(Flow.read_now flow) (Flow.read flow)

let of_string ?size ?(chunk = max_int) s =
  if chunk <= 0 then invalid_arg "Buf_reader.of_string: chunk must be positive";
  let off = ref 0 in
  make ?size (fun buf pos len ->
      let n = Int.min (Int.min len chunk) (String.length s - !off) in
      Bytes.blit_string s !off buf pos n;
      off := !off + n;
      n)

let buffered r = r.len
let capacity r = Bytes.length r.buf
let consumed r = r.consumed

let consume r n =
  r.pos <- r.pos + n;
  r.len <- r.len - n;
  r.consumed <- r.consumed + n;
  if r.len = 0 then r.pos <- 0

(* Reads more input after the buffered bytes, making room first: allocate the
   buffer, double a full one, or move the buffered bytes to the front. [false]
   at end of input. *)
let fill r =
  if r.eof
  then false
  else begin
    let cap = Bytes.length r.buf in
    if cap = 0
    then r.buf <- Bytes.create r.size
    else if r.len = cap
    then begin
      let buf = Bytes.create (2 * cap) in
      Bytes.blit r.buf r.pos buf 0 r.len;
      r.buf <- buf;
      r.pos <- 0
    end
    else if r.pos + r.len = cap
    then begin
      Bytes.blit r.buf r.pos r.buf 0 r.len;
      r.pos <- 0
    end;
    let start = r.pos + r.len in
    let n = r.read r.buf start (Bytes.length r.buf - start) in
    if n = 0
    then (r.eof <- true; false)
    else (r.len <- r.len + n; true)
  end

let read_line r ~max =
  if max < 0 then invalid_arg "Buf_reader.read_line: negative max";
  (* [i] buffered bytes are known to hold no CR or LF. *)
  let rec scan i =
    let limit = Int.min r.len (max + 1) in
    let rec find i =
      if i >= limit
      then i
      else match Bytes.get r.buf (r.pos + i) with
      | '\r' | '\n' -> i
      | _ -> find (i + 1)
    in
    let j = find i in
    if j < limit
    then begin
      if Bytes.get r.buf (r.pos + j) = '\n' then raise Bad_line_ending;
      if j + 1 < r.len
      then begin
        if Bytes.get r.buf (r.pos + j + 1) <> '\n' then raise Bad_line_ending;
        let line = Bytes.sub_string r.buf r.pos j in
        consume r (j + 2);
        line
      end
      else if fill r then scan j else raise Eof
    end
    else if r.len > max then raise Line_too_long
    else if fill r then scan j else raise Eof
  in
  scan 0

let read_into r buf off len =
  if off < 0 || len <= 0 || off > Bytes.length buf - len
  then invalid_arg "Buf_reader.read_into: invalid range";
  (* A small read fills the buffer first: then the next small reads, for
     example the parts of a frame header, do not read the input again. A
     large read goes directly into [buf], without a copy. *)
  if r.len = 0 && len < r.size then ignore (fill r);
  if r.len > 0
  then begin
    let n = Int.min len r.len in
    Bytes.blit r.buf r.pos buf off n;
    consume r n;
    n
  end
  else if r.eof
  then 0
  else begin
    let n = r.read buf off len in
    if n = 0 then r.eof <- true;
    r.consumed <- r.consumed + n;
    n
  end

let fill_now r =
  if r.len > 0
  then `Bytes
  else if r.eof
  then `Eof
  else begin
    if Bytes.length r.buf = 0 then r.buf <- Bytes.create r.size;
    r.pos <- 0;
    match r.read_now r.buf 0 (Bytes.length r.buf) with
    | None -> `Would_block
    | Some 0 -> r.eof <- true; `Eof
    | Some n -> r.len <- n; `Bytes
  end

let peek_eof r = r.len = 0 && not (fill r)

let take_buffered r =
  let s = Bytes.sub_string r.buf r.pos r.len in
  consume r r.len;
  s
