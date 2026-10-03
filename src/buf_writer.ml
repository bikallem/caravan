type t = {
  write : bytes -> int -> int -> unit;
  size : int; (* of [buf] once allocated *)
  mutable buf : bytes; (* empty until the first buffered byte *)
  mutable len : int; (* buffered bytes, from 0 *)
}

let make ?(size = 4096) write =
  if size <= 0 then invalid_arg "Buf_writer.make: size must be positive";
  { write; size; buf = Bytes.empty; len = 0 }

let ensure_buffer w = if Bytes.length w.buf = 0 then w.buf <- Bytes.create w.size

let buffered w = w.len
let capacity w = Bytes.length w.buf

let flush w =
  if w.len > 0
  then begin
    let len = w.len in
    w.len <- 0;
    w.write w.buf 0 len
  end

let write_bytes w buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len
  then invalid_arg "Buf_writer.write_bytes: invalid range";
  let size = w.size in
  if len > size - w.len then flush w;
  if len >= size
  then w.write buf off len
  else begin
    ensure_buffer w;
    Bytes.blit buf off w.buf w.len len;
    w.len <- w.len + len
  end

let write_string w s = write_bytes w (Bytes.unsafe_of_string s) 0 (String.length s)

let write_char w c =
  if w.len = w.size then flush w;
  ensure_buffer w;
  Bytes.set w.buf w.len c;
  w.len <- w.len + 1
