type t = {
  write : bytes -> int -> int -> unit;
  buf : bytes;
  mutable len : int; (* buffered bytes, from 0 *)
  mutable written : int;
}

let make ?(size = 16384) write =
  if size <= 0 then invalid_arg "Buf_writer.make: size must be positive";
  { write; buf = Bytes.create size; len = 0; written = 0 }

let of_flow ?size flow = make ?size (Flow.write flow)
let buffered w = w.len
let written w = w.written

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
  let size = Bytes.length w.buf in
  if len > size - w.len then flush w;
  if len >= size
  then w.write buf off len
  else begin
    Bytes.blit buf off w.buf w.len len;
    w.len <- w.len + len
  end;
  w.written <- w.written + len

let write_string w s = write_bytes w (Bytes.unsafe_of_string s) 0 (String.length s)

let write_char w c =
  if w.len = Bytes.length w.buf then flush w;
  Bytes.set w.buf w.len c;
  w.len <- w.len + 1;
  w.written <- w.written + 1
