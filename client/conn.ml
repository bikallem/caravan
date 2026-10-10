module Mtime = Affect_unix.Mtime

type t =
  { flow: Caravan.Flow.t
  ; host: string
  ; port: int
  ; id: int
  ; read_buf: bytes
  ; write_buf: bytes
        (* The buffers of the reader and the writer of each exchange. The pool
           gives the connection to one exchange at a time, so one exchange at a
           time uses them. *)
  ; mutable released_at: Mtime.t
        (* The time of the last release. Pool.release sets it before the
           connection goes idle, and only idle connections read it. *) }

let next_id = Atomic.make 0

(* The size of the read and the write buffer of a connection. *)
let buffer_size = 4096

let make ~host ~port flow =
  { flow
  ; host
  ; port
  ; id= Atomic.fetch_and_add next_id 1
  ; read_buf= Bytes.create buffer_size
  ; write_buf= Bytes.create buffer_size
  ; released_at= Mtime.min_stamp }
