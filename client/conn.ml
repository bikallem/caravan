type t =
  { flow: Caravan.Flow.t
  ; host: string
  ; port: int
  ; id: int
  ; read_buf: bytes
  ; write_buf: bytes
        (* The buffers of the reader and the writer of each exchange. The pool
           gives the connection to one exchange at a time, so one exchange at a
           time uses them. An exchange does not touch its reader or writer after
           it gives the connection back, and a request body that ends after the
           response keeps the connection until its writer ends
           (Writers.when_done). The writer of a body producer stops when the
           producer returns. A write that a different fiber of the producer
           started before the return can still end after it (see
           Body.of_writer): such a producer can write into a later exchange. *)
  ; mutable released_at: Weft.Clock.mono option
        (* The time of the last release. Pool.release sets it before the
           connection goes idle, and only idle connections read it. [None] until
           the first release: user code cannot make the smallest
           [Clock.mono]. *) }

(* The size of the read and the write buffer of a connection. *)
let buffer_size = 4096

(* [make ~id ~host ~port flow] is a connection. The pool gives [id], unique in
   the pool: a counter of each pool, thus a run does not depend on the runs
   before it. *)
let make ~id ~host ~port flow =
  { flow
  ; host
  ; port
  ; id
  ; read_buf= Bytes.create buffer_size
  ; write_buf= Bytes.create buffer_size
  ; released_at= None }
