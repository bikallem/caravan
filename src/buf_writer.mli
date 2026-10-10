(* Internal. Buffered writing, for HTTP/1.1 serialization. *)

(** The type for buffered writers. A writer has a single producer. *)
type t

val make : ?size:int -> ?buffer:bytes -> (bytes -> int -> int -> unit) -> t
(** [make ~size ~buffer write] writes its output with [write buf off len], which
    writes all [len] bytes or raises. [size] is the buffer size, 4 KiB by
    default. The buffer is allocated by the first write that needs it.

    With [buffer], the writer uses these bytes as its buffer, and [size] is
    their length. The caller must not use them, or give them to a different
    writer, while this writer can use them.

    @raise Invalid_argument
      if [size <= 0], if [buffer] is empty, or if [size] and [buffer] are both
      given. *)

val write_string : t -> string -> unit
(** [write_string w s] writes [s]. *)

val write_bytes : t -> bytes -> int -> int -> unit
(** [write_bytes w buf off len] writes [len] bytes of [buf] from [off]:
    - A write that fits goes into the buffer.
    - A write that is smaller than the buffer but does not fit flushes the
      buffer first. Then it goes into the buffer.
    - A write of at least the buffer size first fills the buffer and flushes it,
      if the buffer holds bytes. Then the rest goes directly to the output,
      without a copy, if it is at least the buffer size. If not, the rest stays
      in the buffer. Thus a small head does not go out alone before a large
      body.

    Bytes that stay in the buffer go out at the next {!flush}.

    @raise Invalid_argument if the range is not valid in [buf]. *)

val write_char : t -> char -> unit
(** [write_char w c] writes [c]. *)

val flush : t -> unit
(** [flush w] writes out the buffered bytes, if any. *)

val buffered : t -> int
(** [buffered w] is the number of bytes written to [w] but not yet flushed. *)

val capacity : t -> int
(** [capacity w] is the size of [w]'s buffer, [0] until it is allocated. With a
    [buffer] given to {!make}, the buffer is there from the start. *)
