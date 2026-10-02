(* Internal. Buffered writing, for HTTP/1.1 serialization. *)

type t
(** The type for buffered writers. A writer has a single producer. *)

val make : ?size:int -> (bytes -> int -> int -> unit) -> t
(** [make ~size write] writes its output with [write buf off len], which
    writes all [len] bytes or raises. [size] is the buffer size, 4 KiB by
    default. The buffer is allocated by the first write that needs it.

    @raise Invalid_argument if [size <= 0]. *)

val of_flow : ?size:int -> Flow.t -> t
(** [of_flow ~size flow] writes to [flow]. *)

val write_string : t -> string -> unit
(** [write_string w s] writes [s]. *)

val write_bytes : t -> bytes -> int -> int -> unit
(** [write_bytes w buf off len] writes [len] bytes of [buf] from [off]. Bytes
    that fit are copied into the buffer. Otherwise the buffer is flushed first,
    and a write of at least the buffer size then goes directly to the output,
    without a copy.

    @raise Invalid_argument if the range is not valid in [buf]. *)

val write_char : t -> char -> unit
(** [write_char w c] writes [c]. *)

val flush : t -> unit
(** [flush w] writes out the buffered bytes, if any. *)

val buffered : t -> int
(** [buffered w] is the number of bytes written to [w] but not yet flushed. *)

val capacity : t -> int
(** [capacity w] is the size of [w]'s buffer, [0] until it is allocated. *)

val written : t -> int
(** [written w] is the number of bytes written to [w] so far, flushed or not. *)
