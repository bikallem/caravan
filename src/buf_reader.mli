(* Internal. Buffered reading of lines and bytes, for HTTP/1.1 parsing. *)

type t
(** The type for buffered readers. A reader has a single consumer. *)

exception Eof
(** Raised by {!read_line} when the input ends before a line's CRLF. *)

exception Line_too_long
(** Raised by {!read_line} when a line is longer than allowed. *)

exception Bad_line_ending
(** Raised by {!read_line} on a bare CR or LF, which RFC 9112 §2.2 lets
    recipients reject. *)

val make : ?size:int -> (bytes -> int -> int -> int) -> t
(** [make ~size read] reads from [read buf off len], which returns the number
    of bytes it read, at most [len > 0], and [0] at the end of the input.
    [size] is the initial buffer size, 4 KiB by default. The buffer is
    allocated by the first read that needs it, and grows to hold long
    lines.

    @raise Invalid_argument if [size <= 0]. *)

val of_flow : ?size:int -> Flow.t -> t
(** [of_flow ~size flow] reads from [flow]. *)

val of_string : ?size:int -> ?chunk:int -> string -> t
(** [of_string ~size ~chunk s] reads [s], at most [chunk] bytes per read of the
    underlying input (default: no limit). For tests.

    @raise Invalid_argument if [chunk <= 0]. *)

val read_line : t -> max:int -> string
(** [read_line r ~max] is the next line of [r], without its CRLF, if it is at
    most [max] bytes long. It either returns and consumes the line and its
    CRLF, or raises and consumes nothing:
    - {!Bad_line_ending} if a LF is not preceded by CR, or a CR is not followed
      by LF.
    - {!Line_too_long} if no CRLF ends a line of at most [max] bytes.
    - {!Eof} if the input ends first.

    [read_line r ~max:0] consumes an empty line.

    @raise Invalid_argument if [max < 0]. *)

val read_into : t -> bytes -> int -> int -> int
(** [read_into r buf off len] reads at most [len] bytes into [buf] from [off]
    and returns how many it read, [0] at the end of the input. Buffered bytes
    come first; when there are none, it reads directly into [buf].

    @raise Invalid_argument if [len <= 0] or the range is not valid in [buf]. *)

val peek_eof : t -> bool
(** [peek_eof r] is [true] iff [r] has no buffered bytes and its input has
    ended. It reads from the input if nothing is buffered. *)

val buffered : t -> int
(** [buffered r] is the number of bytes read from the input but not yet
    consumed. *)

val take_buffered : t -> string
(** [take_buffered r] consumes and returns the buffered bytes. *)

val capacity : t -> int
(** [capacity r] is the size of [r]'s buffer, [0] until it is allocated. *)

val consumed : t -> int
(** [consumed r] is the number of bytes consumed from [r] so far. *)
