(** Bidirectional byte streams.

    A flow is what a protocol engine reads requests from and writes responses
    to. A TCP connection is a flow; later a TLS session or an HTTP/2 stream
    will be too. Upgrade callbacks and [CONNECT] tunnels receive the raw flow
    of their connection.

    Reads may run concurrently with writes, but two concurrent reads, or two
    concurrent writes, on the same flow are not supported. *)

open Affect

type t
(** The type for flows. *)

exception Timeout
(** Raised by a read or a write whose deadline has passed. *)

exception Closed
(** Raised by a read or a write on a closed flow, or when the peer reset the
    connection. *)

val make :
  read:(bytes -> int -> int -> int) ->
  read_now:(bytes -> int -> int -> int option) ->
  write:(bytes -> int -> int -> unit) ->
  readable:unit Action.t ->
  shutdown_send:(unit -> unit) ->
  close:(unit -> unit) ->
  ?reset:(unit -> unit) ->
  set_read_deadline:(Affect_unix.Mtime.t option -> unit) ->
  set_write_deadline:(Affect_unix.Mtime.t option -> unit) ->
  unit ->
  t
(** [make ~read ~read_now ~write ~readable ~shutdown_send ~close ~reset
    ~set_read_deadline ~set_write_deadline ()] is a flow implemented by the
    given functions:
    - [read buf off len] reads at most [len > 0] bytes into [buf] from [off]
      and returns how many it read, [0] at the end of the stream. It may block
      cooperatively.
    - [read_now buf off len] is like [read] but never blocks: it is [None]
      when [read] would.
    - [write buf off len] writes all [len] bytes of [buf] from [off], or
      raises.
    - [readable] is an action enabled when [read] can return without blocking,
      including at the end of the stream.
    - [shutdown_send ()] signals the end of the stream to the peer.
    - [close ()] releases the flow's resources. The resulting flow calls it at
      most once.
    - [reset ()] releases them abortively, so that the peer sees the stream
      fail rather than end (default: [close]). The resulting flow calls at
      most one of [close] and [reset], once.
    - [set_read_deadline d] and [set_write_deadline d] set or clear the time
      after which a blocked read or write raises {!Timeout}. *)

(** {1:reading Reading} *)

val read : t -> bytes -> int -> int -> int
(** [read flow buf off len] reads at most [len] bytes into [buf] starting at
    [off], and returns the number of bytes read. It returns [0] only at the end
    of the stream.

    @raise Invalid_argument if [len <= 0] or [off] and [len] do not designate a
    valid range of [buf].
    @raise Closed if [flow] is closed. *)

val read_now : t -> bytes -> int -> int -> int option
(** [read_now flow buf off len] is like {!read} but does not block: it is
    [None] if no byte is available yet. A flow can be {!readable'} and still
    have none: readiness can be stale, and under TLS the bytes that arrived
    may not complete a record.

    @raise Invalid_argument as {!read} does.
    @raise Closed if [flow] is closed. *)

val readable' : t -> 'tag -> 'tag Action.t
(** [readable' flow tag] is an action enabled with [tag] when a {!read} on
    [flow] would not block. Use it with {!Affect.Action.choose} to wait for
    input together with a timeout or a shutdown signal. *)

(** {1:writing Writing} *)

val write : t -> bytes -> int -> int -> unit
(** [write flow buf off len] writes the [len] bytes of [buf] starting at
    [off]. Writing zero bytes does nothing.

    @raise Invalid_argument if [off] and [len] do not designate a valid range of
    [buf].
    @raise Closed if [flow] is closed. *)

val write_string : t -> string -> unit
(** [write_string flow s] writes all of [s]. *)

val shutdown_send : t -> unit
(** [shutdown_send flow] tells the peer that nothing more will be written,
    while reads remain possible (a TCP half-close). *)

(** {1:lifetime Deadlines and closing} *)

val set_read_deadline : t -> Affect_unix.Mtime.t option -> unit
(** [set_read_deadline flow d] makes reads that are blocked at time [d]
    raise {!Timeout}. [None] removes the deadline. The deadline applies also
    to a read that waits at the time of the call, for example in a
    different fiber. *)

val set_write_deadline : t -> Affect_unix.Mtime.t option -> unit
(** [set_write_deadline flow d] is like {!set_read_deadline} for writes. *)

val close : t -> unit
(** [close flow] closes [flow]. Closing is idempotent. Reads and writes on a
    closed flow raise {!Closed}. *)

val reset : t -> unit
(** [reset flow] closes [flow] abortively: a TCP peer gets a reset instead
    of the end of the stream, so it can tell that what it received was cut
    short. It does nothing on a closed flow. *)

(** {1:derived Derived flows} *)

val prepend : string -> t -> t
(** [prepend s flow] is [flow] with the bytes of [s] read first. Its
    {!readable'} action is enabled at once while bytes of [s] remain. Writes,
    deadlines and closing go to [flow].

    An HTTP/1.1 connection uses it to hand the bytes it has already buffered
    to an upgrade callback. *)

val of_string : ?chunk:int -> string -> Buffer.t -> t
(** [of_string ~chunk input output] is an in-memory flow for tests. Reads
    return the bytes of [input], at most [chunk] bytes per read (default: no
    limit) to imitate a network that splits data, and then [0]. Writes append
    to [output]. It is always readable and ignores deadlines.

    @raise Invalid_argument if [chunk <= 0]. *)
