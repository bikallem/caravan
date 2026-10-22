(** Bidirectional byte streams.

    A flow is what a protocol engine reads requests from and writes responses
    to. A TCP connection is a flow. Later a TLS session or an HTTP/2 stream will
    be one too. Upgrade callbacks and [CONNECT] tunnels get the raw flow of
    their connection.

    A read and a write can run at the same time in two fibers. Two reads at the
    same time, or two writes at the same time, on one flow are not supported. A
    function of this module needs a weft fiber only when a function that {!make}
    got needs one. The flows of {!Tcp} need a fiber. {!of_string} needs none. *)

(** The type for flows. *)
type t

(** Raised by a read or a write whose deadline or timeout passed. *)
exception Timeout

(** Raised by a read or a write on a closed flow, or when the peer reset the
    connection. *)
exception Closed

val make :
     read:(bytes -> int -> int -> int)
  -> read_now:(bytes -> int -> int -> int option)
  -> write:(bytes -> int -> int -> unit)
  -> readable:unit Weft.event
  -> shutdown_send:(unit -> unit)
  -> close:(unit -> unit)
  -> ?reset:(unit -> unit)
  -> set_read_deadline:(Weft.Clock.mono option -> unit)
  -> set_write_deadline:(Weft.Clock.mono option -> unit)
  -> ?set_read_timeout:(Weft.Span.t option -> unit)
  -> ?set_write_timeout:(Weft.Span.t option -> unit)
  -> unit
  -> t
(** [make ~read ~read_now ~write ~readable ~shutdown_send ~close ~reset
     ~set_read_deadline ~set_write_deadline ~set_read_timeout ~set_write_timeout
     ()] is a flow with these functions:
    - [read buf off len] reads at most [len > 0] bytes into [buf] from [off] and
      gives the count, [0] at the end of the stream. It can wait.
    - [read_now buf off len] is as [read], but it never waits: it is [None] when
      [read] would wait.
    - [write buf off len] writes all [len] bytes of [buf] from [off], or raises.
    - [readable] is an event that is ready when [read] can give a result with no
      wait. It is also ready at the end of the stream and after a close. It can
      be stale: use [read_now] after it.
    - [shutdown_send ()] sends the end of the stream to the peer.
    - [close ()] releases the resources of the flow. The flow calls it at most
      one time. After it, [readable] must be ready, and a read or a write that
      waits in another fiber must raise {!Closed}.
    - [reset ()] releases them with an abort, so that the peer sees a failure,
      not the end of the stream (default: [close]). The flow calls at most one
      of [close] and [reset], one time.
    - [set_read_deadline d] and [set_write_deadline d] set or remove the time
      after which a read or a write that waits raises {!Timeout}. They must not
      wait. They can do a world effect (a wake of another domain), so a caller
      must not hold a stdlib [Mutex].
    - [set_read_timeout] and [set_write_timeout] set the timeout of each later
      read or write that waits (see {!set_read_timeout}). They must not wait and
      must not do a world effect. By default the flow ignores the timeouts. *)

(** {1:reading Reading} *)

val read : t -> bytes -> int -> int -> int
(** [read flow buf off len] reads at most [len] bytes into [buf] from [off], and
    gives the count. It gives [0] only at the end of the stream.

    @raise Invalid_argument
      if [len <= 0], or if [off] and [len] are not a valid range of [buf].
    @raise Closed if [flow] is closed. *)

val read_now : t -> bytes -> int -> int -> int option
(** [read_now flow buf off len] is as {!read}, but it does not wait: it is
    [None] if no byte is ready. A flow can be {!readable} and have no byte:
    readiness can be stale, and under TLS the bytes can be a part of a record.

    @raise Invalid_argument as {!read} does.
    @raise Closed if [flow] is closed. *)

val readable : t -> unit Weft.event
(** [readable flow] is ready when a {!read} of [flow] would not wait. Use it in
    a [Weft.choose] with a deadline or a stop, then use {!read_now}. On [None],
    wait again. *)

(** {1:writing Writing} *)

val write : t -> bytes -> int -> int -> unit
(** [write flow buf off len] writes the [len] bytes of [buf] from [off]. A write
    of zero bytes does nothing.

    @raise Invalid_argument if [off] and [len] are not a valid range of [buf].
    @raise Closed if [flow] is closed. *)

val write_string : t -> string -> unit
(** [write_string flow s] writes all of [s]. *)

val shutdown_send : t -> unit
(** [shutdown_send flow] tells the peer that no more bytes come. Reads stay
    possible (a TCP half-close). *)

(** {1:lifetime Deadlines, timeouts and closing} *)

val set_read_deadline : t -> Weft.Clock.mono option -> unit
(** [set_read_deadline flow d] makes a read that waits at the time [d] raise
    {!Timeout}. [None] removes the deadline. The deadline applies also to a read
    that waits at the time of the call in another fiber. *)

val set_write_deadline : t -> Weft.Clock.mono option -> unit
(** [set_write_deadline flow d] is as {!set_read_deadline}, for writes. *)

val set_read_timeout : t -> Weft.Span.t option -> unit
(** [set_read_timeout flow span] gives each later {!read} that waits the
    deadline [span] after the start of its first wait. A second wait of the same
    read keeps that deadline. The read raises {!Timeout} at the earlier of this
    deadline and the read deadline ({!set_read_deadline}). [None] removes the
    timeout.

    A set does not change a read that waits at this time. Set it before the
    reads that need it. It does no world effect, so a fiber can call it before
    each read. *)

val set_write_timeout : t -> Weft.Span.t option -> unit
(** [set_write_timeout flow span] is as {!set_read_timeout}, for writes. A set
    does not change a write that waits at this time. Set it before the writes
    that need it. A write of many bytes is one call: its timeout starts at its
    first wait and holds until all the bytes went. *)

val close : t -> unit
(** [close flow] closes [flow]. A second close does nothing. Reads and writes on
    a closed flow raise {!Closed}. This also applies to a read or a write that
    waits in another fiber at the time of the call. It never waits, so a
    cancelled fiber can call it. *)

val reset : t -> unit
(** [reset flow] closes [flow] with an abort. A TCP peer gets a reset, not the
    end of the stream, so it can tell that it got only a part of the data. After
    {!shutdown_send}, the peer gets the end of the stream (Linux and the
    simulator). On a closed flow, it does nothing. *)

val linger : t -> max:int -> unit
(** [linger flow ~max] prepares [flow] for a {!close} when the peer can send
    more input. On Linux and in the simulator, a close of a socket with unread
    input sends a reset. The reset can delete our last bytes before the peer
    reads them. Thus [linger] calls {!shutdown_send}. Then it reads and discards
    the input until the end of the stream, for at most [max] bytes. RFC 9112
    section 9.6 gives this procedure for HTTP.

    [linger] stops at the read deadline of [flow]. Before the shutdown, it sets
    the read timeout of [flow] to [None], because each read of the linger is a
    new call. The flow ends after the linger, so no caller needs the old
    timeout. It ignores {!Timeout} and {!Closed}, but not
    [Weft.Fiber.Cancelled].

    Set the read deadline before the call, and close [flow] after it. No other
    fiber can read [flow] during the call. *)

(** {1:derived Derived flows} *)

val prepend : string -> t -> t
(** [prepend s flow] is [flow] with the bytes of [s] first. Its {!readable}
    event is ready at once while bytes of [s] remain. Writes, deadlines,
    timeouts and the close go to [flow].

    An HTTP/1.1 connection uses it to give the bytes that it has in its buffer
    to an upgrade callback. *)

val of_string : ?chunk:int -> string -> Buffer.t -> t
(** [of_string ~chunk input output] is a flow in memory, for tests. Reads give
    the bytes of [input], at most [chunk] bytes in each read (default: no
    limit), then [0]. Writes add to [output]. It is always readable, and it
    ignores the deadlines and the timeouts. It needs no fiber.

    @raise Invalid_argument if [chunk <= 0]. *)
