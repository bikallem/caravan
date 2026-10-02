(** Message bodies.

    A body is a stream of bytes that can be consumed once. It has one of two
    shapes:
    - A {e source} is pulled by its consumer, chunk by chunk. Request bodies,
      and responses passed through by a proxy, are sources. A source is only
      read when its consumer can use the data, which gives backpressure.
    - A {e writer} body is a producer function that pushes its bytes into a
      {!Writer.t} sink provided by its consumer, typically the protocol engine
      writing a response. Handlers that generate output use it.

    Bodies are stateful: reading part of a body and then calling {!to_string}
    returns the rest. Only one call should consume a body at a time; {!close}
    may be called from any call. *)

(** Sinks that bodies are written into. *)
module Writer : sig
  type t
  (** The type for body sinks. *)

  val make : write:(bytes -> int -> int -> unit) -> flush:(unit -> unit) -> t
  (** [make ~write ~flush] is a sink that writes bytes with [write buf off len]
      and pushes them to the peer with [flush ()]. Protocol engines make
      these. *)

  val write : t -> bytes -> int -> int -> unit
  (** [write w buf off len] writes the [len] bytes of [buf] starting at [off].
      Writing zero bytes does nothing.

      @raise Invalid_argument if [off] and [len] do not designate a valid range
      of [buf]. *)

  val write_string : t -> string -> unit
  (** [write_string w s] writes all of [s]. *)

  val flush : t -> unit
  (** [flush w] pushes what was written so far to the peer. *)

  val set_trailers : t -> Headers.t -> unit
  (** [set_trailers w h] sets the trailer fields sent after the body. The last
      call wins. *)

  val trailers : t -> Headers.t
  (** [trailers w] are the trailer fields set with {!set_trailers}, empty if
      none. *)
end

type t
(** The type for bodies. *)

exception Too_large
(** Raised by {!to_string} when a body exceeds its [max]. *)

exception Truncated
(** Raised by reads when the peer ends a body before its declared end, for
    example when a connection closes in the middle of a request body. *)

(** {1:make Making bodies} *)

val empty : t
(** [empty] is the body with no bytes. *)

val of_string : string -> t
(** [of_string s] is the body with the bytes of [s] and length
    [String.length s]. *)

val of_source :
  ?length:int ->
  ?trailers:(unit -> Headers.t) ->
  ?close:(unit -> unit) ->
  (bytes -> int -> int -> int) ->
  t
(** [of_source ~length ~trailers ~close read] is the body whose bytes are
    pulled with [read buf off len], which returns the number of bytes it read,
    at most [len > 0], and [0] at the end of the body.
    - [length] is the number of bytes [read] produces in total, if known.
      Engines check it when they write the body out.
    - [trailers ()] are the trailer fields, called once when [read] returns
      [0]. Defaults to no trailers.
    - [close ()] is called at most once, by {!close}, to release the source
      when it is abandoned. Defaults to doing nothing.

    @raise Invalid_argument if [length] is negative. *)

val of_writer : ?length:int -> (Writer.t -> unit) -> t
(** [of_writer ~length produce] is the body that [produce w] writes into the
    sink [w] of its consumer. [length] is the number of bytes [produce] writes,
    if known. [produce] may call {!Writer.flush} to send what it wrote so far,
    for example between events of a stream.

    @raise Invalid_argument if [length] is negative. *)

(** {1:consume Consuming bodies} *)

val length : t -> int option
(** [length b] is the declared number of bytes of [b], if known. *)

val read : t -> bytes -> int -> int -> int
(** [read b buf off len] reads at most [len] bytes of [b] into [buf] starting
    at [off] and returns the number of bytes read, [0] at the end of the body.

    @raise Invalid_argument if [len <= 0], if [off] and [len] do not designate a
    valid range of [buf], if [b] is a writer body, which can only be written
    out with {!write_to}, or if [b] was closed. *)

val write_to : t -> Writer.t -> unit
(** [write_to b w] writes the rest of [b] into [w]. A source is copied with a
    {!Writer.flush} after each chunk it produces, so that streamed data and
    proxied responses reach the peer without delay. A writer body runs its
    producer on [w]. The body's trailers, if any, are set on [w].

    @raise Invalid_argument if [b] is a writer body that was already written
    out, or if [b] was closed. *)

val to_string : ?max:int -> t -> string
(** [to_string ~max b] is the rest of [b] as a string.

    @raise Too_large if [b] has more than [max] bytes left (default: no
    limit). *)

val trailers : t -> Headers.t
(** [trailers b] are the trailer fields of [b] once it has been consumed to
    its end, empty before that or if it has none. *)

val close : t -> unit
(** [close b] abandons [b]: the [close] function of a source is called, and a
    writer body's producer will not run. Closing is idempotent. *)
