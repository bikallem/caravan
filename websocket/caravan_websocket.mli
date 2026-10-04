(** WebSocket ({{:https://www.rfc-editor.org/rfc/rfc6455}RFC 6455}) for
    caravan servers.

    A handler gives the response of {!accept} to a WebSocket request:
    {[
      let handler req =
        match Caravan.Request.path_only req with
        | "/echo" ->
            Caravan_websocket.accept req (fun c ->
                let rec loop () =
                  Caravan_websocket.(send c (recv c));
                  loop ()
                in
                loop ())
        | _ -> Caravan.Handler.not_found req
    ]} *)

type t
(** The type for WebSocket connections. *)

(** {1:handshake Opening handshake} *)

val accept :
  ?protocols:string list ->
  ?origin:(string -> bool) ->
  ?max_message:int ->
  ?close_timeout:Affect_unix.Mtime.Span.t ->
  ?write_timeout:Affect_unix.Mtime.Span.t ->
  ?read_timeout:Affect_unix.Mtime.Span.t ->
  ?idle_timeout:Affect_unix.Mtime.Span.t ->
  Caravan.Request.t ->
  (t -> unit) ->
  Caravan.Response.t
(** [accept req f] is the response to the WebSocket request [req]
    ({{:https://www.rfc-editor.org/rfc/rfc6455#section-4.2}RFC 6455
    §4.2}). If [req] is a correct request, the response is a [101] that
    gives the connection to [f]. If not, the response is an error:
    - [426] with [Sec-WebSocket-Version: 13] if [req] asks for a different
      version of the protocol.
    - [403] if the origin of [req] is not permitted.
    - [400] for other problems: a method that is not [GET], a version that
      is not HTTP/1.1, no [Upgrade: websocket] or [Connection: upgrade]
      token, no [Sec-WebSocket-Key] or a key that is not 16 bytes.

    The options are:
    - [protocols] are the subprotocols of the server, in order of
      preference. [accept] chooses the first one that [req] offers in
      [Sec-WebSocket-Protocol], see {!protocol}. The default is none.
    - [origin o] is [true] if the [Origin] [o] of a request is permitted.
      By default, the origin must have the host and the port of [req]: a
      page from a different site cannot use the cookies of the user on
      this server. A missing port is the default port of the scheme of the
      origin (80 or 443). If the scheme of [req] is [https], the origin
      must also be [https]. A request without [Origin] does not come from
      a browser, and [accept] permits it.
    - [max_message] is the size limit of a received message, 8 MiB by
      default.
    - [close_timeout] is the time limit of {!close}, 5 s by default.
    - [write_timeout] is the time limit to send one frame, 30 s by default.
      If a frame does not go in this time, the connection closes with
      1006.
    - [read_timeout] is the time limit of each read inside a frame, 30 s
      by default. Each read that gets bytes starts a new limit, thus a
      slow frame with no long pause can take more time. If no byte comes
      in this time, the connection sends a Close frame with 1001 and
      closes.
    - [idle_timeout] is the time limit between two frames: from the time
      that {!recv} waits for a frame until its first byte. If no byte
      comes in this time, the connection sends a Close frame with 1001
      and closes at once, without a wait for the reply of the peer. By
      default, there is no limit. Use {!ping} to keep a connection open
      through proxies that close idle connections.

    When [f] ends, [accept] closes the connection with 1000. If [f] raises
    {!Closed} for its connection, the connection is closed already. If [f]
    raises a different exception, [accept] closes the connection with 1011
    and gives the exception to the server, which reports it. A {!Closed} of
    a different connection, for example a different client, is such an
    exception. A [Cancelled] from a call
    that [f] made goes to the server as a [Failure]. If the server cancels
    [f], [accept] sends no Close frame. [accept] does not accept
    extensions. *)

val protocol : t -> string option
(** [protocol c] is the subprotocol that {!accept} chose for [c], if one. *)

val accept_key : string -> string
(** [accept_key key] is the [Sec-WebSocket-Accept] value that answers a
    request's [Sec-WebSocket-Key] [key]: the base64 SHA-1 digest of [key]
    followed by the protocol's GUID
    ({{:https://www.rfc-editor.org/rfc/rfc6455#section-4.2.2}RFC 6455
    §4.2.2}). *)

(** {1:connections Connections} *)

type message =
| Text of string  (** A text message. The text is valid UTF-8. *)
| Binary of string  (** A binary message. *)
(** The type for messages. *)

exception Closed of { code : int; reason : string }
(** The connection is closed. [code] is the status code of the closing
    handshake
    ({{:https://www.rfc-editor.org/rfc/rfc6455#section-7.4}RFC 6455 §7.4}),
    and [reason] is its text:
    - The code and the reason from the peer's Close frame. The code is 1005
      if the frame has no code.
    - The code that we sent because the peer did not obey the protocol: 1002
      for a protocol error, 1007 for text that is not UTF-8, 1009 for a
      message that is too large. The reason tells the problem.
    - 1001 with the reason ["read timeout"] or ["idle timeout"] if no byte
      came in the [read_timeout] or the [idle_timeout] of {!accept}.
    - 1006 if the connection stopped without a Close frame. *)

val recv : t -> message
(** [recv c] is the next message from the peer. If the peer sends a Ping,
    [recv] sends a Pong with the same data. It ignores a Pong.

    Many fibers can call [recv] on a connection at the same time. They get
    the messages one at a time: each message goes to one fiber.

    If a cancel stops [recv] between two messages, the connection stays
    open. If the cancel stops it in the middle of a frame or a fragmented
    message, the connection closes with the code 1006. If the cancel comes
    after a Ping and before its Pong goes, the peer gets no Pong.

    @raise Closed when the peer closes the connection, or when it does not
    obey the protocol. In the second case, [recv] sends a Close frame, if
    possible. Then it discards the input until the peer closes, for at
    most the close timeout and 1 MiB, and closes the connection. *)

val send : t -> message -> unit
(** [send c m] sends [m] in one frame. Many fibers can send on a connection
    at the same time. If a cancel stops [send] in the middle of the frame,
    the connection closes with the code 1006.

    @raise Invalid_argument if [m] is a [Text] that is not valid UTF-8.
    @raise Closed if the connection is closed. *)

val ping : t -> string -> unit
(** [ping c data] sends a Ping with [data]. The peer sends a Pong with the
    same data, and {!recv} ignores the Pong.

    @raise Invalid_argument if [data] has more than 125 bytes.
    @raise Closed if the connection is closed. *)

val close : ?code:int -> ?reason:string -> t -> unit
(** [close ~code ~reason c] closes [c] with the closing handshake
    ({{:https://www.rfc-editor.org/rfc/rfc6455#section-7}RFC 6455 §7}). It
    sends a Close frame with [code] (default 1000) and [reason] (default
    empty). Then it waits for the Close frame of the peer, for at most the
    close timeout of [c]. Then the connection closes:
    - After [close], {!send} and {!ping} raise {!Closed}.
    - Until the Close frame of the peer comes, the connection discards the
      messages from the peer and sends no Pong.
    - If a fiber is in {!recv}, that fiber reads the Close frame of the peer
      and raises {!Closed} with its code. [close] waits for this.
    - At the time limit, the reads and writes on [c] stop, and {!recv}
      raises {!Closed} with the code 1006. A read or a write with an
      earlier deadline, from [write_timeout], [read_timeout] or
      [idle_timeout], stops at that deadline.
    - If we sent a Close frame on [c] before, [close] does nothing.

    @raise Invalid_argument if [code] is not a code that a Close frame can
    contain, or if [reason] is not valid UTF-8 or has more than 123 bytes. *)

(**/**)

(** Internal modules, exposed for tests. Not part of the public API. *)
module Private : sig
  module Sha1 = Sha1
  module Base64 = Base64
  module Frame = Frame

  val of_flow :
    ?max_message:int ->
    ?close_timeout:Affect_unix.Mtime.Span.t ->
    ?write_timeout:Affect_unix.Mtime.Span.t ->
    ?read_timeout:Affect_unix.Mtime.Span.t ->
    ?idle_timeout:Affect_unix.Mtime.Span.t ->
    Caravan.Flow.t ->
    t
    (** [of_flow flow] is a server connection on [flow], with the options
        of {!accept}. *)
end
