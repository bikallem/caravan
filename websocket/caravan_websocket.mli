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
      this server. A request without [Origin] does not come from a browser,
      and [accept] permits it.
    - [max_message] is the size limit of a received message, 8 MiB by
      default.
    - [close_timeout] is the time limit of {!close}, 5 s by default.

    When [f] ends, [accept] closes the connection with 1000. If [f] raises
    {!Closed}, the connection is closed already. If [f] raises a different
    exception, [accept] closes the connection with 1011 and gives the
    exception to the server, which reports it. [accept] does not accept
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
    - 1006 if the connection stopped without a Close frame. *)

val recv : t -> message
(** [recv c] is the next message from the peer. If the peer sends a Ping,
    [recv] sends a Pong with the same data. It ignores a Pong.

    Only one fiber at a time can call [recv] on a connection.

    @raise Closed when the peer closes the connection, or when it does not
    obey the protocol. Then [recv] sends a Close frame, if possible, and
    closes the connection. *)

val send : t -> message -> unit
(** [send c m] sends [m] in one frame. Many fibers can send on a connection
    at the same time.

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
      raises {!Closed} with the code 1006.
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
    ?max_message:int -> ?close_timeout:Affect_unix.Mtime.Span.t -> Caravan.Flow.t -> t
    (** [of_flow ~max_message ~close_timeout flow] is a server connection on
        [flow]. The default of [max_message] is 8 MiB, and the default of
        [close_timeout] is 5 s. *)
end
