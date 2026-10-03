(** WebSocket ({{:https://www.rfc-editor.org/rfc/rfc6455}RFC 6455}) for
    caravan servers. *)

(** {1:handshake Opening handshake} *)

val accept_key : string -> string
(** [accept_key key] is the [Sec-WebSocket-Accept] value that answers a
    request's [Sec-WebSocket-Key] [key]: the base64 SHA-1 digest of [key]
    followed by the protocol's GUID
    ({{:https://www.rfc-editor.org/rfc/rfc6455#section-4.2.2}RFC 6455
    §4.2.2}). *)

(** {1:connections Connections} *)

type t
(** The type for WebSocket connections. *)

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

(**/**)

(** Internal modules, exposed for tests. Not part of the public API. *)
module Private : sig
  module Sha1 = Sha1
  module Base64 = Base64
  module Frame = Frame

  val of_flow : ?max_message:int -> Caravan.Flow.t -> t
  (** [of_flow ~max_message flow] is a server connection on [flow]. The
      default of [max_message] is 8 MiB. *)
end
