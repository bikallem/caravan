(** Connection information.

    What a handler knows about the connection a request arrived on. *)

(** The type for connection information. A value contains a function, thus [=]
    and [compare] on it raise [Invalid_argument]. *)
type t

val make :
     id:int
  -> peer:Unix.sockaddr
  -> local:Unix.sockaddr
  -> ?scheme:[`Http | `Https]
  -> ?alpn:string
  -> ?stopping:unit Affect.Action.t
  -> unit
  -> t
(** [make ~id ~peer ~local ~scheme ~alpn ~stopping ()] describes connection
    [id], from [peer] to our address [local]. [scheme] defaults to [`Http].
    [alpn] is the protocol negotiated by TLS, if any. [stopping] is {!stopping},
    {!Affect.Action.never} by default. *)

val id : t -> int
(** [id c] is a number identifying [c] among the server's connections. *)

val peer : t -> Unix.sockaddr
(** [peer c] is the address of the client. *)

val local : t -> Unix.sockaddr
(** [local c] is the address the connection was accepted on. *)

val scheme : t -> [`Http | `Https]
(** [scheme c] is [`Https] iff [c] is secured by TLS. *)

val alpn : t -> string option
(** [alpn c] is the protocol negotiated with TLS ALPN, for example ["h2"]. *)

val stopping : t -> unit Affect.Action.t
(** [stopping c] is an action that is enabled when the server starts to stop:
    soon after {!Server.shutdown} or {!Server.abort}, the first signal of
    {!Server.run}, or a failure of the listener. "Soon" is one step of the
    server's supervisor, which sets the action of each connection. It stays
    enabled. A handler that runs for a long time, for example a stream of events
    or a WebSocket handler, can choose it with its other actions. Then the
    handler can end before the server cancels it at the end of [shutdown_grace].
*)

val pp_addr : Format.formatter -> Unix.sockaddr -> unit
(** [pp_addr] formats a socket address as [host:port], [[ipv6]:port] or a Unix
    domain socket path. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a connection as [#id peer -> local]. *)
