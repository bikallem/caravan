(** Connection information.

    What a handler knows about the connection of a request. *)

(** The type for connection information. *)
type t

val make :
     id:int
  -> peer:Weft.Net.addr
  -> local:Weft.Net.addr
  -> ?scheme:[`Http | `Https]
  -> ?alpn:string
  -> ?stopping:unit Weft.event
  -> unit
  -> t
(** [make ~id ~peer ~local ~scheme ~alpn ~stopping ()] tells about connection
    [id], from [peer] to our address [local]. [scheme] is [`Http] by default.
    [alpn] is the protocol that TLS selected, if one. [stopping] is {!stopping},
    [Weft.never] by default. *)

val with_tls : ?alpn:string -> t -> t
(** [with_tls ~alpn c] is [c] with the scheme [`Https] and the ALPN protocol
    [alpn]. A TLS engine gives it to the engine that it wraps. *)

val id : t -> int
(** [id c] is the number of [c] among the connections of the server. *)

val peer : t -> Weft.Net.addr
(** [peer c] is the address of the client. On macOS, an aborted connection can
    have [Tcp ("", 0)]. *)

val local : t -> Weft.Net.addr
(** [local c] is the address that accepted the connection. *)

val scheme : t -> [`Http | `Https]
(** [scheme c] is [`Https] iff TLS secures [c]. *)

val alpn : t -> string option
(** [alpn c] is the protocol that TLS ALPN selected, for example ["h2"]. *)

val stopping : t -> unit Weft.event
(** [stopping c] is ready when the server starts to stop. These events start the
    stop:
    - [Server.shutdown] or [Server.abort]
    - A failure of the listener
    - A cancel of the scope of the server
    - The first SIGINT or SIGTERM of [Server.stop_on_signals].

    It stays ready. A handler that runs for a long time can put it in a
    [Weft.choose] with its other events. Examples are a stream of events and a
    WebSocket handler. *)

val pp_addr : Format.formatter -> Weft.Net.addr -> unit
(** [pp_addr] prints an address as [host:port], [[ipv6]:port] or the path of a
    Unix socket. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints a connection as [#id peer -> local]. *)
