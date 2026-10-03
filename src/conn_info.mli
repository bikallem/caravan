(** Connection information.

    What a handler knows about the connection a request arrived on. *)

type t
(** The type for connection information. *)

val make :
  id:int ->
  peer:Unix.sockaddr ->
  local:Unix.sockaddr ->
  ?scheme:[ `Http | `Https ] ->
  ?alpn:string ->
  unit ->
  t
(** [make ~id ~peer ~local ~scheme ~alpn ()] describes connection [id], from
    [peer] to our address [local]. [scheme] defaults to [`Http]. [alpn] is the
    protocol negotiated by TLS, if any. *)

val id : t -> int
(** [id c] is a number identifying [c] among the server's connections. *)

val peer : t -> Unix.sockaddr
(** [peer c] is the address of the client. *)

val local : t -> Unix.sockaddr
(** [local c] is the address the connection was accepted on. *)

val scheme : t -> [ `Http | `Https ]
(** [scheme c] is [`Https] iff [c] is secured by TLS. *)

val alpn : t -> string option
(** [alpn c] is the protocol negotiated with TLS ALPN, for example ["h2"]. *)

val pp_addr : Format.formatter -> Unix.sockaddr -> unit
(** [pp_addr] formats a socket address as [host:port], [\[ipv6\]:port] or a
    Unix domain socket path. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a connection as [#id peer -> local]. *)
