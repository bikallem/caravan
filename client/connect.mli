(* Internal. A connection to a host and a port. *)

type tls = host:string -> Caravan.Flow.t -> (Caravan.Flow.t, string) result
(* The TLS handshake of the client (Caravan_client.tls). *)

val connect : ?tls:tls -> Config.t -> host:string -> port:int -> Caravan.Flow.t
(* [connect ?tls config ~host ~port] resolves [host] and connects to each of its
   addresses in turn. With [tls], it then does the TLS handshake [tls ~host] on
   the first TCP flow. One deadline, [config.connect_timeout] from the call,
   covers the lookup, all the connects and the handshake. A connect that
   completes at the deadline gives its flow. A cancel of the caller raises
   Weft.Fiber.Cancelled, and no socket stays open. @raise Error.Error with
   Timeout `Connect at the deadline, Connect with the error of the last address
   or with a Net.Error of [tls], or Tls with the [Error] of [tls]. An other
   exception of [tls] goes to the caller. *)
