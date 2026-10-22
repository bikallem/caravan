(* Internal. A connection to a host and a port. *)

val connect : Config.t -> host:string -> port:int -> Caravan.Flow.t
(* [connect config ~host ~port] resolves [host] and connects to each of its
   addresses in turn. One deadline, [config.connect_timeout] from the call,
   covers the lookup and all the connects. A connect that completes at the
   deadline gives its flow. A cancel of the caller raises Weft.Fiber.Cancelled,
   and no socket stays open. @raise Error.Error with Timeout `Connect at the
   deadline, or Connect with the error of the last address. *)
