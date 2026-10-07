(* Internal. A connection to a host and a port. *)

val connect :
     ?getaddrinfo:
       (string -> string -> Unix.getaddrinfo_option list -> Unix.addr_info list)
  -> Config.t
  -> host:string
  -> port:int
  -> Caravan.Flow.t
(* [connect config ~host ~port] resolves [host] and connects to each of its
   addresses in turn, with one deadline ([config.connect_timeout]) for the
   lookup and all the connects. @raise Error.Error with Timeout `Connect at the
   deadline, or Connect with the error of the last address. *)
