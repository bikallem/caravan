(** The code that all examples share. *)

val url : scheme:string -> host:string -> Weft.Net.addr -> string
(** [url ~scheme ~host addr] is the URL that {!serve} prints: [scheme], [host]
    and the port of [addr]. For an address that is not TCP, it is the address
    only. *)

val serve :
     ?config:Caravan.Config.t
  -> ?services:Caravan.Server.service list
  -> ?engine:Caravan.Engine.t
  -> ?scheme:string
  -> ?host:string
  -> ?port:int
  -> Caravan.Handler.t
  -> unit
(** [serve handler] serves [handler] on [127.0.0.1:port] (port 8080 by default)
    with {!Caravan.Server.run} in the OS world of weft, until Ctrl-C. It
    replaces the address of [config], and prints the URL at the start, with
    [scheme] (["http"] by default) and [host] (["127.0.0.1"] by default) (see
    {!url}). [host] changes only the printed URL, not the address of the server:
    for example ["localhost"] for the examples that a browser opens on
    localhost. [engine] goes to {!Caravan.Server.run}: for example the TLS
    engine of [caravan.tls], with [~scheme:"https"]. *)
