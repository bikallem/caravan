module Cfg = Config
open Weft
open Caravan

type tls = host:string -> Flow.t -> (Flow.t, string) result

(* [secure tls ~host tcp] is the TLS flow of the handshake [tls] on [tcp]. Each
   failure closes [tcp], also a cancel and the deadline. A failed handshake does
   not try the next address: its certificate is the same. *)
let secure tls ~host tcp =
  match tls ~host tcp with
  | Ok flow ->
      flow
  | Error m ->
      Flow.close tcp ; Error.fail (Tls m)
  | exception Net.Error (e, _) ->
      Flow.close tcp ;
      Error.fail (Connect (Error.net_text e))
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Flow.close tcp ;
      Printexc.raise_with_backtrace e bt

(* One deadline, [connect_timeout] from the call, for the lookup, all the
   connects and the TLS handshake. Each address in the order of the lookup:
   localhost can give ::1 first while a server listens only on 127.0.0.1.

   No late value exists. A deadline in the lookup detaches the job of the
   lookup. A deadline in Net.connect closes its socket. A connect that completes
   at the deadline keeps its flow: [with_timeout] never drops a value.
   [with_timeout] raises [Timeout] only for its own deadline: a cancel of the
   caller gives its [Cancelled]. *)
let connect ?tls (config : Cfg.t) ~host ~port =
  match
    Fiber.with_timeout Mono config.connect_timeout (fun () ->
        let rec try_each last = function
          | [] ->
              Error.fail (Connect last)
          | a :: rest -> (
            match Tcp.connect (Net.Tcp (a, port)) with
            | flow ->
                flow
            | exception Net.Error (e, _) ->
                try_each (Error.net_text e) rest )
        in
        let tcp = try_each "no address" (Resolver.resolve host) in
        match tls with None -> tcp | Some tls -> secure tls ~host tcp )
  with
  | flow ->
      flow
  | exception Fiber.Timeout ->
      Error.fail (Timeout `Connect)
