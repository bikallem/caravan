module Cfg = Config
open Weft
open Caravan

(* One deadline, [connect_timeout] from the call, for the lookup and all the
   connects. Each address in the order of the lookup: localhost can give ::1
   first while a server listens only on 127.0.0.1.

   No late value exists. A deadline in the lookup detaches the job of the
   lookup. A deadline in Net.connect closes its socket. A connect that completes
   at the deadline keeps its flow: [with_timeout] never drops a value.
   [with_timeout] raises [Timeout] only for its own deadline: a cancel of the
   caller gives its [Cancelled]. *)
let connect (config : Cfg.t) ~host ~port =
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
        try_each "no address" (Resolver.resolve host) )
  with
  | flow ->
      flow
  | exception Fiber.Timeout ->
      Error.fail (Timeout `Connect)
