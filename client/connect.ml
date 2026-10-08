module Cfg = Config
open Affect
open Caravan

(* [f] in a child call that stops at [deadline]. At the deadline, or at a cancel
   of the caller, the child stops, and [cleanup] gets a value that the child
   made in the race. *)
let bounded time deadline f ~cleanup =
  let c = Fun.Async.call f in
  let stop () =
    Fun.Async.cancel c ;
    Fun.Async.mask_cancellation (fun () ->
        match Fun.Async.get c with v -> cleanup v | exception _ -> () )
  in
  match
    Action.invoke
      (Action.choose
         [ Action.map (fun v -> `Done v) (Fun.Async.get' c)
         ; Time.at time deadline `Timeout ] )
  with
  | `Done v ->
      v
  | `Timeout ->
      stop () ;
      Error.fail (Timeout `Connect)
  | exception (Fun.Async.Cancelled as e) ->
      let bt = Printexc.get_raw_backtrace () in
      stop () ;
      Printexc.raise_with_backtrace e bt

(* One deadline for the lookup and all the connects. Each address in the order
   of the lookup: localhost can give ::1 first while a server listens only on
   127.0.0.1. *)
let connect ?getaddrinfo (config : Cfg.t) ~host ~port =
  let time = config.mono_time in
  let deadline = Time.after time config.connect_timeout in
  let addrs =
    bounded time deadline
      (Resolver.resolve ?getaddrinfo ~host ~port)
      ~cleanup:ignore
  in
  let rec try_each last = function
    | [] ->
        Error.fail (Connect last)
    | addr :: rest -> (
      match
        bounded time deadline
          (fun () -> Tcp.connect ~time addr)
          ~cleanup:Flow.close
      with
      | flow ->
          flow
      | exception Unix.Unix_error (e, _, _) ->
          try_each (Unix.error_message e) rest )
  in
  try_each "no address" addrs
