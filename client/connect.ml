module Cfg = Config
open Affect
open Caravan
module Mtime = Affect_unix.Mtime

let at = function
  | None ->
      Action.never
  | Some d ->
      let now = Mtime.now () in
      if Mtime.is_earlier now ~than:d then
        Mtime.wait_for' (Mtime.span now d) `Timeout
      else Action.always `Timeout

(* [f] in a child call that stops at [deadline]. At the deadline, or at a cancel
   of the caller, the child stops, and [cleanup] gets a value that the child
   made in the race. *)
let bounded deadline f ~cleanup =
  let c = Fun.Async.call f in
  let stop () =
    Fun.Async.cancel c ;
    Fun.Async.mask_cancellation (fun () ->
        match Fun.Async.get c with v -> cleanup v | exception _ -> () )
  in
  match
    Action.invoke
      (Action.choose
         [Action.map (fun v -> `Done v) (Fun.Async.get' c); at deadline] )
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
  let deadline = Mtime.add_span (Mtime.now ()) config.connect_timeout in
  let addrs =
    bounded deadline (Resolver.resolve ?getaddrinfo ~host ~port) ~cleanup:ignore
  in
  let rec try_each last = function
    | [] ->
        Error.fail (Connect last)
    | addr :: rest -> (
      match
        bounded deadline (fun () -> Tcp.connect addr) ~cleanup:Flow.close
      with
      | flow ->
          flow
      | exception Unix.Unix_error (e, _, _) ->
          try_each (Unix.error_message e) rest )
  in
  try_each "no address" addrs
