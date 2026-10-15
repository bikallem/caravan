(* The harness of the tests on the simulator. See sim_support.mli. *)

open Weft

exception Failed of string

let failf fmt = Printf.ksprintf (fun m -> raise (Failed m)) fmt

(* The text of a span in the largest unit that gives a whole number, for example
   "2 s" or "1 ns". *)
let span_text d =
  let ns = Span.to_ns d in
  let unit_of k = Int64.rem ns k = 0L in
  if ns = 0L then "0 s"
  else if unit_of 1_000_000_000L then
    Printf.sprintf "%Ld s" (Int64.div ns 1_000_000_000L)
  else if unit_of 1_000_000L then
    Printf.sprintf "%Ld ms" (Int64.div ns 1_000_000L)
  else if unit_of 1_000L then Printf.sprintf "%Ld us" (Int64.div ns 1_000L)
  else Printf.sprintf "%Ld ns" ns

(* weft registers no printer for the exceptions of the simulator. *)
let () =
  let items name l = Some (name ^ ": " ^ String.concat "; " l) in
  Printexc.register_printer (function
    | Failed m ->
        Some ("Sim_support.Failed: " ^ m)
    | Weft_sim.Deadlock l ->
        items "Weft_sim.Deadlock" l
    | Weft_sim.Livelock l ->
        items "Weft_sim.Livelock" l
    | Weft_sim.Lost_wakeup l ->
        items "Weft_sim.Lost_wakeup" l
    | Weft_sim.Unawaited l ->
        items "Weft_sim.Unawaited" l
    | Weft_sim.Model_error m ->
        Some ("Weft_sim.Model_error: " ^ m)
    | Weft_sim.Killed _ ->
        Some "Weft_sim.Killed"
    | _ ->
        None )

(* Exact time: the operations on the mono line *)

let ( +: ) t d =
  match Time.add Mono t d with
  | Some t' ->
      t'
  | None ->
      failf "%s after the time is out of the range of the mono line"
        (span_text d)

(* [is_at_or_after a b] is true if [a] is not before [b]. *)
let is_at_or_after a b = Span.compare (Time.diff Mono a b) Span.zero >= 0

(* [not_after fn ~limit what] raises [Failed] if the time now is after [limit].
   [what] names [limit] in the text. *)
let not_after fn ~limit what =
  let late = Time.diff Mono (Time.now Mono) limit in
  if Span.compare late Span.zero > 0 then
    failf "%s: the time now is %s after %s" fn (span_text late) what

(* Runs *)

let default_guard = Span.s 2

let guard = default_guard

(* The reason of the cancel of the guard. The body gets [Fiber.Cancelled
   Guard_expired]. *)
exception Guard_expired

let names = function [] -> "none" | l -> String.concat ", " l

(* The guard is a daemon fiber with a timer, not [Fiber.with_timeout]. Thus a
   [Fiber.Timeout] of the code under test is not a guard failure. When the timer
   ends, the daemon records the fibers that wait, then cancels the scope of the
   body. The result depends only on the virtual time at the end of the scope.
   The order of the body and the daemon at the deadline has no effect. *)
let guarded sim limit f =
  let deadline = Time.now Mono +: limit in
  let blocked = ref [] in
  let outcome =
    match
      Fiber.scope (fun s ->
          Fiber.fork_daemon ~name:"sim_support.guard" s (fun () ->
              sync (Time.at Mono deadline) ;
              blocked := Private.Sched_debug.blocked (Weft_sim.sched sim) ;
              Fiber.cancel s Guard_expired ) ;
          f sim )
    with
    | v ->
        Ok v
    | exception e ->
        Error (e, Printexc.get_raw_backtrace ())
  in
  if not (is_at_or_after (Time.now Mono) deadline) then
    match outcome with
    | Ok v ->
        v
    | Error (e, bt) ->
        Printexc.raise_with_backtrace e bt
  else
    let m =
      Printf.sprintf
        "the body did not end before the end of the guard of %s of virtual \
         time. The fibers that wait: %s."
        (span_text limit) (names !blocked)
    in
    match outcome with
    | Ok _ | Error (Fiber.Cancelled Guard_expired, _) ->
        raise (Failed m)
    | Error (e, _) ->
        failf "%s The body raised %s." m (Printexc.to_string e)

let run ?(seed = 0) ?(domains = 1) ?(guard = default_guard) f =
  Weft_sim.run ~seed ~domains (fun sim -> guarded sim guard f)

let race_with ~replay ?(seeds = 100) ?(domains = 4) ?guard f =
  let to_run =
    match replay with
    | None | Some "" ->
        List.init seeds (fun i -> i + 1)
    | Some s -> (
      match int_of_string_opt s with
      | Some seed ->
          [seed]
      | None ->
          failf "CARAVAN_SEED has the value %S, which is not an integer" s )
  in
  List.iter
    (fun seed ->
      match run ~seed ~domains ?guard f with
      | () ->
          ()
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          failf "seed %d, %d domains (CARAVAN_SEED=%d replays it): %s\n%s" seed
            domains seed (Printexc.to_string e)
            (Printexc.raw_backtrace_to_string bt) )
    to_run

let race ?seeds ?domains ?guard f =
  race_with ~replay:(Sys.getenv_opt "CARAVAN_SEED") ?seeds ?domains ?guard f

let manual ?(seed = 0) ?(domains = 1) f =
  Weft_sim.run ~seed ~domains ~auto:false f

(* Calls *)

(* [ended] gets a value when the function ends. *)
type 'a call = {promise: 'a Promise.t; ended: unit Ivar.t}

let call ?name s f =
  let ended = Ivar.create () in
  let body () = Fun.protect ~finally:(fun () -> Ivar.fill ended ()) f in
  {promise= Fiber.fork_promise ?name s body; ended}

let ended sim c =
  Weft_sim.settle sim ;
  Option.is_some (Ivar.peek c.ended)

let await c = Promise.await c.promise

let cancel c = Promise.cancel c.promise

(* Exact time: the steps *)

let advance_to sim at =
  not_after "advance_to" ~limit:at "the target" ;
  Weft_sim.advance_until sim at ;
  Weft_sim.settle sim

let advance sim d = advance_to sim (Time.now Mono +: d)

(* The three steps of [ends_at] and [holds_from]. [yes] and [no] tell the two
   states of [p] in the text of [Failed]. *)
let steps fn sim ~t0 d ~yes ~no p =
  if Span.compare d (Span.ns 1) < 0 then
    failf "%s: the span %s is less than 1 ns" fn (span_text d) ;
  let target = t0 +: d in
  let last = target +: Span.ns (-1) in
  not_after fn ~limit:last "t0 + d - 1 ns" ;
  Weft_sim.settle sim ;
  if p () then failf "%s: %s at the start, before t0 + d" fn yes ;
  advance_to sim last ;
  if p () then failf "%s: %s already at t0 + d - 1 ns" fn yes ;
  advance_to sim target ;
  if not (p ()) then failf "%s: %s at t0 + d" fn no

let ends_at sim ~t0 d c =
  steps "ends_at" sim ~t0 d ~yes:"the call ended" ~no:"the call did not end"
    (fun () -> ended sim c ) ;
  await c

let holds_from sim ~t0 d ~what p =
  steps "holds_from" sim ~t0 d ~yes:(what ^ " is true")
    ~no:(what ^ " is not true") p

(* Connected pairs and fds *)

let with_pair sim f =
  let client, server = Weft_sim.socket_pair sim in
  Fun.protect
    ~finally:(fun () -> Net.close client ; Net.close server)
    (fun () -> f client server)

let no_fd_leak sim f =
  let before = Weft_sim.open_fds sim in
  let v = f () in
  Weft_sim.settle sim ;
  let after = Weft_sim.open_fds sim in
  if after <> before then
    failf "no_fd_leak: %d fds open before the body, %d after" before after ;
  v
