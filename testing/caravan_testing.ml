open Weft
open Caravan

type t =
  { sim: Weft_sim.t
  ; server: Server.t
  ; client: Caravan_client.t
  ; errors: (exn * Printexc.raw_backtrace) list Atomic.t
  ; seed: int
  ; domains: int
  ; manual: bool
  ; background: Fiber.scope }

exception Guard_expired of Span.t

exception Unchecked_errors of (exn * Printexc.raw_backtrace) list

exception
  Race_failed of
    {seed: int; domains: int; error: exn; backtrace: Printexc.raw_backtrace}

(* The text of a span in the largest unit that gives a whole number, for example
   "2 s" or "1 ns". *)
let span_text d =
  let ns = Span.to_ns d in
  let rec pick = function
    | (k, u) :: rest ->
        if Int64.rem ns k = 0L then Printf.sprintf "%Ld %s" (Int64.div ns k) u
        else pick rest
    | [] ->
        Printf.sprintf "%Ld ns" ns
  in
  if ns = 0L then "0 s"
  else pick [(1_000_000_000L, "s"); (1_000_000L, "ms"); (1_000L, "us")]

let () =
  Printexc.register_printer (function
    | Guard_expired d ->
        Some
          (Printf.sprintf
             "Caravan_testing.Guard_expired: the function did not end before \
              the guard of %s of virtual time"
             (span_text d) )
    | Race_failed {seed; domains; error; _} ->
        Some
          (Printf.sprintf
             "Caravan_testing.Race_failed: seed %d (%d domains), replay with \
              ~replay:%d: %s"
             seed domains seed (Printexc.to_string error) )
    (* weft registers no printer for the exceptions of its simulator. The format
       is that of the test harness of caravan. *)
    | Weft_sim.Deadlock l ->
        Some ("Weft_sim.Deadlock: " ^ String.concat "; " l)
    | Weft_sim.Livelock l ->
        Some ("Weft_sim.Livelock: " ^ String.concat "; " l)
    | Weft_sim.Lost_wakeup l ->
        Some ("Weft_sim.Lost_wakeup: " ^ String.concat "; " l)
    | Weft_sim.Unawaited l ->
        Some ("Weft_sim.Unawaited: " ^ String.concat "; " l)
    | Weft_sim.Model_error m ->
        Some ("Weft_sim.Model_error: " ^ m)
    | Unchecked_errors l ->
        Some
          ( "Caravan_testing.Unchecked_errors: "
          ^ String.concat "; " (List.map (fun (e, _) -> Printexc.to_string e) l)
          )
    | (_ [@mutate off "Printexc skips a printer that raises"]) ->
        None )

let host = "127.0.0.1"

let addr = Net.Tcp (host, 80)

(* The reason of the cancel of the guard. *)
exception Guard_cancel

(* The reason of the cancel of the background fibers at the end of [f]. *)
exception Background_end

(* [guarded guard f] runs [f] in a scope with a daemon that cancels the scope at
   the deadline. The virtual time at the end of [f] decides the result, not the
   exception: a [Fiber.Timeout] of [f] is not a guard failure, and the order of
   [f] and the daemon at the deadline has no effect. A run starts the guard at
   mono 0, thus [Time.add] cannot overflow. *)
let guarded guard f =
  let deadline = Option.get (Time.add Mono (Time.now Mono) guard) in
  let outcome =
    match
      Fiber.scope (fun s ->
          Fiber.fork_daemon ~name:"caravan_testing.guard" s (fun () ->
              sync (Time.at Mono deadline) ;
              Fiber.cancel s Guard_cancel ) ;
          f () )
    with
    | v ->
        Ok v
    | exception e ->
        Error (e, Printexc.get_raw_backtrace ())
  in
  if Span.compare (Time.diff Mono (Time.now Mono) deadline) Span.zero >= 0 then
    Error (Guard_expired guard, Printexc.get_callstack 0)
  else outcome

let push errors e =
  let rec loop () =
    let l = Atomic.get errors in
    if not (Atomic.compare_and_set errors l (e :: l)) then loop ()
  in
  loop ()

let reraise (e, bt) = Printexc.raise_with_backtrace e bt

let run ?(config = Config.default) ?engine ?(on_error = fun _ _ -> ())
    ?client_config ?(seed = 0) ?(domains = 1) ?(manual = false) ?guard handler f
    =
  let errors = Atomic.make [] in
  (* The first exception of [on_error]. *)
  let failed = Atomic.make None in
  let record e bt =
    push errors (e, bt) ;
    try on_error e bt
    with e' ->
      let bt' = Printexc.get_raw_backtrace () in
      ignore (Atomic.compare_and_set failed None (Some (e', bt'))) ;
      Printexc.raise_with_backtrace e' bt'
  in
  let config = {config with Config.addr; on_error= record} in
  (* A manual run has no guard unless the test gives one: a wait that nothing
     can end is a [Weft_sim.Deadlock]. *)
  let guard =
    match guard with None when not manual -> Some (Span.s 600) | g -> g
  in
  let in_guard f =
    match guard with
    | Some g ->
        guarded g f
    | None ->
        (* An exception of [f] goes to the catch around the client. *)
        Ok (f ())
  in
  (* In manual mode, the client has no grace: a wait for the grace is a wait for
     the time, thus a deadlock. Its daemon is in [sw]: the end of [sw] cancels
     it, and that closes the client (see [Caravan_client.make]). *)
  let with_client sw g =
    if manual then g (Caravan_client.make ?config:client_config sw)
    else Caravan_client.run ?config:client_config g
  in
  Weft_sim.run ~seed ~domains ~auto:(not manual) (fun sim ->
      Fiber.scope (fun sw ->
          let server = Server.start ?engine ~config sw handler in
          let outcome =
            match
              with_client sw (fun client ->
                  (* The background scope is outside the guard, thus the guard
                     measures only [f]. When [f] raises with no guard,
                     [Fiber.scope] cancels the fibers of the scope. *)
                  Fiber.scope (fun background ->
                      let r =
                        in_guard (fun () ->
                            f
                              { sim
                              ; server
                              ; client
                              ; errors
                              ; seed
                              ; domains
                              ; manual
                              ; background } )
                      in
                      Fiber.cancel background Background_end ;
                      r ) )
            with
            | r ->
                r
            (* An exception of [f] with no guard (a manual run), or a defect of
               the client. *)
            | exception e ->
                Error (e, Printexc.get_raw_backtrace ())
          in
          Server.abort server ;
          (* The wait awaits the promise of the server, also on the error paths:
             a failed promise that no fiber awaited writes a report to the
             standard error. After the wait, all connections ended, thus
             [failed] and [errors] are complete. *)
          let waited =
            match Server.wait server with
            | () ->
                Ok ()
            | ((exception e)
               [@mutate
                 off
                   "wait raises the exception in failed, or a failure of the \
                    listener, which no test can make"] ) ->
                Error (e, Printexc.get_raw_backtrace ())
          in
          match (Atomic.get failed, outcome, waited) with
          | Some e, _, _ ->
              reraise e
          | None, Error e, _ ->
              reraise e
          | ((None, Ok _, Error e)
             [@mutate off "only a failure of the listener reaches it"] ) ->
              reraise e
          | None, Ok v, Ok () -> (
            match List.rev (Atomic.exchange errors []) with
            | [] ->
                v
            | l ->
                raise (Unchecked_errors l) ) ) )

let race ?(seeds = 100) ?replay ?(domains = 4) ?config ?engine ?on_error
    ?client_config ?manual ?guard handler f =
  if replay = None && seeds < 1 then
    invalid_arg "Caravan_testing.race: seeds < 1" ;
  if domains < 1 then invalid_arg "Caravan_testing.race: domains < 1" ;
  let to_run =
    match replay with Some s -> [s] | None -> List.init seeds (fun i -> i + 1)
  in
  List.iter
    (fun seed ->
      match
        run ?config ?engine ?on_error ?client_config ~seed ~domains ?manual
          ?guard handler f
      with
      | () ->
          ()
      | exception error ->
          let backtrace = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Race_failed {seed; domains; error; backtrace})
            backtrace )
    to_run

let seed t = t.seed

let background t g =
  Fiber.fork_promise ~name:"caravan_testing.background" t.background g

let domains t = t.domains

let settle t = Weft_sim.settle t.sim

let advance_to t at =
  if not t.manual then
    invalid_arg "Caravan_testing.advance_to: the run is not in manual mode" ;
  if Span.compare (Time.diff Mono at (Time.now Mono)) Span.zero < 0 then
    invalid_arg "Caravan_testing.advance_to: a time before the time now" ;
  Weft_sim.advance_until t.sim at ;
  Weft_sim.settle t.sim

let advance t d =
  if not t.manual then
    invalid_arg "Caravan_testing.advance: the run is not in manual mode" ;
  if Span.compare d Span.zero < 0 then
    invalid_arg "Caravan_testing.advance: a negative span" ;
  match Time.add Mono (Time.now Mono) d with
  | Some at ->
      advance_to t at
  | None ->
      invalid_arg "Caravan_testing.advance: after the end of the mono line"

let client t = t.client

let server t = t.server

let sim t = t.sim

(* [authority] takes [t], so that a later version can serve on other
   addresses. *)
let authority _ = host

let take_errors t = List.rev (Atomic.exchange t.errors [])

let with_host req =
  match Request.authority req with
  | Some _ ->
      req
  | None ->
      Request.with_authority (Some host) req

let request t req = Caravan_client.request t.client (with_host req)

let with_response t req f =
  Caravan_client.with_response t.client (with_host req) f

type reply = {status: Status.t; headers: Headers.t; body: string}

let fetch t req =
  with_response t req (fun r ->
      let body = Body.to_string (Caravan_client.body r) in
      {status= Response.status r; headers= Response.headers r; body} )

let get ?(headers = Headers.empty) t path =
  fetch t (Request.make ~headers Method.GET path)
