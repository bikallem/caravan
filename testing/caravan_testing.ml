open Weft
open Caravan

type t =
  { sim: Weft_sim.t
  ; server: Server.t
  ; client: Caravan_client.t
  ; errors: (exn * Printexc.raw_backtrace) list Atomic.t }

exception Guard_expired of Span.t

exception Unchecked_errors of (exn * Printexc.raw_backtrace) list

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
    ?client_config ?seed ?domains ?(guard = Span.s 600) handler f =
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
  Weft_sim.run ?seed ?domains (fun sim ->
      Fiber.scope (fun sw ->
          let server = Server.start ?engine ~config sw handler in
          let outcome =
            match
              Caravan_client.run ?config:client_config (fun client ->
                  guarded guard (fun () -> f {sim; server; client; errors}) )
            with
            | r ->
                r
            | ((exception e)
               [@mutate
                 off
                   "guarded catches the exceptions of f, thus only a defect of \
                    the client raises here"] ) ->
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
