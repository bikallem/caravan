(* Lesson 2: actions, the things you block on.
   Run: dune exec examples/tour/l2_actions.exe *)

open Affect
open Affect_unix

let log fmt = Printf.printf (fmt ^^ "\n%!")

let main () =
  Unix.main
  @@ fun () ->
  (* 1. A port is a rendezvous: [offer] and [take] each block until the
     other side arrives. Handy for handing work to another function. *)
  log "1. ports" ;
  let p = Port.make () in
  let producer =
    Fun.Async.call (fun () ->
        List.iter (Port.offer p) [Some "GET /"; Some "GET /about"; None] )
  in
  let rec consume () =
    match Port.take p with
    | Some req ->
        log "  took %S" req ; consume ()
    | None ->
        log "  producer finished"
  in
  consume () ;
  Fun.Async.get producer ;
  (* 2. Every blocking operation has an action form (the primed name, or a
     [wait_*] function). Actions are values: [choose] invokes several and
     keeps only the first to synchronize. That is how you build timeouts. *)
  log "2. choose with a timeout" ;
  let slow =
    Fun.Async.call (fun () ->
        Mtime.wait_for Mtime.Span.(300 * ms) ;
        "slow result" )
  in
  let wait_result_or_timeout span =
    Action.invoke
      (Action.choose
         [ Action.map (fun v -> `Value v) (Fun.Async.get' slow)
         ; Mtime.wait_for' span `Timeout ] )
  in
  ( match wait_result_or_timeout Mtime.Span.(100 * ms) with
  | `Value v ->
      log "  got %S" v
  | `Timeout ->
      log "  timed out after 100ms (slow is still running)" ) ;
  ( match wait_result_or_timeout Mtime.Span.(1 * s) with
  | `Value v ->
      log "  got %S" v
  | `Timeout ->
      log "  timed out after 1s" ) ;
  (* 3. A semaphore caps how many functions do something at once, e.g. a
     maximum number of concurrent connections. Async functions may run in
     parallel on several domains, so shared counters must be [Atomic]. *)
  log "3. semaphore (at most 2 at a time)" ;
  let sem = Semaphore.make 2 in
  let active = Atomic.make 0 and peak = Atomic.make 0 in
  Fun.Async.get
  @@ Fun.Async.call (fun () ->
      for _ = 1 to 6 do
        Fun.Async.call_trap_exn (fun () ->
            Semaphore.with_acquired sem
            @@ fun () ->
            let n = 1 + Atomic.fetch_and_add active 1 in
            let rec bump () =
              let p = Atomic.get peak in
              if n > p && not (Atomic.compare_and_set peak p n) then bump ()
            in
            bump () ;
            Mtime.wait_for Mtime.Span.(50 * ms) ;
            Atomic.decr active )
      done ) ;
  log "  peak concurrency = %d" (Atomic.get peak) ;
  0

let () = if !Sys.interactive then () else exit (main ())
