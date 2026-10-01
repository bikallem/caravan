(* Lesson 3: cancellation.
   Run: dune exec examples/tour/l3_cancel.exe *)

open Affect
open Affect_unix

let log fmt = Printf.printf (fmt ^^ "\n%!")

(* Run [f] in a child call; cancel it if it takes longer than [span].
   This is the building block for request and idle timeouts. *)
let with_timeout span f =
  let task = Fun.Async.call f in
  match
    Action.invoke
      (Action.either (Fun.Async.get' task) (Mtime.wait_for' span ()))
  with
  | Left v ->
      Some v
  | Right () ->
      Fun.Async.cancel task ; None

let main () =
  Unix.main
  @@ fun () ->
  (* 1. [cancel] marks a call. Its next (or current) blocking action raises
     [Cancelled]. [Fun.Async.protect]'s [finally] still runs, and it may
     itself block (cancellation is masked there): close sockets here. *)
  log "1. cancel a blocked call" ;
  let conn =
    Fun.Async.call (fun () ->
        Fun.Async.protect ~finally:(fun () ->
            Mtime.wait_for Mtime.Span.(10 * ms) (* blocking is fine here *) ;
            log "  finally: connection closed" )
        @@ fun () ->
        log "  reading..." ;
        Mtime.wait_for Mtime.Span.(10 * s) ;
        log "  never printed" )
  in
  Mtime.wait_for Mtime.Span.(50 * ms) ;
  Fun.Async.cancel conn ;
  ( match Fun.Async.get conn with
  | () ->
      ()
  | exception Fun.Async.Cancelled ->
      log "  get raised Cancelled" ) ;
  (* 2. Cancellation is structured: cancelling a call cancels everything it
     called. Cancel the accept loop and every connection under it stops. *)
  log "2. cancellation reaches children" ;
  let server =
    Fun.Async.call (fun () ->
        for i = 1 to 3 do
          Fun.Async.call_trap_exn (fun () ->
              Fun.Async.protect ~finally:(fun () ->
                  log "  conn %d cleaned up" i )
              @@ fun () -> Mtime.wait_for Mtime.Span.(10 * s) )
        done ;
        Mtime.wait_for Mtime.Span.(10 * s) )
  in
  Mtime.wait_for Mtime.Span.(50 * ms) ;
  Fun.Async.cancel server ;
  (try Fun.Async.get server with Fun.Async.Cancelled -> ()) ;
  log "  server and all its connections returned" ;
  (* 3. Scheduling and cancellation are cooperative. Code that never blocks
     never notices it was cancelled, and can starve other functions sharing
     its domain (without the [yield] below, this program hangs: the timer
     that should cancel the loop never gets to run). Long CPU loops should
     periodically [yield] and [check_cancellation]. *)
  log "3. cooperative check" ;
  let cpu =
    Fun.Async.call (fun () ->
        let i = ref 0 in
        while true do
          incr i ;
          if !i mod 100_000 = 0 then (
            Fun.Async.yield () ;
            Fun.Async.check_cancellation () )
        done )
  in
  Mtime.wait_for Mtime.Span.(20 * ms) ;
  Fun.Async.cancel cpu ;
  (try Fun.Async.get cpu with Fun.Async.Cancelled -> log "  CPU loop stopped") ;
  (* 4. Timeouts = choose + cancel. *)
  log "4. with_timeout" ;
  let r1 =
    with_timeout
      Mtime.Span.(100 * ms)
      (fun () ->
        Mtime.wait_for Mtime.Span.(10 * ms) ;
        "fast" )
  in
  let r2 =
    with_timeout
      Mtime.Span.(100 * ms)
      (fun () ->
        Mtime.wait_for Mtime.Span.(1 * s) ;
        "slow" )
  in
  let show = function Some v -> v | None -> "<timeout>" in
  log "  fast: %s, slow: %s" (show r1) (show r2) ;
  0

let () = if !Sys.interactive then () else exit (main ())
