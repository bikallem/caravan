(* Lesson 1: asynchronous functions and structured concurrency.
   Run: dune exec examples/tour/l1_async.exe *)

open Affect
open Affect_unix

let log fmt = Printf.printf (fmt ^^ "\n%!")

(* Simulates work that blocks cooperatively (like waiting on a socket). *)
let work name n result =
  log "  %s: start" name ;
  Mtime.wait_for Mtime.Span.(n * ms) ;
  log "  %s: done" name ;
  result

let main () =
  Unix.main
  @@ fun () ->
  (* 1. [call] returns a handle immediately; [get] blocks until the result
     is ready. The two calls run concurrently, so this takes ~200ms, not
     ~300ms. *)
  log "1. call/get" ;
  let a = Fun.Async.call (fun () -> work "a" 200 1) in
  let b = Fun.Async.call (fun () -> work "b" 100 2) in
  log "  a + b = %d" (Fun.Async.get a + Fun.Async.get b) ;
  (* 2. Exceptions raised by an async function are re-raised by [get]. *)
  log "2. exceptions" ;
  let c = Fun.Async.call (fun () -> failwith "boom") in
  ( match Fun.Async.get c with
  | () ->
      ()
  | exception Failure msg ->
      log "  get raised Failure %S" msg ) ;
  (* 3. Structured concurrency: an *async* function does not return until
     its async children have. A plain OCaml function call is not a scope,
     so wrap a body in [get (call ...)] to get one. *)
  log "3. scopes" ;
  let plain () =
    Fun.Async.call_trap_exn (fun () -> work "child-of-plain" 100 ())
  in
  plain () ;
  log "  plain function returned (its child is still running)" ;
  Fun.Async.get
  @@ Fun.Async.call (fun () ->
      Fun.Async.call_trap_exn (fun () -> work "child-of-scope" 100 ()) ) ;
  log "  scoped call returned (its child had finished)" ;
  (* The root ([main]) also waits for "child-of-plain" before returning. *)
  0

let () = if !Sys.interactive then () else exit (main ())
