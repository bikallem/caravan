(* Lesson 4: a TCP echo server, the skeleton of an HTTP server.
   Run:  dune exec examples/tour/l4_echo_server.exe
   Try:  nc 127.0.0.1 8080   (type lines; idle 10s to be disconnected)
   Stop: Ctrl-C (in-flight connections are cancelled and cleaned up) *)

open Affect
open Affect_unix

let log fmt = Printf.printf (fmt ^^ "\n%!")

let port = 8080

let max_connections = 100

let idle_timeout = Mtime.Span.(10 * s)

let pp_addr = function
  | Unix.ADDR_INET (a, p) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr a) p
  | Unix.ADDR_UNIX path ->
      path

(* [Unix.write] can return early on a non-blocking socket; loop until all
   bytes are out. *)
let rec write_all fd buf off len =
  if len > 0 then
    let n = Unix.single_write fd buf off len in
    write_all fd buf (off + n) (len - n)

(* Block until [fd] has data or [idle_timeout] passes, whichever is first. *)
let wait_input fd =
  Action.invoke
    (Action.choose
       [Unix.wait_readable fd `Readable; Mtime.wait_for' idle_timeout `Idle] )

let handle_connection fd peer =
  let buf = Bytes.create 4096 in
  let rec loop () =
    match wait_input fd with
    | `Idle ->
        log "%s: idle timeout" peer
    | `Readable -> (
      match Unix.read fd buf 0 (Bytes.length buf) with
      | 0 ->
          log "%s: disconnected" peer
      | n ->
          write_all fd buf 0 n ; loop () )
  in
  try loop ()
  with Unix.Unix_error (e, _, _) -> log "%s: %s" peer (Unix.error_message e)

let serve () =
  let sock =
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
    (* non-blocking *)
  in
  Fun.Async.protect ~finally:(fun () -> Unix.close_noerr sock)
  @@ fun () ->
  Unix.setsockopt sock Unix.SO_REUSEADDR true ;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) ;
  Unix.listen sock 128 ;
  log "listening on 127.0.0.1:%d" port ;
  let slots = Semaphore.make max_connections in
  let open_conns = Atomic.make 0 in
  let rec accept_loop () =
    Semaphore.acquire slots (* stop accepting while at capacity *) ;
    let fd, addr = Unix.accept sock in
    (* Accepted sockets do NOT inherit non-blocking mode. Without this,
       [Unix.read] would block a whole domain instead of one function. *)
    Unix.set_nonblock fd ;
    let peer = pp_addr addr in
    log "%s: connected (%d open)" peer (1 + Atomic.fetch_and_add open_conns 1) ;
    (* One async call per connection. It is a child of [serve], so
       cancelling [serve] cancels it, and [serve] waits for it to finish. *)
    Fun.Async.call_trap_exn (fun () ->
        Fun.Async.protect ~finally:(fun () ->
            Unix.close_noerr fd ;
            Atomic.decr open_conns ;
            Semaphore.release slots )
        @@ fun () -> handle_connection fd peer ) ;
    accept_loop ()
  in
  accept_loop ()

(* Ctrl-C handling.

   OCaml 5 runs signal handlers only when a domain is executing OCaml code,
   and the OS delivers a signal to whichever thread it likes. If SIGINT lands
   on a domain that is asleep while another sits in [select], the handler may
   never run and Ctrl-C is lost (affect's [Unix.Signal.wait'] showed exactly
   this with several domains). So we block SIGINT in every thread, receive it
   synchronously in one dedicated system thread with [sigwait], and turn it
   into a byte on a pipe, which affect can wait on like any socket. *)
let sigint_fd () =
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint]) (* inherited *) ;
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock r ;
  let wait () =
    ignore (Thread.wait_signal [Sys.sigint]) ;
    ignore (Unix.write_substring w "x" 0 1)
  in
  ignore (Thread.create wait ()) ;
  r

let main () =
  (* Before [Unix.main], so worker domains inherit the blocked mask. *)
  let sigint = sigint_fd () in
  Unix.main
  @@ fun () ->
  let server = Fun.Async.call serve in
  (* Wait for Ctrl-C, or for the server to fail (e.g. port in use). *)
  match
    Action.invoke
      (Action.either (Unix.wait_readable sigint ()) (Fun.Async.get' server))
  with
  | Right () ->
      0
  | Left () -> (
      log "\nshutting down" ;
      Fun.Async.cancel server ;
      match Fun.Async.get server with
      | () ->
          0
      | exception Fun.Async.Cancelled ->
          log "all connections closed" ;
          0 )

let () = if !Sys.interactive then () else exit (main ())
