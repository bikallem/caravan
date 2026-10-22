(* Caravan_run.run for the signal tests of test_linux. It prints its port, then
   lines that show what the handlers see, in CRLF lines.

   /slow prints "started". It gives "done" when the server starts to stop, or
   "timeout" after 2 s.

   /deaf prints "started", then "stopping" when the server starts to stop. Then
   it waits 10 s. When the server cancels it, it prints "cancelled". Then it
   waits 10 s in a shield, deaf to the cancel. The shield does not block its
   domain, so the other fibers of the domain continue to do I/O. *)

open Weft
open Caravan

let print line =
  print_string (line ^ "\r\n") ;
  flush stdout

let stopping req = Conn_info.stopping (Option.get (Request.conn req))

(* Prints "started", then "stopping" when the server starts to stop, then waits
   10 s. *)
let wait_for_stop req =
  print "started" ;
  sync (stopping req) ;
  print "stopping" ;
  Time.sleep Mono (Span.s 10)

let handler req =
  match Request.path_only req with
  | "/slow" ->
      print "started" ;
      Response.string
        (sync
           (choose
              [ wrap (stopping req) (fun () -> "done")
              ; wrap (Time.after Mono (Span.s 2)) (fun () -> "timeout") ] ) )
  | "/deaf" -> (
    match wait_for_stop req with
    | () ->
        Response.string "done"
    | exception Fiber.Cancelled _ ->
        print "cancelled" ;
        Fiber.shield (fun () -> Time.sleep Mono (Span.s 10)) ;
        Response.string "done" )
  | _ ->
      Response.string "hello"

let () =
  (* A parent that ignores SIGINT (a shell job with &, nohup) gives the child
     SIG_IGN, and weft Process.spawn resets only SIGPIPE. Then the third SIGINT
     of the tests gets SIG_IGN after the unwatch. *)
  Sys.set_signal Sys.sigint Sys.Signal_default ;
  let config =
    {Config.default with addr= Net.Tcp ("127.0.0.1", 0); date_header= false}
  in
  Caravan_run.run ~config ~domains:2 handler ~on_start:(fun server ->
      match Server.local_addr server with
      | Net.Tcp (_, port) ->
          print (string_of_int port)
      | _ ->
          assert false )
