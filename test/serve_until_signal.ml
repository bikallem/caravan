(* Server.run for the signal tests. It prints its port, then lines that show
   what the handlers see, in CRLF lines.

   /slow prints "started". It gives "done" when the server starts to stop, or
   "timeout" after 2 s.

   /stuck prints "started", then "stopping" when the server starts to stop. Then
   it waits 10 s.

   /deaf does as /stuck, but when the server cancels it, it prints "cancelled"
   and blocks its domain for 10 s, deaf to the cancel. A handler cannot print
   while a different handler blocks its domain: then affect does no I/O.

   With the argument "after-run", the program prints "stopped" once run
   returned, and lives on for 10 s. *)

open Affect
open Caravan
module Mtime = Affect_unix.Mtime

let print line =
  print_string (line ^ "\r\n") ;
  flush stdout

(* Prints "started", then "stopping" when the server starts to stop, then waits
   10 s. *)
let wait_for_stop req =
  print "started" ;
  Action.invoke (Conn_info.stopping (Option.get (Request.conn req))) ;
  print "stopping" ;
  Mtime.wait_for Mtime.Span.(10 * s)

let handler req =
  match Request.path_only req with
  | "/slow" ->
      print "started" ;
      let stopping = Conn_info.stopping (Option.get (Request.conn req)) in
      Response.string
        (Action.invoke
           (Action.choose
              [ Action.map (fun () -> "done") stopping
              ; Mtime.wait_for' Mtime.Span.(2 * s) "timeout" ] ) )
  | "/stuck" ->
      wait_for_stop req ; Response.string "done"
  | "/deaf" -> (
    match wait_for_stop req with
    | () ->
        Response.string "done"
    | exception Fun.Async.Cancelled ->
        print "cancelled" ; Unix.sleepf 10. ; Response.string "done" )
  | _ ->
      Response.string "hello"

let () =
  let config =
    { Config.default with
      addr= Unix.ADDR_INET (Unix.inet_addr_loopback, 0)
    ; date_header= false }
  in
  Server.run ~config ~domain_count:2 handler ~on_start:(fun server ->
      match Server.local_addr server with
      | Unix.ADDR_INET (_, port) ->
          Printf.printf "%d\r\n%!" port
      | Unix.ADDR_UNIX _ ->
          assert false )

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "after-run" then begin
    print "stopped" ; Unix.sleepf 10.
  end
