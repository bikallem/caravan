(* The gateway on 127.0.0.1:8080. UPSTREAM (default 127.0.0.1:8081) is the
   upstream server. The program makes the client and the server in one scope,
   and stops at Ctrl-C (SIGINT) or SIGTERM. It writes each error of the client
   to stderr. *)

open Weft
open Caravan

let () =
  let upstream =
    Option.value (Sys.getenv_opt "UPSTREAM") ~default:"127.0.0.1:8081"
  in
  Weft.run ~signals:[SIGINT; SIGTERM] (module Weft_os)
  @@ fun () ->
  Fiber.scope
  @@ fun sw ->
  let client =
    Caravan_client.make ~config:Example_client.App.client_config sw
  in
  let handler =
    Example_client.App.make ~report:prerr_endline ~client ~upstream
  in
  let server = Server.start sw handler in
  Format.printf "Listening on http://%a, upstream http://%s (Ctrl-C to stop)@."
    Conn_info.pp_addr (Server.local_addr server) upstream ;
  (* The first signal stops the server with a grace, the second aborts it. *)
  Fiber.fork_daemon sw (fun () -> Server.stop_on_signals server) ;
  Server.wait server ;
  Caravan_client.close client
