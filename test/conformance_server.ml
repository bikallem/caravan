(* The server of make conformance. It serves Conformance_app without the fixture
   paths on 127.0.0.1 and the port of argv.(1). The configuration is the
   default, thus responses get a Date field. *)

open Caravan

let () =
  let port =
    match Sys.argv with
    | [|_; port|] ->
        int_of_string port
    | _ ->
        prerr_endline "usage: conformance_server PORT" ;
        exit 2
  in
  let config =
    {Config.default with addr= Unix.ADDR_INET (Unix.inet_addr_loopback, port)}
  in
  (* make conformance starts the test tool after this line. *)
  let on_start _ = print_endline "listening" in
  Server.run ~config ~on_start (Conformance_app.handler ~fixtures:false)
