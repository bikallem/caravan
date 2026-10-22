(* An echo server on port 9001 for the Autobahn test suite (make autobahn). The
   cases 9.x send messages of up to 16 MiB. The test suite is not a browser, so
   all origins are permitted. *)

open Caravan
module Ws = Caravan_websocket

let echo c =
  let rec loop () =
    Ws.send c (Ws.recv c) ;
    loop ()
  in
  loop ()

let handler req =
  Ws.accept ~origin:(fun _ -> true) ~max_message:(64 * 1024 * 1024) req echo

let () =
  let config = {Config.default with addr= Weft.Net.Tcp ("127.0.0.1", 9001)} in
  (* make autobahn starts the test suite after this line. *)
  let on_start _ = print_endline "listening on port 9001" in
  Caravan_run.run ~config ~on_start handler
