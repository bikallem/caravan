(* The tests of the shared code of the examples. *)

open Windtrap

let () =
  exit
  @@ Windtrap.run "common"
       [ test "url: the scheme, the host and the port of the address" (fun () ->
             let addr = Weft.Net.Tcp ("127.0.0.1", 8080) in
             equal string "http://127.0.0.1:8080"
               (Example_common.url ~scheme:"http" ~host:"127.0.0.1" addr) ;
             equal string "http://localhost:8080"
               (Example_common.url ~scheme:"http" ~host:"localhost" addr) ;
             equal string "https://localhost:8443"
               (Example_common.url ~scheme:"https" ~host:"localhost"
                  (Weft.Net.Tcp ("127.0.0.1", 8443)) ) ) ]
