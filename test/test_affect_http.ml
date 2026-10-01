open Windtrap

let smoke = group "smoke" [test "library links" (fun () -> equal int 1 1)]

let () = exit (run "affect-http" [smoke])
