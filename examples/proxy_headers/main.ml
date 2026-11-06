module App = Example_proxy_headers.App

(* The proxy runs on this host, thus its address is 127.0.0.1. *)
let () =
  Example_common.serve
    (App.make
       ~trusted:(Caravan_middleware.Forwarded.trust_ips ["127.0.0.1"])
       ~backends:(App.backends ()) ~log:print_endline ~report:prerr_endline )
