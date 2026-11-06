(* The tests of the tls example. Each test serves the application over TLS with
   a development certificate, through the real server and client of caravan, in
   the simulator of weft. Each test sets a seeded generator first, and runs on
   one domain: the simulator runs on one OS thread, thus the Fortuna generator
   is correct here. The test server listens on port 80, thus the URLs have the
   port 80 in place of 8443. *)

open Windtrap
open Weft
open Caravan
open Example_dev
module T = Caravan_testing
module C = Caravan_client
module App = Example_tls.App

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan tls example"
       (module Mirage_crypto_rng.Fortuna) )

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

(* The time of the simulator and of the certificate: 2025-06-15. *)
let t0 = 1_750_000_000.

let time = Option.get (Ptime.of_float_s t0)

(* [run f] serves the application over TLS with a new development certificate
   for localhost and app.test, which go to 127.0.0.1. It calls [f] with the
   certificate. *)
let run f =
  seed () ;
  let cert = Dev_cert.make ~time ~names:["localhost"; "app.test"] in
  T.run
    ~engine:(Caravan_tls.engine (Dev_cert.server_config cert) Server.http1)
    App.handler
    (fun t ->
      let sim = T.sim t in
      Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
      Weft_sim.set_dns sim "localhost" (Addresses ["127.0.0.1"]) ;
      Weft_sim.set_dns sim "app.test" (Addresses ["127.0.0.1"]) ;
      f cert )

(* The client configuration of curl --cacert: it trusts the CA of [cert], and
   offers the ALPN protocol http/1.1 (no ALPN offer with ~alpn:false). *)
let client_config ?(alpn = true) cert =
  ok
    (Tls.Config.client
       ~authenticator:(Trust.of_cas [Dev_cert.ca cert])
       ?alpn_protocols:(if alpn then Some ["http/1.1"] else None)
       () )

type reply = {body: string; hsts: string option}

(* [get config url] is the reply to a GET of [url], with a client that uses the
   TLS configuration [config]. *)
let get config url =
  C.run ~tls:(Caravan_tls.https config) (fun c ->
      let resp = C.get c url in
      { body= Body.to_string (C.body resp)
      ; hsts= Headers.find "strict-transport-security" (Response.headers resp)
      } )

let () =
  exit
  @@ Windtrap.run "tls"
       [ test "localhost: scheme https, ALPN http/1.1, no HSTS" (fun () ->
             run (fun cert ->
                 let r = get (client_config cert) "https://localhost:80/" in
                 equal string "scheme: https\nalpn: http/1.1\n" r.body ;
                 equal (option string) None r.hsts ) )
       ; test "a client with no ALPN offer: alpn: none" (fun () ->
             run (fun cert ->
                 let r =
                   get (client_config ~alpn:false cert) "https://localhost:80/"
                 in
                 equal string "scheme: https\nalpn: none\n" r.body ) )
       ; test "app.test gets Strict-Transport-Security" (fun () ->
             run (fun cert ->
                 let r = get (client_config cert) "https://app.test:80/" in
                 equal string "scheme: https\nalpn: http/1.1\n" r.body ;
                 equal (option string) (Some "max-age=31536000") r.hsts ) )
       ; test "an http request gets no Strict-Transport-Security" (fun () ->
             (* The application with no TLS engine. *)
             T.run App.handler (fun t ->
                 Weft_sim.set_dns (T.sim t) "app.test" (Addresses ["127.0.0.1"]) ;
                 let r =
                   T.fetch t (Request.make ~authority:"app.test" GET "/")
                 in
                 equal string "scheme: http\nalpn: none\n" r.body ;
                 equal (option string) None
                   (Headers.find "strict-transport-security" r.headers) ) )
       ; test "a client that does not trust the CA gets Error (Tls _)"
           (fun () ->
             run (fun _ ->
                 (* Another CA: the generator continues, thus new keys. *)
                 let other = Dev_cert.make ~time ~names:["localhost"] in
                 match get (client_config other) "https://localhost:80/" with
                 | _ ->
                     fail "the client trusted the certificate"
                 | exception C.Error (Tls _) ->
                     () ) ) ]
