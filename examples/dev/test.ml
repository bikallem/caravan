(* The tests of the dev library. Each test sets a seeded generator first, then
   serves a page over TLS in the simulator of weft, on one domain. The simulator
   runs on one OS thread, thus the Fortuna generator is correct here. *)

open Windtrap
open Weft
open Caravan
open Example_dev
module T = Caravan_testing
module C = Caravan_client

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan dev tests"
       (module Mirage_crypto_rng.Fortuna) )

(* The time of the certificates: 2025-06-15. *)
let t0 = 1_750_000_000.

let time = Option.get (Ptime.of_float_s t0)

let day = 86400.

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let hello _req = Response.string "hello"

(* [alpn] gives the ALPN protocol of the connection. *)
let alpn req =
  let alpn = Option.bind (Request.conn req) Conn_info.alpn in
  Response.string (Option.value ~default:"none" alpn)

(* [run ?handler ?at ~names f] serves [handler] over TLS with a new certificate
   for [names], and calls [f] with it. The posix time of the simulator is [at]
   (t0 by default). Three names go to 127.0.0.1. *)
let run ?(handler = hello) ?(at = t0) ~names f =
  seed () ;
  let cert = Dev_cert.make ~time ~names in
  T.run
    ~engine:(Caravan_tls.engine (Dev_cert.server_config cert) Server.http1)
    handler
    (fun t ->
      let sim = T.sim t in
      Weft_sim.set_posix sim (Time.posix_of_unix_s at) ;
      List.iter
        (fun n -> Weft_sim.set_dns sim n (Addresses ["127.0.0.1"]))
        ["localhost"; "app.test"; "other.test"] ;
      f cert )

(* [fetch authenticator url] is the body of a GET of [url], or "Tls" for [Error
   (Tls _)]. The port of the test server is 80. *)
let fetch authenticator url =
  let config =
    ok (Tls.Config.client ~authenticator ~alpn_protocols:["http/1.1"] ())
  in
  C.run ~tls:(Caravan_tls.https config) (fun c ->
      match C.get c url with
      | resp ->
          Body.to_string (C.body resp)
      | exception C.Error (Tls _) ->
          "Tls" )

let trusts cert = Trust.of_cas [Dev_cert.ca cert]

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

(* A PEM block with no certificate in it. *)
let bad_pem = "-----BEGIN CERTIFICATE-----\nAAAA\n-----END CERTIFICATE-----\n"

(* [ca_id cert] is the key ID of the CA of [cert]. *)
let ca_id cert =
  X509.Public_key.id (X509.Certificate.public_key (Dev_cert.ca cert))

let with_cert_file value f =
  Unix.putenv "SSL_CERT_FILE" value ;
  Fun.protect ~finally:(fun () -> Unix.putenv "SSL_CERT_FILE" "") f

let () =
  exit
  @@ Windtrap.run "dev"
       [ test "a client that trusts the CA gets the page" (fun () ->
             run ~names:["localhost"] (fun cert ->
                 equal string "hello"
                   (fetch (trusts cert) "https://localhost:80/") ) )
       ; test "the certificate is for each of its names, and no other"
           (fun () ->
             run ~names:["localhost"; "app.test"] (fun cert ->
                 equal string "hello"
                   (fetch (trusts cert) "https://localhost:80/") ;
                 equal string "hello"
                   (fetch (trusts cert) "https://app.test:80/") ;
                 equal string "Tls"
                   (fetch (trusts cert) "https://other.test:80/") ;
                 equal string "Tls"
                   (fetch (trusts cert) "https://127.0.0.1:80/") ) )
       ; test "an IP address goes in the certificate as an address" (fun () ->
             run ~names:["127.0.0.1"] (fun cert ->
                 equal string "hello"
                   (fetch (trusts cert) "https://127.0.0.1:80/") ) )
       ; test "the CA of another make is not trusted" (fun () ->
             run ~names:["localhost"] (fun _ ->
                 let other = Dev_cert.make ~time ~names:["localhost"] in
                 equal string "Tls"
                   (fetch (trusts other) "https://localhost:80/") ) )
       ; test "valid from one day before the time to 30 days after it"
           (fun () ->
             let at_offset s =
               run ~at:(t0 +. s) ~names:["localhost"] (fun cert ->
                   fetch (trusts cert) "https://localhost:80/" )
             in
             equal string "hello" (at_offset (-.day +. 60.)) ;
             equal string "Tls" (at_offset (-.day -. 60.)) ;
             equal string "hello" (at_offset ((30. *. day) -. 60.)) ;
             equal string "Tls" (at_offset ((30. *. day) +. 60.)) )
       ; test "the server configuration has the ALPN protocol http/1.1"
           (fun () ->
             run ~handler:alpn ~names:["localhost"] (fun cert ->
                 equal string "http/1.1"
                   (fetch (trusts cert) "https://localhost:80/") ) )
       ; test "the CA has the path length 0 and a subject key ID" (fun () ->
             seed () ;
             let cert = Dev_cert.make ~time ~names:["localhost"] in
             let ext = X509.Certificate.extensions (Dev_cert.ca cert) in
             let find k = X509.Extension.find k ext in
             is_true (find Basic_constraints = Some (true, (true, Some 0))) ;
             is_true (find Subject_key_id = Some (false, ca_id cert)) )
       ; test "the server certificate has the key ID of the CA" (fun () ->
             run ~names:["localhost"] (fun cert ->
                 let chain = ref [] in
                 let auth : X509.Authenticator.t =
                  fun ?ip ~host certs ->
                   chain := certs ;
                   trusts cert ?ip ~host certs
                 in
                 equal string "hello" (fetch auth "https://localhost:80/") ;
                 let ext = X509.Certificate.extensions (List.hd !chain) in
                 match X509.Extension.find Authority_key_id ext with
                 | Some (_, (id, _, _)) ->
                     equal (option string) (Some (ca_id cert)) id
                 | None ->
                     fail "no authority key ID" ) )
       ; test "make refuses an empty list of names" (fun () ->
             seed () ;
             raises_match
               (function Invalid_argument _ -> true | _ -> false)
               (fun () -> ignore (Dev_cert.make ~time ~names:[])) )
       ; test "write_ca writes the CA in PEM, and no key" (fun () ->
             seed () ;
             let cert = Dev_cert.make ~time ~names:["localhost"] in
             let file = Filename.temp_file "dev-ca" ".pem" in
             Dev_cert.write_ca cert file ;
             let text = In_channel.with_open_bin file In_channel.input_all in
             Sys.remove file ;
             equal (list string)
               [X509.Certificate.encode_pem (Dev_cert.ca cert)]
               (List.map X509.Certificate.encode_pem
                  (ok (X509.Certificate.decode_pem_multiple text)) ) ;
             is_false (contains text "KEY") )
       ; test "Trust.system reads the file of SSL_CERT_FILE" (fun () ->
             run ~names:["localhost"] (fun cert ->
                 let file = Filename.temp_file "dev-ca" ".pem" in
                 Dev_cert.write_ca cert file ;
                 let body =
                   with_cert_file file (fun () ->
                       fetch (Trust.system ()) "https://localhost:80/" )
                 in
                 Sys.remove file ; equal string "hello" body ) )
       ; test "system_file: SSL_CERT_FILE, else the file of Debian" (fun () ->
             let file value = with_cert_file value Trust.system_file in
             equal string "/etc/ssl/certs/ca-certificates.crt" (file "") ;
             equal string "/a/ca.pem" (file "/a/ca.pem") )
       ; test "of_file skips a certificate that does not decode" (fun () ->
             run ~names:["localhost"] (fun cert ->
                 let file = Filename.temp_file "dev-ca" ".pem" in
                 let pem c = X509.Certificate.encode_pem (Dev_cert.ca c) in
                 let other = Dev_cert.make ~time ~names:["localhost"] in
                 (* The CA of the server is the second good one. *)
                 Out_channel.with_open_bin file (fun oc ->
                     output_string oc
                       (bad_pem ^ pem other ^ bad_pem ^ pem cert ^ bad_pem) ) ;
                 let trust = Trust.of_file file in
                 Sys.remove file ;
                 equal string "hello" (fetch trust "https://localhost:80/") ) )
       ; test "Trust.system fails for a file with no certificate" (fun () ->
             let file = Filename.temp_file "empty" ".pem" in
             let r =
               with_cert_file file (fun () ->
                   match Trust.system () with
                   | (_ : X509.Authenticator.t) ->
                       "no failure"
                   | exception Failure _ ->
                       "Failure" )
             in
             Sys.remove file ; equal string "Failure" r )
       ; test "Trust.system fails for a file that does not exist" (fun () ->
             with_cert_file "/nonexistent/ca.pem" (fun () ->
                 raises_match
                   (function Failure _ -> true | _ -> false)
                   (fun () -> ignore (Trust.system () : X509.Authenticator.t)) ) )
       ]
