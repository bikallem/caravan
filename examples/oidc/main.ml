(* The oidc example on 127.0.0.1:8080. The header comment of app.ml tells how to
   register a client at a provider and set the variables OIDC_ISSUER,
   OIDC_CLIENT_ID and OIDC_CLIENT_SECRET. The program makes the client and the
   server in one scope, and stops at Ctrl-C (SIGINT) or SIGTERM. *)

open Weft
open Caravan
module Oidc = Caravan_auth.Oidc
module App = Example_oidc.App

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

(* [env name] is the value of the environment variable [name]. With no value,
   the program stops with a message. *)
let env name =
  match Sys.getenv_opt name with
  | Some v when v <> "" ->
      v
  | _ ->
      Printf.eprintf
        "Set the environment variable %s: see examples/oidc/app.ml.\n" name ;
      exit 2

let redirect_uri = "http://127.0.0.1:8080/auth/callback"

let () =
  let issuer = env "OIDC_ISSUER" in
  let id = env "OIDC_CLIENT_ID" in
  let secret = env "OIDC_CLIENT_SECRET" in
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let key = Example_dev.Dev_key.session () in
  (* The client trusts the CAs of the system, as a browser does. Trust.system is
     a stand-in for development. In production, use Ca_certs.authenticator ()
     (README "OpenID Connect"). *)
  let tls =
    Caravan_tls.https
      (ok (Tls.Config.client ~authenticator:(Example_dev.Trust.system ()) ()))
  in
  let code =
    Weft.run ~signals:[SIGINT; SIGTERM] (module Weft_os)
    @@ fun () ->
    Fiber.scope
    @@ fun sw ->
    let client = Caravan_client.make ~tls sw in
    match Oidc.Provider.discover client issuer with
    | Error e ->
        Printf.eprintf "The discovery of %s failed: %s\n" issuer e ;
        Caravan_client.close client ;
        1
    | Ok provider ->
        let oidc = Oidc.client ~secret ~provider ~id ~redirect_uri () in
        let users = App.users () in
        let handler = App.make ~key ~report:prerr_endline ~oidc ~users in
        let config = {Config.default with addr= Tcp ("127.0.0.1", 8080)} in
        let server = Server.start ~config sw handler in
        Format.printf "Open http://127.0.0.1:8080/me (Ctrl-C to stop)@." ;
        (* The first signal stops the server with a grace, the second aborts
           it. *)
        Fiber.fork_daemon sw (fun () -> Server.stop_on_signals server) ;
        Server.wait server ;
        Caravan_client.close client ;
        0
  in
  exit code
