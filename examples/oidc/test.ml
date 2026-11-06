(* The tests of the oidc example. Each test runs the application through the
   real server and client of caravan, in the simulator of weft, on one domain.
   Each run first sets a seeded generator.

   The test server serves the application on 127.0.0.1:80. A fake provider runs
   as a second server in the same simulator: on 127.0.0.1:443, with the name
   idp.test, over TLS with a development certificate. The application gets its
   client in the run, thus the test handler reads the application from a cell.

   The jar of the tests keeps the cookies of the browser. The browser follows
   the redirects of the login: the test sends the browser to the provider, and
   the provider sends it back to the callback, with a code. *)

open Windtrap
open Weft
open Caravan
open Example_dev
module T = Caravan_testing
module C = Caravan_client
module Oidc = Caravan_auth.Oidc
module Jar = Example_support.Jar
module Page = Example_support.Page
module App = Example_oidc.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan oidc example"
       (module Mirage_crypto_rng.Fortuna) )

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

(* The time of the simulator: 2025-06-15. *)
let t0 = 1_750_000_000

let issuer = "https://idp.test"

let client_id = "app"

let secret = "the secret of the client"

let redirect_uri = "http://127.0.0.1:8080/auth/callback"

let key = Caravan_session.Key.of_string (String.make 32 'k')

(* {1 The fake provider} *)

(* What a code of the provider is for. *)
type grant = {nonce: string; challenge: string}

(* The state of the fake provider. The test changes the account, or the nonce or
   the audience of the next ID token. *)
type idp =
  { jwt: Dev_jwt.t
  ; mutable subject: string
  ; mutable name: string option  (** [None]: no name claim. *)
  ; mutable email: string
  ; mutable nonce: string option  (** In place of the nonce of the login. *)
  ; mutable audience: string option  (** In place of the client ID. *)
  ; mutable codes: (string * grant) list
  ; mutable next: int }

let json_response j = Response.string (Yojson.Safe.to_string j)

let error_response e =
  Response.string ~status:Status.bad_request
    (Yojson.Safe.to_string (`Assoc [("error", `String e)]))

let document =
  `Assoc
    [ ("issuer", `String issuer)
    ; ("authorization_endpoint", `String (issuer ^ "/authorize"))
    ; ("token_endpoint", `String (issuer ^ "/token"))
    ; ("jwks_uri", `String (issuer ^ "/jwks"))
    ; ("response_types_supported", `List [`String "code"])
    ; ("code_challenge_methods_supported", `List [`String "S256"])
    ; ("authorization_response_iss_parameter_supported", `Bool true)
    ; ( "token_endpoint_auth_methods_supported"
      , `List [`String "client_secret_basic"] ) ]

(* The PKCE challenge of a verifier: the base64url of its SHA-256 (RFC 7636
   §4.2). *)
let s256 verifier =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
    Digestif.SHA256.(to_raw_string (digest_string verifier))

(* [basic v] is the client ID and the secret of the Authorization field [v]
   (client_secret_basic). The client form-encodes the two parts first (RFC 6749
   §2.3.1). *)
let basic v =
  let decode s =
    match Form.parse s with [(text, "")] -> text | _ -> failwith "Basic"
  in
  match String.split_on_char ' ' v with
  | ["Basic"; b64] -> (
    match Base64.decode b64 with
    | Ok pair -> (
      match String.index_opt pair ':' with
      | Some i ->
          Some
            ( decode (String.sub pair 0 i)
            , decode (String.sub pair (i + 1) (String.length pair - i - 1)) )
      | None ->
          None )
    | Error _ ->
        None )
  | _ ->
      None

(* The authorization endpoint. The user approves at once: a 303 to the redirect
   URI with a new code, the state and the issuer (RFC 9207). *)
let authorize idp req =
  let q = Request.query_params req in
  let f n = Option.value ~default:"" (Form.find n q) in
  if
    f "response_type" <> "code"
    || f "client_id" <> client_id
    || f "redirect_uri" <> redirect_uri
    || f "code_challenge_method" <> "S256"
  then error_response "invalid_request"
  else
    let code = Printf.sprintf "code%d" idp.next in
    idp.next <- idp.next + 1 ;
    idp.codes <-
      (code, {nonce= f "nonce"; challenge= f "code_challenge"}) :: idp.codes ;
    Response.empty ~status:Status.see_other
      ~headers:
        (Headers.of_list
           [ ( "Location"
             , redirect_uri ^ "?"
               ^ Form.encode
                   [("code", code); ("state", f "state"); ("iss", issuer)] ) ] )
      ()

(* The ID token of a grant, signed with the ES256 key of the provider. *)
let id_token idp (g : grant) =
  let name =
    Option.fold ~none:[] ~some:(fun n -> [("name", `String n)]) idp.name
  in
  Dev_jwt.sign idp.jwt
    (Yojson.Safe.to_string
       (`Assoc
          ( [ ("iss", `String issuer)
            ; ("aud", `String (Option.value idp.audience ~default:client_id))
            ; ("sub", `String idp.subject)
            ; ("email", `String idp.email)
            ; ("nonce", `String (Option.value idp.nonce ~default:g.nonce))
            ; ("iat", `Int t0)
            ; ("exp", `Int (t0 + 3600)) ]
          @ name ) ) )

(* The token endpoint. It checks the client secret, the code (one use), the
   redirect URI and the PKCE verifier, as a real provider does. *)
let token idp req =
  let form = Result.get_ok (Request.form req) in
  let f n = Option.value ~default:"" (Form.find n form) in
  let code = f "code" in
  match List.assoc_opt code idp.codes with
  | Some g
    when Option.bind (Headers.find "authorization" (Request.headers req)) basic
         = Some (client_id, secret)
         && f "grant_type" = "authorization_code"
         && f "redirect_uri" = redirect_uri
         && s256 (f "code_verifier") = g.challenge ->
      idp.codes <- List.remove_assoc code idp.codes ;
      json_response
        (`Assoc
           [ ("access_token", `String "at")
           ; ("token_type", `String "Bearer")
           ; ("expires_in", `Int 3600)
           ; ("id_token", `String (id_token idp g)) ] )
  | _ ->
      error_response "invalid_grant"

let idp_handler idp req =
  match Request.path_only req with
  | "/.well-known/openid-configuration" ->
      json_response document
  | "/jwks" ->
      Response.string (Dev_jwt.jwks idp.jwt)
  | "/authorize" ->
      authorize idp req
  | "/token" ->
      token idp req
  | _ ->
      Response.string ~status:Status.not_found ""

(* {1 The runs} *)

type env =
  { t: T.t
  ; idp: idp
  ; browser: C.t  (** The client of the browser, for the provider. *)
  ; users: App.users
  ; reports: string list ref  (** The reports of the application. *) }

(* [run f] calls [f] with a new application and a new fake provider. *)
let run f =
  seed () ;
  let app = ref Handler.not_found in
  T.run
    (fun r -> !app r)
    (fun t ->
      let sim = T.sim t in
      Weft_sim.set_posix sim (Time.posix_of_unix_s (float_of_int t0)) ;
      Weft_sim.set_dns sim "idp.test" (Addresses ["127.0.0.1"]) ;
      (* The certificate has the time of the simulator. *)
      let time =
        Option.get (Ptime.of_float_s (Time.posix_to_unix_s (Time.now Posix)))
      in
      let cert = Dev_cert.make ~time ~names:["idp.test"] in
      let tls =
        Caravan_tls.https
          (ok
             (Tls.Config.client
                ~authenticator:(Trust.of_cas [Dev_cert.ca cert])
                () ) )
      in
      let idp =
        { jwt= Dev_jwt.make ()
        ; subject= "ada-1"
        ; name= Some "Ada Lovelace"
        ; email= "ada@example.com"
        ; nonce= None
        ; audience= None
        ; codes= []
        ; next= 1 }
      in
      Fiber.scope (fun sw ->
          let server =
            Server.start
              ~config:{Config.default with addr= Tcp ("127.0.0.1", 443)}
              ~engine:
                (Caravan_tls.engine (Dev_cert.server_config cert) Server.http1)
              sw (idp_handler idp)
          in
          let client = C.make ~tls sw and browser = C.make ~tls sw in
          Fun.protect
            ~finally:(fun () ->
              C.close client ; C.close browser ; Server.abort server )
            (fun () ->
              let provider =
                match Oidc.Provider.discover client issuer with
                | Ok p ->
                    p
                | Error e ->
                    failwith e
              in
              let oidc =
                Oidc.client ~secret ~provider ~id:client_id ~redirect_uri ()
              in
              let users = App.users () and reports = ref [] in
              let report l = reports := !reports @ [l] in
              app := App.make ~key ~report ~oidc ~users ;
              f {t; idp; browser; users; reports} ) ) )

(* {1 The browser} *)

let header name (r : T.reply) = Headers.find name r.headers

let location r = Option.get (header "location" r)

let get ?(headers = []) e jar path =
  Jar.fetch jar e.t (Request.make ~headers:(Headers.of_list headers) GET path)

(* [query url] is the query of [url]. *)
let query url =
  match String.index_opt url '?' with
  | Some i ->
      Form.parse (String.sub url (i + 1) (String.length url - i - 1))
  | None ->
      []

(* [at_provider e url] is the redirect of the provider for [url]: the URL of the
   callback, with the code. The browser goes from the redirect URI to the path
   of the test server. *)
let at_provider e url =
  let resp = C.get e.browser url in
  ignore (Body.to_string (C.body resp)) ;
  let callback = Option.get (Headers.find "location" (Response.headers resp)) in
  let n = String.length "http://127.0.0.1:8080" in
  String.sub callback n (String.length callback - n)

(* [login e jar path] is the reply of the login [path], and the path of the
   callback that the provider gives. *)
let login ?(path = "/auth/login?return=/me") e jar =
  let r = get e jar path in
  (r, at_provider e (location r))

(* [sign_in e jar] is the reply of the callback after the login [path]. *)
let sign_in ?path e jar =
  let _, callback = login ?path e jar in
  get e jar callback

let expect_failure e (r : T.reply) failure =
  equal status Status.bad_request r.status ;
  equal (list string) ["oidc: " ^ failure] !(e.reports)

let csrf (r : T.reply) = Page.csrf_token r.body

(* [with_iss iss callback] is the path [callback] with the parameter iss [iss],
   or with no iss for [None]. *)
let with_iss iss callback =
  let q = List.filter (fun (n, _) -> n <> "iss") (query callback) in
  let q = match iss with Some v -> q @ [("iss", v)] | None -> q in
  "/auth/callback?" ^ Form.encode q

let navigate = [("Sec-Fetch-Mode", "navigate")]

let cookie = "__Host-session"

let () =
  exit
  @@ Windtrap.run "oidc"
       [ test "/me with no user, in a navigation: 303 to the login" (fun () ->
             run (fun e ->
                 let r = get ~headers:navigate e (Jar.create ()) "/me" in
                 equal status Status.see_other r.status ;
                 equal string "/auth/login?return=/me" (location r) ) )
       ; test "/me with no user, not a navigation: 401" (fun () ->
             run (fun e ->
                 let r = get e (Jar.create ()) "/me" in
                 equal status Status.unauthorized r.status ) )
       ; test "/auth/login: 303 to the provider with state, nonce, S256"
           (fun () ->
             run (fun e ->
                 let r = get e (Jar.create ()) "/auth/login?return=/me" in
                 equal status Status.see_other r.status ;
                 let url = location r in
                 let prefix = issuer ^ "/authorize?" in
                 equal string prefix (String.sub url 0 (String.length prefix)) ;
                 let q = query url in
                 let p n = Form.find n q in
                 equal (option string) (Some "code") (p "response_type") ;
                 equal (option string) (Some client_id) (p "client_id") ;
                 equal (option string) (Some redirect_uri) (p "redirect_uri") ;
                 equal (option string) (Some "S256") (p "code_challenge_method") ;
                 List.iter
                   (fun n ->
                     is_true ~msg:n
                       (Option.fold ~none:false ~some:(( <> ) "") (p n)) )
                   ["state"; "nonce"; "code_challenge"] ) )
       ; test "the callback signs the user in, then goes to the return path"
           (fun () ->
             run (fun e ->
                 let jar = Jar.create () in
                 let _, callback = login e jar in
                 (* The login made the session. *)
                 let before = Jar.find cookie jar in
                 is_some before ;
                 let r = get e jar callback in
                 equal status Status.see_other r.status ;
                 equal string "/me" (location r) ;
                 not_equal ~msg:"a new session ID" (option string) before
                   (Jar.find cookie jar) ;
                 let me = get e jar "/me" in
                 equal status Status.ok me.status ;
                 contains ~sub:"Signed in as Ada Lovelace." me.body ;
                 contains ~sub:"User 1: the account ada-1 of https://idp.test."
                   me.body ;
                 contains ~sub:"Hello, Ada Lovelace." (get e jar "/").body ;
                 equal (list string) [] !(e.reports) ) )
       ; test "the page escapes the name; with no name, the subject" (fun () ->
             run (fun e ->
                 e.idp.name <- Some "<b>Ada</b>" ;
                 let jar = Jar.create () in
                 ignore (sign_in e jar) ;
                 contains ~sub:"Signed in as &lt;b&gt;Ada&lt;/b&gt;."
                   (get e jar "/me").body ;
                 e.idp.subject <- "bob-2" ;
                 e.idp.name <- None ;
                 let jar = Jar.create () in
                 ignore (sign_in e jar) ;
                 contains ~sub:"Signed in as bob-2." (get e jar "/me").body ) )
       ; test "a wrong state: 400, No_login, no user" (fun () ->
             run (fun e ->
                 let jar = Jar.create () in
                 let _, callback = login e jar in
                 let q = query callback in
                 let code = Option.get (Form.find "code" q) in
                 let r =
                   get e jar
                     ( "/auth/callback?"
                     ^ Form.encode [("code", code); ("state", "wrong")] )
                 in
                 expect_failure e r "No_login" ;
                 equal status Status.unauthorized (get e jar "/me").status ) )
       ; test "a second use of a state: 400, No_login" (fun () ->
             run (fun e ->
                 let jar = Jar.create () in
                 let _, callback = login e jar in
                 equal status Status.see_other (get e jar callback).status ;
                 let r = get e jar callback in
                 expect_failure e r "No_login" ) )
       ; test "a browser with no session cookie: 400, No_login" (fun () ->
             run (fun e ->
                 let _, callback = login e (Jar.create ()) in
                 let r = get e (Jar.create ()) callback in
                 expect_failure e r "No_login" ) )
       ; test "a token with another nonce: 400, Bad_nonce" (fun () ->
             run (fun e ->
                 e.idp.nonce <- Some "another nonce" ;
                 let jar = Jar.create () in
                 let r = sign_in e jar in
                 expect_failure e r "Id_token (Bad_nonce)" ;
                 equal status Status.unauthorized (get e jar "/me").status ) )
       ; test "a token for another audience: 400, Bad_audience" (fun () ->
             run (fun e ->
                 e.idp.audience <- Some "another-app" ;
                 let r = sign_in e (Jar.create ()) in
                 expect_failure e r "Id_token (Jwt (Bad_audience))" ) )
       ; test "a return path to another site goes to /" (fun () ->
             run (fun e ->
                 List.iter
                   (fun return ->
                     let path =
                       "/auth/login?" ^ Form.encode [("return", return)]
                     in
                     let r = sign_in ~path e (Jar.create ()) in
                     equal ~msg:return status Status.see_other r.status ;
                     equal ~msg:return string "/" (location r) )
                   ["https://evil.example/"; "//evil.example/"] ) )
       ; test "the same account again: the same user, also with a new email"
           (fun () ->
             run (fun e ->
                 let first = sign_in e (Jar.create ()) in
                 equal status Status.see_other first.status ;
                 e.idp.email <- "ada@new.example" ;
                 let jar = Jar.create () in
                 ignore (sign_in e jar) ;
                 contains ~sub:"User 1:" (get e jar "/me").body ;
                 (* Another account of the provider is another user, also with
                    the same email claim. *)
                 e.idp.subject <- "bob-2" ;
                 let jar = Jar.create () in
                 ignore (sign_in e jar) ;
                 contains ~sub:"User 2: the account bob-2"
                   (get e jar "/me").body ) )
       ; test "the key of an account is the issuer and the subject" (fun () ->
             let users = App.users () in
             let id issuer subject =
               Caravan_auth.User.id
                 (App.account users ~issuer ~subject ~name:"Ada")
             in
             let a = id "https://a.test" "1" in
             equal string "2" (id "https://b.test" "1") ;
             equal string a (id "https://a.test" "1") )
       ; test "the 11th login of a burst gets 429" (fun () ->
             run (fun e ->
                 let codes =
                   List.init 11 (fun _ ->
                       (get e (Jar.create ()) "/auth/login").status )
                 in
                 equal (list status)
                   ( List.init 10 (fun _ -> Status.see_other)
                   @ [Status.too_many_requests] )
                   codes ;
                 let login () = (get e (Jar.create ()) "/auth/login").status in
                 Time.sleep Mono (Span.s 5) ;
                 equal ~msg:"after 5 s" status Status.too_many_requests
                   (login ()) ;
                 Time.sleep Mono (Span.s 1) ;
                 equal ~msg:"after 6 s" status Status.see_other (login ()) ) )
       ; test "the callback with another iss, or with no iss: Bad_response"
           (fun () ->
             run (fun e ->
                 let jar = Jar.create () in
                 let _, callback = login e jar in
                 let evil = with_iss (Some "https://evil.test") callback in
                 let r = get e jar evil in
                 expect_failure e r
                   "Bad_response \"the iss parameter is not the issuer\"" ;
                 e.reports := [] ;
                 let _, callback = login e jar in
                 let r = get e jar (with_iss None callback) in
                 expect_failure e r "Bad_response \"no iss parameter\"" ;
                 equal status Status.unauthorized (get e jar "/me").status ) )
       ; test "the 11th callback of a burst gets 429, and no report" (fun () ->
             run (fun e ->
                 let jar = Jar.create () in
                 let callback () =
                   (get e jar "/auth/callback?code=c&state=wrong").status
                 in
                 let codes = List.init 11 (fun _ -> callback ()) in
                 equal (list status)
                   ( List.init 10 (fun _ -> Status.bad_request)
                   @ [Status.too_many_requests] )
                   codes ;
                 equal int 10 (List.length !(e.reports)) ;
                 (* The login has its own bucket. *)
                 equal status Status.see_other
                   (get e (Jar.create ()) "/auth/login").status ;
                 Time.sleep Mono (Span.s 5) ;
                 equal ~msg:"after 5 s" status Status.too_many_requests
                   (callback ()) ;
                 Time.sleep Mono (Span.s 1) ;
                 equal ~msg:"after 6 s" status Status.bad_request (callback ()) ) )
       ; test "a user that is no longer in the table signs out" (fun () ->
             run (fun e ->
                 let jar = Jar.create () in
                 ignore (sign_in e jar) ;
                 equal status Status.ok (get e jar "/me").status ;
                 Mutex.protect e.users.mutex (fun () ->
                     Hashtbl.reset e.users.by_id ) ;
                 equal status Status.unauthorized (get e jar "/me").status ) )
       ; test "the logout needs the CSRF token, from the same origin" (fun () ->
             run (fun e ->
                 let jar = Jar.create () in
                 ignore (sign_in e jar) ;
                 let token = csrf (get e jar "/me") in
                 let post ?(headers = []) form =
                   Jar.fetch jar e.t
                     (Request.with_form form
                        (Request.make ~headers:(Headers.of_list headers) POST
                           "/logout" ) )
                 in
                 let no_token = post [] in
                 equal ~msg:"no token" status Status.forbidden no_token.status ;
                 equal ~msg:"cross-site" status Status.forbidden
                   (post
                      ~headers:[("Sec-Fetch-Site", "cross-site")]
                      [("_csrf", token)] )
                     .status ;
                 equal status Status.ok (get e jar "/me").status ;
                 let old = Option.get (Jar.find cookie jar) in
                 let r = post [("_csrf", token)] in
                 equal status Status.see_other r.status ;
                 equal string "/" (location r) ;
                 contains ~sub:"Hello, guest." (get e jar "/").body ;
                 (* The old cookie gives no user. *)
                 let me =
                   T.fetch e.t
                     (Request.make
                        ~headers:
                          (Headers.of_list [("Cookie", cookie ^ "=" ^ old)])
                        GET "/me" )
                 in
                 equal status Status.unauthorized me.status ) ) ]
