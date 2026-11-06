(* Sign-in with OpenID Connect (OIDC), for example with Google: the
   authorization code flow with PKCE.

   First, register a client at the provider, with this redirect URI. The
   provider then gives a client ID and a client secret:

   http://127.0.0.1:8080/auth/callback

   This URI is for development only. In production, use an https redirect URI on
   the domain of your site: https://<your site>/auth/callback.

   For Google: in the Google Cloud console, make an OAuth client of the type
   "Web application" ("APIs & Services", then "Credentials"), and add the
   redirect URI to its authorized redirect URIs. The names of the console can
   change. RFC 8252 recommends 127.0.0.1, not localhost, for an http redirect
   URI.

   Then set these variables (the issuer of Google is
   https://accounts.google.com):

   export OIDC_ISSUER=https://accounts.google.com

   export OIDC_CLIENT_ID='<the client ID>'

   Type the secret with read: then the secret is not in the shell history.

   read -rs OIDC_CLIENT_SECRET; export OIDC_CLIENT_SECRET

   Run dune exec examples/oidc/main.exe.

   Then open http://127.0.0.1:8080/me in a browser. With no user, /me sends the
   browser to /auth/login, and the login sends it to the provider. After the
   sign-in, the provider sends the browser to /auth/callback, and the callback
   sends it back to /me.

   The login keeps its state in the session. Thus the callback needs the session
   cookie of the login. Open 127.0.0.1, not localhost: the cookie of localhost
   does not go to 127.0.0.1. The session cookie has the name __Host-session,
   thus it has Secure. Some browsers do not keep a Secure cookie over http for
   127.0.0.1. With no cookie, the callback finds no login: it gives 400, and the
   report on stderr shows No_login.

   With curl, first set this shell variable:

   S=127.0.0.1:8080

   The navigation of a browser to /me with no user goes to the login, with the
   return path. Another request gets 401:

   curl -i -H 'Sec-Fetch-Mode: navigate' $S/me

   curl -i $S/me

   The login gives 303 to the provider, with state, nonce and code_challenge in
   the query:

   curl -i "$S/auth/login?return=/me" *)

open Weft
open Caravan
open Caravan_html
module Auth = Caravan_auth
module Oidc = Caravan_auth.Oidc
module User = Caravan_auth.User

(* {1 The users} *)

(* The users, in memory. A real application keeps them in a database. The key of
   an account is the issuer and the subject (the sub claim). Each provider gives
   its own subjects, thus the issuer is a part of the key. The domains of the
   server share the table: thus a mutex protects it.

   Never find or link an account by the email claim. Another provider or tenant
   can send any address, and email_verified is only the statement of the
   provider. Also, a user can change the address. *)
type users =
  { mutex: Mutex.t
  ; accounts: (string * string, User.t) Hashtbl.t
  ; by_id: (string, User.t) Hashtbl.t }

let users () =
  {mutex= Mutex.create (); accounts= Hashtbl.create 16; by_id= Hashtbl.create 16}

(* [account users ~issuer ~subject ~name] is the user of the account [subject]
   of [issuer]. The first sign-in of the account makes a user, with a new ID. *)
let account users ~issuer ~subject ~name =
  Mutex.protect users.mutex (fun () ->
      match Hashtbl.find_opt users.accounts (issuer, subject) with
      | Some u ->
          u
      | None ->
          let id = string_of_int (Hashtbl.length users.by_id + 1) in
          let u =
            User.make
              ~claims:[("name", name); ("issuer", issuer); ("subject", subject)]
              id
          in
          Hashtbl.replace users.accounts (issuer, subject) u ;
          Hashtbl.replace users.by_id id u ;
          u )

let find_user users id =
  Mutex.protect users.mutex (fun () -> Hashtbl.find_opt users.by_id id)

(* {1 The pages} *)

let see_other path =
  Response.empty ~status:Status.see_other
    ~headers:(Headers.of_list [("Location", path)])
    ()

let page title children =
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head [El.meta ~at:[At.v "charset" "utf-8"] (); El.title title]
        ; El.body children ] ]

let claim name u = Option.value ~default:"" (User.claim name u)

(* The logout form, with the CSRF token. *)
let logout_form req =
  El.form
    ~at:[At.method_ "post"; At.action "/logout"]
    [ El.input
        ~at:
          [ At.type_ "hidden"
          ; At.name "_csrf"
          ; At.value (Caravan_session.Csrf.token req) ]
        ()
    ; El.button [txt "Sign out"] ]

let home req =
  response
    (page "OIDC"
       ( match Auth.user req with
       | Some u ->
           [ El.p [txtf "Hello, %s." (claim "name" u)]
           ; El.a ~at:[At.href "/me"] [txt "Your account"] ]
       | None ->
           [ El.p [txt "Hello, guest."]
           ; El.a ~at:[At.href "/auth/login?return=/me"] [txt "Sign in"] ] ) )

(* The page of the user: the name, the account at the provider, and the logout
   form. Caravan_html escapes the claims of the provider. *)
let me req =
  let u = Option.get (Auth.user req) in
  response
    (page "Your account"
       [ El.p [txtf "Signed in as %s." (claim "name" u)]
       ; El.p
           [ txtf "User %s: the account %s of %s." (User.id u)
               (claim "subject" u) (claim "issuer" u) ]
       ; logout_form req ] )

(* {1 The sign-in} *)

(* The callback calls [signed_in] after it checked the ID token. The issuer of
   the token is that of the provider: verify_id_token checks it. The name is the
   name claim, else the subject. sign_in gives the session a new ID (no session
   fixation). [return] is the return path of the login: Oidc.login keeps only a
   path of this site, else "/". *)
let signed_in ~users req (t : Oidc.tokens) ~return =
  let issuer = Option.get (Auth.Jwt.issuer t.claims) in
  let name =
    Option.value ~default:t.subject (Auth.Jwt.string "name" t.claims)
  in
  Auth.sign_in req (account users ~issuer ~subject:t.subject ~name) ;
  see_other return

(* A failed callback gives 400, and [report] gets the failure. For example
   No_login: the session has no login with the state of the callback. *)
let failed ~report _req failure =
  report (Format.asprintf "oidc: %a" Oidc.pp_failure failure) ;
  response ~status:Status.bad_request
    (page "Sign-in failed"
       [ El.p [txt "The sign-in failed."]
       ; El.a ~at:[At.href "/auth/login"] [txt "Try again"] ] )

(* sign_out ends the session, thus the old cookie gives no user. *)
let logout req = Auth.sign_out req ; see_other "/"

(* {1 The handler} *)

(* [make ~key ~report ~oidc ~users] is the handler. [key] signs the session
   cookies and the CSRF tokens. [oidc] is the client of the provider. [report]
   gets each failed callback.

   Each login of a browser with no session makes a session. Thus the login route
   has a rate limit: each client address has a burst of 10 logins, and one more
   login each 6 s. The 11th login of a burst gets 429. Each failed callback
   gives a line to [report]. Thus the callback has a rate limit too, with its
   own bucket of the same size.

   [validate] reads the user of the session in the table at each request. A user
   that is no longer in the table signs out.

   The order: Cross_origin, the session, the users, then the CSRF tokens.
   Cross_origin and the CSRF token protect the logout form. *)
let make ~key ~report ~oidc ~users =
  let session =
    Auth.Scheme.session ~login:"/auth/login"
      ~validate:(fun u -> find_user users (User.id u))
      ()
  in
  let limit () =
    Caravan_middleware.Rate_limit.middleware ~burst:10 ~every:(Span.s 6) ()
  in
  Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Caravan_session.middleware ~keys:[key]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Auth.middleware ~schemes:[session] ()
    ; Caravan_session.Csrf.middleware () ]
    Caravan_router.(
      handler
        [ get "/" home
        ; get "/me" (Auth.require () me)
        ; get "/auth/login" (limit () (Oidc.login oidc))
        ; get "/auth/callback"
            (limit ()
               (Oidc.callback ~failed:(failed ~report) oidc (signed_in ~users)) )
        ; post "/logout" logout ] )
