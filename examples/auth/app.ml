(* Users and policies: a login with a session, and routes that need a user, a
   role, or more.

   The login has no password: it is a demo only. A real login checks the
   password with a password hash, for example argon2 of another library, and
   never compares the text of a password. caravan has no password hash.
   caravan.auth also has logins with no password: codes by email, passkeys and
   OpenID Connect.

   Run dune exec examples/auth/main.exe, then open http://localhost:8080/ in a
   browser. The session cookie has the name __Host-session, thus it has Secure.
   Chrome and Firefox keep a Secure cookie over http for localhost, but some
   browsers do not keep it for 127.0.0.1.

   With curl, first set this shell variable:

   S=127.0.0.1:8080

   With no user, the navigation of a browser goes to the login page, with the
   return path. Another request gets 401:

   curl -i -H 'Sec-Fetch-Mode: navigate' $S/admin

   curl -i $S/admin

   ada is an admin. After the login, the response goes to the return path. The
   jar of curl keeps the session cookie:

   curl -i -c jar -b jar -d name=ada "$S/login?return=/admin"

   curl -i -b jar $S/admin

   curl -i -b jar $S/admin/report

   curl -i -b jar $S/me

   curl -i -b jar $S/ops

   A return path to another site goes to /:

   curl -i -c jar -b jar -d name=ada "$S/login?return=https://evil.example/"

   bob is not an admin. The file bob keeps the session cookie of bob:

   curl -i -c bob -b bob -d name=bob $S/login

   curl -i -b bob $S/admin

   curl -i -b bob $S/reports

   curl -i -b bob $S/ops

   The logout needs the CSRF token of a page. Without it, the logout gets 403:

   curl -i -b jar -X POST $S/logout

   Copy the token of the admin page to T, then send it:

   T=$(curl -s -b jar $S/admin | grep -o '[[:alnum:]_-]\{86\}')

   curl -i -c jar -b jar -d "_csrf=$T" $S/logout *)

open Caravan
open Caravan_html
module Auth = Caravan_auth
module User = Caravan_auth.User

(* {1 The users} *)

(* The users of the demo. A real application keeps its users in a database. Each
   user has an ID, roles and claims. *)
let demo_users =
  [ ( "ada"
    , User.make ~roles:["admin"; "staff"]
        ~claims:[("name", "Ada Lovelace"); ("team", "ops")]
        "ada" )
  ; ( "bob"
    , User.make ~roles:["staff"]
        ~claims:[("name", "Bob"); ("team", "sales")]
        "bob" ) ]

let demo_user id = List.assoc_opt id demo_users

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

(* The logout form, with the CSRF token. Csrf.token gives the response
   Cache-Control: no-store: a cache must not keep a token. *)
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

let name u = Option.value ~default:(User.id u) (User.claim "name" u)

(* A public page: a user or a guest. *)
let home req =
  response
    (page "Auth"
       ( match Auth.user req with
       | Some u ->
           [El.p [txtf "Hello, %s." (name u)]; logout_form req]
       | None ->
           [ El.p [txt "Hello, guest."]
           ; El.a ~at:[At.href "/login"] [txt "Sign in"] ] ) )

(* The login page. The form sends the return path again, in the query of its
   action. *)
let login_page req =
  let action =
    match Auth.return_path req with
    | Some p ->
        "/login?" ^ Form.encode [("return", p)]
    | None ->
        "/login"
  in
  response
    (page "Sign in"
       [ El.form
           ~at:[At.method_ "post"; At.action action]
           [ El.label
               [txt "Name "; El.input ~at:[At.name "name"; At.required] ()]
           ; El.button [txt "Sign in"] ] ] )

(* The login of the demo: the name only, with no password.

   WARNING: Replace this login with a password check before you deploy. Anyone
   can sign in as any user, also as the admin.

   With passwords, give one message for an unknown user and a bad password.

   sign_in puts the user in the session, with a new session ID (no session
   fixation). The redirect goes only to Auth.return_path: a path of this site.
   Another value of return, for example https://evil.example/, can send the user
   to another site after the login: the redirect goes to / in its place. *)
let login ~users req =
  match Request.form ~max_bytes:1024 req with
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
  | Ok form -> (
    match Option.bind (Form.find "name" form) users with
    | Some u ->
        Auth.sign_in req u ;
        see_other (Option.value ~default:"/" (Auth.return_path req))
    | None ->
        Response.string ~status:Status.unauthorized "Unknown user.\n" )

(* sign_out ends the session, thus the old cookie gives no user. *)
let logout req = Auth.sign_out req ; see_other "/"

(* The user of the request. require gives the request to this handler only with
   a user. User.scheme is the scheme that found the user, here "Session". *)
let me req =
  let u = Option.get (Auth.user req) in
  Response.string
    (Printf.sprintf "id: %s\nscheme: %s\nroles: %s\nname: %s\n" (User.id u)
       (User.scheme u)
       (String.concat ", " (User.roles u))
       (name u) )

(* The admin page has the logout form, thus a token: Cache-Control: no-store. *)
let admin req =
  response (page "Admin" [El.p [txt "The admin page."]; logout_form req])

(* No token and no Cache-Control: require adds Cache-Control: private. A shared
   cache then does not keep the page of a user. For sensitive pages, set
   no-store. *)
let admin_report _req = Response.string "The report of the admins.\n"

let reports _req = Response.string "The reports of the staff.\n"

let ops _req = Response.string "The page of the ops team.\n"

(* {1 The policies} *)

(* A policy is a function of the user. This policy of the application reads a
   claim. *)
let team t u = User.claim "team" u = Some t

let routes ~users =
  let open Caravan_router in
  let only policy = [Auth.require ~policy ()] in
  handler
    [ get "/" home
    ; get "/login" login_page
    ; post "/login" (login ~users)
    ; post "/logout" logout
    ; group ~middleware:(only Auth.authenticated) "/me" [get "" me]
    ; group
        ~middleware:(only (Auth.any [Auth.role "admin"; Auth.role "staff"]))
        "/reports"
        [get "" reports]
    ; group
        ~middleware:(only (Auth.all [Auth.role "staff"; team "ops"]))
        "/ops"
        [get "" ops]
    ; group
        ~middleware:(only (Auth.role "admin"))
        "/admin"
        [get "" admin; get "/report" admin_report] ]

(* {1 The handler} *)

(* Gives [report] a line for each request with a failed scheme, for example a
   session of a user that [users] does not find. *)
let report_failure ~report next req =
  ( match Auth.failure req with
  | Some (scheme, text) ->
      report (Printf.sprintf "auth failure: %s: %s" scheme text)
  | None ->
      () ) ;
  next req

(* [make ~key ~report ~users] is the handler. [key] signs the session cookies
   and the CSRF tokens. [users id] is the user with the ID [id], if any.

   The session scheme loads the user again with [users] for each request: a
   change of roles takes effect at the next request, and a removed user loses
   the access. With no user, its challenge is a 303 to /login for the navigation
   of a browser, and 401 for the other requests.

   The order, as in the README section "Users and policies": Cross_origin, the
   session, the users, then the CSRF tokens. The login has no session yet, thus
   no token: the CSRF middleware exempts it, and Cross_origin protects it. *)
let make ~key ~report ~users =
  let session =
    Auth.Scheme.session ~login:"/login"
      ~validate:(fun u -> users (User.id u))
      ()
  in
  Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Caravan_session.middleware ~keys:[key]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Auth.middleware ~schemes:[session] ()
    ; Caravan_session.Csrf.middleware
        ~exempt:(fun r -> Request.path_only r = "/login")
        ()
    ; report_failure ~report ]
    (routes ~users)
