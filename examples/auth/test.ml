(* The tests of the auth example. Each request of the header comment of app.ml
   has a test here, with the same request. Each test sets a seeded generator
   first, and runs on one domain: the simulator runs on one OS thread, thus the
   Fortuna generator is correct here. The jar of the tests keeps the cookies, as
   curl -c jar -b jar. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_auth.App
module Jar = Example_support.Jar
module User = Caravan_auth.User

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan auth example"
       (module Mirage_crypto_rng.Fortuna) )

let key = Caravan_session.Key.of_string (String.make 32 'k')

let cookie_name = "__Host-session"

(* [run ~report ~users f] calls [f] with a new application in its own run.
   [users] is the table of the users (the demo users by default). *)
let run ?(report = ignore) ?(users = ref App.demo_users) f =
  seed () ;
  let users id = List.assoc_opt id !users in
  T.run (App.make ~key ~report ~users) f

let request ?(headers = []) ?form meth path =
  let req = Request.make ~headers:(Headers.of_list headers) meth path in
  match form with Some f -> Request.with_form f req | None -> req

let get ?headers jar t path = Jar.fetch jar t (request ?headers GET path)

let post ?headers jar t path form =
  Jar.fetch jar t (request ?headers ~form POST path)

(* The request of curl -c jar -b jar -d name=ada $S/login. *)
let login ?(path = "/login") jar t name = post jar t path [("name", name)]

(* [signed_in name t] is a jar with the session of [name]. *)
let signed_in name t =
  let jar = Jar.create () in
  ignore (login jar t name) ;
  jar

(* The CSRF token of a page. *)
let token (r : T.reply) = Example_support.Page.csrf_token r.body

let header name (r : T.reply) = Headers.find name r.headers

let location r = header "location" r

let set_cookies (r : T.reply) = Headers.find_all "set-cookie" r.headers

let navigate = [("Sec-Fetch-Mode", "navigate")]

let () =
  exit
  @@ Windtrap.run "auth"
       [ test "/admin with no user, in a navigation: 303 to the login page"
           (fun () ->
             let r =
               run (fun t -> get ~headers:navigate (Jar.create ()) t "/admin")
             in
             equal status Status.see_other r.status ;
             equal (option string) (Some "/login?return=/admin") (location r) )
       ; test "/admin with no user, not a navigation: 401" (fun () ->
             let r = run (fun t -> get (Jar.create ()) t "/admin") in
             equal status Status.unauthorized r.status )
       ; test "the login of ada goes to the return path, then /admin: 200"
           (fun () ->
             let r, admin =
               run (fun t ->
                   let jar = Jar.create () in
                   let r = login ~path:"/login?return=/admin" jar t "ada" in
                   (r, get jar t "/admin") )
             in
             equal status Status.see_other r.status ;
             equal (option string) (Some "/admin") (location r) ;
             equal status Status.ok admin.status ;
             contains ~sub:"The admin page." admin.body )
       ; test "the /admin page has the logout form: no-store" (fun () ->
             let r = run (fun t -> get (signed_in "ada" t) t "/admin") in
             contains ~sub:"<form method=\"post\" action=\"/logout\">" r.body ;
             equal (option string) (Some "no-store") (header "cache-control" r) ;
             (* The T= line of the header comment: the page has one run of 86
                base64url characters, and it is the token. *)
             equal (list string)
               [token r]
               (Example_support.Page.grep_runs 86 r.body) )
       ; test "/admin/report has no token: Cache-Control: private" (fun () ->
             let r = run (fun t -> get (signed_in "ada" t) t "/admin/report") in
             equal status Status.ok r.status ;
             equal string "The report of the admins.\n" r.body ;
             equal (option string) (Some "private") (header "cache-control" r) )
       ; test "/me gives the ID, the scheme, the roles and the name claim"
           (fun () ->
             let r = run (fun t -> get (signed_in "ada" t) t "/me") in
             equal status Status.ok r.status ;
             equal string
               "id: ada\n\
                scheme: Session\n\
                roles: admin, staff\n\
                name: Ada Lovelace\n"
               r.body )
       ; test "/ops: all of the role staff and the team ops" (fun () ->
             let ada, bob =
               run (fun t ->
                   ( get (signed_in "ada" t) t "/ops"
                   , get (signed_in "bob" t) t "/ops" ) )
             in
             equal status Status.ok ada.status ;
             equal status Status.forbidden bob.status )
       ; test "a return path to another site goes to /" (fun () ->
             let paths =
               [ "/login?return=https://evil.example/"
               ; "/login?return=//evil.example/"
               ; "/login?return=/%5Cevil.example/"
               ; "/login" ]
             in
             let locations =
               run (fun t ->
                   List.map
                     (fun path -> location (login ~path (Jar.create ()) t "ada"))
                     paths )
             in
             equal
               (list (option string))
               [Some "/"; Some "/"; Some "/"; Some "/"]
               locations )
       ; test "bob: /admin 403, /reports 200, /ops 403" (fun () ->
             let login, admin, reports, ops =
               run (fun t ->
                   let jar = Jar.create () in
                   let r = login jar t "bob" in
                   ( r
                   , get jar t "/admin"
                   , get jar t "/reports"
                   , get jar t "/ops" ) )
             in
             equal (option string) (Some "/") (location login) ;
             equal status Status.forbidden admin.status ;
             equal status Status.ok reports.status ;
             equal status Status.forbidden ops.status )
       ; test "/reports: any of the roles admin and staff" (fun () ->
             let r = run (fun t -> get (signed_in "ada" t) t "/reports") in
             equal status Status.ok r.status )
       ; test "a guest gets 401 on /me, /reports and /ops" (fun () ->
             let statuses =
               run (fun t ->
                   List.map
                     (fun p -> (get (Jar.create ()) t p).status)
                     ["/me"; "/reports"; "/ops"] )
             in
             equal (list status)
               [Status.unauthorized; Status.unauthorized; Status.unauthorized]
               statuses )
       ; test "POST /logout without the token: 403, the user stays" (fun () ->
             let r, me =
               run (fun t ->
                   let jar = signed_in "ada" t in
                   let r = Jar.fetch jar t (request POST "/logout") in
                   (r, get jar t "/me") )
             in
             equal status Status.forbidden r.status ;
             equal status Status.ok me.status )
       ; test "POST /logout with the token: no user, and the old cookie neither"
           (fun () ->
             let r, kept, me, old =
               run (fun t ->
                   let jar = signed_in "ada" t in
                   let old = Option.get (Jar.find cookie_name jar) in
                   let tok = token (get jar t "/admin") in
                   let r = post jar t "/logout" [("_csrf", tok)] in
                   let cookie = [("Cookie", cookie_name ^ "=" ^ old)] in
                   ( r
                   , Jar.find cookie_name jar
                   , get jar t "/me"
                   , T.fetch t (request ~headers:cookie GET "/me") ) )
             in
             equal status Status.see_other r.status ;
             equal (option string) (Some "/") (location r) ;
             equal (option string) None kept ;
             equal status Status.unauthorized me.status ;
             equal status Status.unauthorized old.status )
       ; test "a login gives a new session ID: the old cookie has no user"
           (fun () ->
             let me, old =
               run (fun t ->
                   let jar = signed_in "ada" t in
                   let first = Option.get (Jar.find cookie_name jar) in
                   ignore (login jar t "bob") ;
                   let cookie = [("Cookie", cookie_name ^ "=" ^ first)] in
                   ( get jar t "/me"
                   , T.fetch t (request ~headers:cookie GET "/me") ) )
             in
             contains ~sub:"id: bob\n" me.body ;
             equal status Status.unauthorized old.status )
       ; test "an unknown user: 401 and no cookie" (fun () ->
             let r = run (fun t -> login (Jar.create ()) t "eve") in
             equal status Status.unauthorized r.status ;
             equal (list string) [] (set_cookies r) )
       ; test "a login from another site: 403 and no cookie" (fun () ->
             let r =
               run (fun t ->
                   post
                     ~headers:[("Sec-Fetch-Site", "cross-site")]
                     (Jar.create ()) t "/login"
                     [("name", "ada")] )
             in
             equal status Status.forbidden r.status ;
             equal (list string) [] (set_cookies r) )
       ; test "the login page sends the return path again, a local path only"
           (fun () ->
             let local, other =
               run (fun t ->
                   ( get (Jar.create ()) t "/login?return=/admin"
                   , get (Jar.create ()) t "/login?return=https://evil.example/"
                   ) )
             in
             contains ~sub:"action=\"/login?return=%2Fadmin\"" local.body ;
             contains ~sub:"action=\"/login\"" other.body )
       ; test "/ shows the name of the user, or a guest" (fun () ->
             let user, guest =
               run (fun t ->
                   (get (signed_in "ada" t) t "/", get (Jar.create ()) t "/") )
             in
             contains ~sub:"Hello, Ada Lovelace." user.body ;
             contains ~sub:"action=\"/logout\"" user.body ;
             contains ~sub:"Hello, guest." guest.body ;
             contains ~sub:"href=\"/login\"" guest.body )
       ; test "a change of roles takes effect at the next request" (fun () ->
             let users = ref App.demo_users in
             let before, after =
               run ~users (fun t ->
                   let jar = signed_in "bob" t in
                   let before = get jar t "/admin" in
                   users :=
                     [("bob", User.make ~roles:["admin"] "bob")]
                     @ List.remove_assoc "bob" !users ;
                   (before, get jar t "/admin") )
             in
             equal status Status.forbidden before.status ;
             equal status Status.ok after.status )
       ; test "a removed user: 401, and a report of the failure" (fun () ->
             let users = ref App.demo_users in
             let reports = ref [] in
             let report l = reports := l :: !reports in
             let r =
               run ~report ~users (fun t ->
                   let jar = signed_in "bob" t in
                   ignore (get jar t "/me") ;
                   equal (list string) [] !reports ;
                   users := List.remove_assoc "bob" !users ;
                   get jar t "/me" )
             in
             equal status Status.unauthorized r.status ;
             equal (list string)
               ["auth failure: Session: the user is not valid"]
               !reports ) ]
