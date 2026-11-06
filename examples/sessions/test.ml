(* The tests of the sessions example. Each request of the header comment of
   app.ml has a test here, with the same request. Each test sets a seeded
   generator first, and runs on one domain: the simulator runs on one OS thread,
   thus the Fortuna generator is correct here. The jar of the tests keeps the
   cookies, as curl -c jar -b jar. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_sessions.App
module Jar = Example_support.Jar
module Page = Example_support.Page
module Session = Caravan_session

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan sessions example"
       (module Mirage_crypto_rng.Fortuna) )

let key = Session.Key.of_string (String.make 32 'k')

let cookie_name = "__Host-session"

(* [run f] calls [f] with a new application in its own run. *)
let run f =
  seed () ;
  T.run (App.make ~key) f

(* [request ~headers ~form meth path] is a request. [form] is an urlencoded
   body, as curl -d sends it. *)
let request ?(headers = []) ?form meth path =
  let req = Request.make ~headers:(Headers.of_list headers) meth path in
  match form with Some f -> Request.with_form f req | None -> req

let get jar t path = Jar.fetch jar t (request GET path)

let post ?headers jar t path form =
  Jar.fetch jar t (request ?headers ~form POST path)

(* The request of curl -c jar -b jar -d name=ada $S/login. *)
let login jar t name = post jar t "/login" [("name", name)]

(* [upload jar t fields] is a POST of the multipart [fields], as curl -F sends
   it. *)
let upload jar t fields =
  Jar.fetch jar t (Request.with_multipart fields (Request.make POST "/upload"))

let file =
  { Multipart.name= "file"
  ; filename= Some "a.txt"
  ; content_type= Some "text/plain"
  ; data= "hi" }

let csrf_field t =
  {Multipart.name= "_csrf"; filename= None; content_type= None; data= t}

(* The CSRF token of a page. *)
let token (r : T.reply) = Page.csrf_token r.body

(* The path of the unsubscribe link of a page. *)
let link (r : T.reply) = "/unsubscribe?t=" ^ Page.after "unsubscribe?t=" r.body

let count ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let header name (r : T.reply) = Headers.find name r.headers

let set_cookies (r : T.reply) = Headers.find_all "set-cookie" r.headers

(* The ID of a session cookie: the part before the signature. *)
let id_of value = String.sub value 0 (String.index value '.')

(* [signed_in jar t] signs in ada, and gives the token of the page. *)
let signed_in jar t =
  ignore (login jar t "ada") ;
  token (get jar t "/")

let cross_site = [("Sec-Fetch-Site", "cross-site")]

(* [set_time t start minutes] sets the wall clock of the simulator to [minutes]
   after [start]. *)
let set_time t start minutes =
  let at = Weft.Time.add Posix start (Weft.Span.s (minutes * 60)) in
  Weft_sim.set_posix (T.sim t) (Option.get at)

(* [visits steps] signs in ada, then sends GET [path] at each [(minutes, path)]
   of [steps], [minutes] after the login. It tells if the last page has the
   user. *)
let visits steps =
  run (fun t ->
      let jar = Jar.create () in
      ignore (login jar t "ada") ;
      let start = Weft.Time.now Posix in
      List.fold_left
        (fun _ (minutes, path) ->
          set_time t start minutes ;
          let r = get jar t path in
          count ~sub:"Hello, ada." r.body > 0 )
        false steps )

let () =
  exit
  @@ Windtrap.run "sessions"
       [ test "GET /about makes no session: no cookie" (fun () ->
             let r = run (fun t -> get (Jar.create ()) t "/about") in
             equal status Status.ok r.status ;
             equal (list string) [] (set_cookies r) )
       ; test "GET / of a guest: the login form, and no cookie" (fun () ->
             let r = run (fun t -> get (Jar.create ()) t "/") in
             contains ~sub:"Hello, guest." r.body ;
             contains ~sub:"<form method=\"post\" action=\"/login\">" r.body ;
             equal (list string) [] (set_cookies r) )
       ; test "POST /login sets the __Host-session cookie, with no-store"
           (fun () ->
             let r = run (fun t -> login (Jar.create ()) t "ada") in
             equal status Status.see_other r.status ;
             equal (option string) (Some "/") (header "location" r) ;
             equal (option string) (Some "no-store") (header "cache-control" r) ;
             match set_cookies r with
             | [c] ->
                 is_true (String.starts_with ~prefix:(cookie_name ^ "=") c) ;
                 contains ~sub:"; Path=/; Secure; HttpOnly; SameSite=Lax" c
             | l ->
                 failf "%d cookies" (List.length l) )
       ; test "with the jar, GET / shows the user" (fun () ->
             let r =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (login jar t "ada") ;
                   get jar t "/" )
             in
             equal status Status.ok r.status ;
             contains ~sub:"Hello, ada." r.body )
       ; test "the page has one token in its three forms, and no-store"
           (fun () ->
             let r =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (login jar t "ada") ;
                   get jar t "/" )
             in
             let t = token r in
             equal int 3
               (count
                  ~sub:(Printf.sprintf "name=\"_csrf\" value=\"%s\"" t)
                  r.body ) ;
             equal (option string) (Some "no-store") (header "cache-control" r) )
       ; test "POST /note with the token: 200, and / shows the note" (fun () ->
             let r, page =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   let r =
                     post jar t "/note" [("text", "hello"); ("_csrf", tok)]
                   in
                   (r, get jar t "/") )
             in
             equal status Status.ok r.status ;
             equal string "Note saved.\n" r.body ;
             contains ~sub:"Note: hello" page.body )
       ; test "POST /note with an empty text removes the note" (fun () ->
             let r, page =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   ignore
                     (post jar t "/note" [("text", "hello"); ("_csrf", tok)]) ;
                   let r = post jar t "/note" [("text", ""); ("_csrf", tok)] in
                   (r, get jar t "/") )
             in
             equal string "Note removed.\n" r.body ;
             contains ~sub:"Note: (none)" page.body )
       ; test "POST /note without the token: 403" (fun () ->
             let r =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (signed_in jar t) ;
                   post jar t "/note" [("text", "hello")] )
             in
             equal status Status.forbidden r.status )
       ; test "POST /note with no session: 403" (fun () ->
             let r =
               run (fun t ->
                   let tok = signed_in (Jar.create ()) t in
                   post (Jar.create ()) t "/note"
                     [("text", "hello"); ("_csrf", tok)] )
             in
             equal status Status.forbidden r.status )
       ; test "a POST from another site: 403, also for the login" (fun () ->
             let login_reply, note =
               run (fun t ->
                   let r =
                     post ~headers:cross_site (Jar.create ()) t "/login"
                       [("name", "ada")]
                   in
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   ( r
                   , post ~headers:cross_site jar t "/note"
                       [("text", "hello"); ("_csrf", tok)] ) )
             in
             equal status Status.forbidden login_reply.status ;
             equal (list string) [] (set_cookies login_reply) ;
             equal status Status.forbidden note.status )
       ; test "the exempt paths: a POST from another site gets 403" (fun () ->
             let up, unsub =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   let l = link (get jar t "/") in
                   let up =
                     Request.make
                       ~headers:(Headers.of_list cross_site)
                       POST "/upload"
                     |> Request.with_multipart [csrf_field tok; file]
                   in
                   ( Jar.fetch jar t up
                   , T.fetch t (request ~headers:cross_site POST l) ) )
             in
             equal status Status.forbidden up.status ;
             equal status Status.forbidden unsub.status )
       ; test "POST /login with a bad name: 400 and no cookie" (fun () ->
             let replies =
               run (fun t ->
                   List.map
                     (fun name -> login (Jar.create ()) t name)
                     ["Ada!"; ""; String.make 21 'a'] )
             in
             List.iter
               (fun (r : T.reply) ->
                 equal status Status.bad_request r.status ;
                 equal (list string) [] (set_cookies r) )
               replies )
       ; test "POST /login with a name of 20 letters: the user" (fun () ->
             let name = String.make 20 'a' in
             let r =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (login jar t name) ;
                   get jar t "/" )
             in
             contains ~sub:(Printf.sprintf "Hello, %s." name) r.body )
       ; test "a multipart upload with the token field: 200, the page lists it"
           (fun () ->
             let r, page =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   let r = upload jar t [csrf_field tok; file] in
                   (r, get jar t "/") )
             in
             equal status Status.ok r.status ;
             equal string "a.txt: 2 bytes\n" r.body ;
             contains ~sub:"<li>a.txt: 2 bytes</li>" page.body )
       ; test "the upload table is for one user" (fun () ->
             let page =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   ignore (upload jar t [csrf_field tok; file]) ;
                   let other = Jar.create () in
                   ignore (login other t "bob") ;
                   get other t "/" )
             in
             contains ~sub:"Hello, bob." page.body ;
             not_contains ~sub:"a.txt" page.body )
       ; test "a multipart upload without the token: 403" (fun () ->
             let r =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (signed_in jar t) ;
                   upload jar t [file] )
             in
             equal status Status.forbidden r.status )
       ; test "a multipart upload with a bad token or no session: 403"
           (fun () ->
             let bad, no_session =
               run (fun t ->
                   let other = signed_in (Jar.create ()) t in
                   let jar = Jar.create () in
                   ignore (signed_in jar t) ;
                   ( upload jar t [csrf_field other; file]
                   , upload (Jar.create ()) t [csrf_field other; file] ) )
             in
             equal status Status.forbidden bad.status ;
             equal status Status.forbidden no_session.status ;
             equal (list string) [] (set_cookies no_session) )
       ; test "the GET of the link shows a form and changes nothing" (fun () ->
             let l, r, page =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (login jar t "ada") ;
                   let l = link (get jar t "/") in
                   let r = Jar.fetch (Jar.create ()) t (request GET l) in
                   (l, r, get jar t "/") )
             in
             equal status Status.ok r.status ;
             contains ~sub:"Stop the mail to ada?" r.body ;
             (* The form posts to the signed link. *)
             contains
               ~sub:(Printf.sprintf {|<form method="post" action="%s">|} l)
               r.body ;
             contains ~sub:"Mail: on." page.body )
       ; test "the POST of the link, with no session, unsubscribes" (fun () ->
             let r, page =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (login jar t "ada") ;
                   let l = link (get jar t "/") in
                   let r = Jar.fetch (Jar.create ()) t (request POST l) in
                   (r, get jar t "/") )
             in
             equal status Status.ok r.status ;
             equal string "ada gets no more mail.\n" r.body ;
             contains ~sub:"Mail: off." page.body )
       ; test "a link signed for another purpose: 400" (fun () ->
             let share = Session.Signed.sign ~purpose:"share" key "ada" in
             let path = "/unsubscribe?" ^ Form.encode [("t", share)] in
             let g, p =
               run (fun t ->
                   (T.fetch t (request GET path), T.fetch t (request POST path)) )
             in
             equal status Status.bad_request g.status ;
             equal status Status.bad_request p.status )
       ; test "POST /logout with the token, then the old cookie: a guest"
           (fun () ->
             let r, kept, old =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   let old = Option.get (Jar.find cookie_name jar) in
                   let r = post jar t "/logout" [("_csrf", tok)] in
                   let kept = Jar.find cookie_name jar in
                   let cookie = [("Cookie", cookie_name ^ "=" ^ old)] in
                   (r, kept, T.fetch t (request ~headers:cookie GET "/")) )
             in
             equal status Status.see_other r.status ;
             ( match set_cookies r with
             | [c] ->
                 contains ~sub:"Max-Age=0" c
             | l ->
                 failf "%d cookies" (List.length l) ) ;
             equal (option string) None kept ;
             contains ~sub:"Hello, guest." old.body )
       ; test "POST /logout without the token: 403, the session stays"
           (fun () ->
             let r, page =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (signed_in jar t) ;
                   let r = post jar t "/logout" [] in
                   (r, get jar t "/") )
             in
             equal status Status.forbidden r.status ;
             contains ~sub:"Hello, ada." page.body )
       ; test
           "a second login of ada: a new ID, the old ID ends, the note and the \
            uploads stay" (fun () ->
             let first, second, old, page =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   ignore
                     (post jar t "/note" [("text", "hello"); ("_csrf", tok)]) ;
                   (* The table of the uploads has the user name as key. *)
                   ignore (upload jar t [csrf_field tok; file]) ;
                   let first = Option.get (Jar.find cookie_name jar) in
                   ignore (login jar t "ada") ;
                   let second = Option.get (Jar.find cookie_name jar) in
                   let cookie = [("Cookie", cookie_name ^ "=" ^ first)] in
                   ( first
                   , second
                   , T.fetch t (request ~headers:cookie GET "/")
                   , get jar t "/" ) )
             in
             is_false (id_of first = id_of second) ;
             contains ~sub:"Hello, guest." old.body ;
             contains ~sub:"Note: hello" page.body ;
             contains ~sub:"<li>a.txt: 2 bytes</li>" page.body )
       ; test "a login after another user: a new session, with no note"
           (fun () ->
             let first, second, page =
               run (fun t ->
                   let jar = Jar.create () in
                   let tok = signed_in jar t in
                   ignore
                     (post jar t "/note" [("text", "secret"); ("_csrf", tok)]) ;
                   let first = Option.get (Jar.find cookie_name jar) in
                   ignore (login jar t "bob") ;
                   let second = Option.get (Jar.find cookie_name jar) in
                   (first, second, get jar t "/") )
             in
             is_false (id_of first = id_of second) ;
             contains ~sub:"Hello, bob." page.body ;
             contains ~sub:"Note: (none)" page.body )
       ; test "a session ends at most 20 min after its last request" (fun () ->
             is_true (visits [(19, "/")]) ;
             is_false (visits [(21, "/")]) )
       ; test "a session ends 8 h after the login, also with requests"
           (fun () ->
             (* A GET each 11 min moves the idle limit. *)
             let steps = List.init 43 (fun i -> (11 * (i + 1), "/")) in
             is_true (visits (steps @ [(479, "/")])) ;
             is_false (visits (steps @ [(481, "/")])) )
       ; test "GET /about moves the limit only after half of it" (fun () ->
             (* /about changes no data: the middleware moves the limit with the
                touch of the store. *)
             is_false (visits [(9, "/about"); (21, "/")]) ;
             is_true (visits [(11, "/about"); (29, "/")]) )
       ; test "the link works after a year" (fun () ->
             let r =
               run (fun t ->
                   let jar = Jar.create () in
                   ignore (login jar t "ada") ;
                   let l = link (get jar t "/") in
                   set_time t (Weft.Time.now Posix) (365 * 24 * 60) ;
                   T.fetch t (request GET l) )
             in
             contains ~sub:"Stop the mail to ada?" r.body )
       ; test "the store: update and touch after a remove do nothing" (fun () ->
             let module S = App.Table_store in
             let s = S.create () in
             let expires = Weft.Time.posix_of_unix_s 100. in
             S.create_row s "k" "v" ~expires ;
             equal bool true (S.remove s "k") ;
             equal bool false (S.remove s "k") ;
             S.update s "k" "w" ~expires ;
             S.touch s "k" ~expires ;
             is_true (S.find s "k" = None) ) ]
