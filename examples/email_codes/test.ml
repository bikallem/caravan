(* The tests of the email_codes example. Each request of the header comment of
   app.ml has a test here, with the same request. Each test sets a seeded
   generator first, and runs on one domain: the simulator runs on one OS thread,
   thus the Fortuna generator is correct here. The jar of the tests keeps the
   cookies, as curl -c jar -b jar. The outbox of a test keeps the mails, and the
   reports of the application go to a list. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module J = Caravan_jobs
module App = Example_email_codes.App
module Jar = Example_support.Jar
module Page = Example_support.Page

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan email_codes example"
       (module Mirage_crypto_rng.Fortuna) )

let key = Caravan_session.Key.of_string (String.make 32 'k')

let cookie_name = "__Host-session"

(* A test application: its mails, its reports and its queue. *)
type app = {mails: string list ref; reports: string list ref; q: J.t}

(* [run ?manual ?outbox f] calls [f t app] with a new application in its own
   run. A cell holds the handler. The function of the run makes the application,
   thus each run starts with new state. The queue starts in a scope, and stops
   with no grace before its end. [outbox] (default: keep the mail) gets each
   mail before the outbox of [app]. *)
let run ?manual ?(outbox = fun _ -> ()) f =
  seed () ;
  let mails = ref [] and reports = ref [] in
  let handler = ref Handler.not_found in
  T.run ?manual
    (fun req -> !handler req)
    (fun t ->
      let a =
        App.make ~key
          ~outbox:(fun m ->
            outbox m ;
            mails := !mails @ [m] )
          ~report:(fun l -> reports := !reports @ [l])
          ~users:App.demo_users
      in
      handler := a.handler ;
      Fiber.scope (fun sw ->
          J.start a.queue sw ;
          let v = f t {mails; reports; q= a.queue} in
          J.stop ~grace:Span.zero a.queue ;
          v ) )

let request ?(headers = []) ?form meth path =
  let req = Request.make ~headers:(Headers.of_list headers) meth path in
  match form with Some f -> Request.with_form f req | None -> req

let get jar t path = Jar.fetch jar t (request GET path)

let post ?headers jar t path form =
  Jar.fetch jar t (request ?headers ~form POST path)

let token (r : T.reply) = Page.csrf_token r.body

(* [without_token r] is the body of [r] with its token replaced: each page has
   another text of the token. *)
let without_token (r : T.reply) =
  let t = token r and s = r.body in
  let n = String.length t in
  let b = Buffer.create (String.length s) in
  let rec copy i =
    if i < String.length s then
      if i + n <= String.length s && String.sub s i n = t then (
        Buffer.add_string b "TOKEN" ;
        copy (i + n) )
      else (
        Buffer.add_char b s.[i] ;
        copy (i + 1) )
  in
  copy 0 ; Buffer.contents b

let is_digit c = c >= '0' && c <= '9'

(* The code of a mail: its first run of 8 digits, as a person reads it. *)
let code_of mail =
  let rec go i =
    if i + 8 > String.length mail then failwith "no code in the mail"
    else if String.for_all is_digit (String.sub mail i 8) then
      String.sub mail i 8
    else go (i + 1)
  in
  go 0

(* [start jar t] is the GET of the page, then the POST of the address. It gives
   the token of the page and the reply of the POST. *)
let start ?(address = "ann@example.com") jar t =
  let tok = token (get jar t "/") in
  let r = post jar t "/email" [("email", address); ("_csrf", tok)] in
  (tok, r)

(* [check jar t tok code] is the POST of a code with the token [tok]. *)
let check jar t tok code = post jar t "/code" [("code", code); ("_csrf", tok)]

(* [last_code app] is the code of the last mail, after the jobs ran. *)
let last_code app =
  J.drain app.q ;
  code_of (List.nth !(app.mails) (List.length !(app.mails) - 1))

let signed_in_as address (r : T.reply) =
  contains ~sub:(Printf.sprintf "Hello, %s." address) r.body

let guest (r : T.reply) = contains ~sub:"Sign in with a code." r.body

let not_correct (r : T.reply) =
  equal status Status.ok r.status ;
  contains ~sub:"The code is not correct." r.body

let set_cookies (r : T.reply) = Headers.find_all "set-cookie" r.headers

let header name (r : T.reply) = Headers.find name r.headers

(* A code that is not [code]. *)
let other code = if code = "00000000" then "11111111" else "00000000"

let wrong left =
  Printf.sprintf "code check: Wrong {address= \"ann@example.com\"; left= %d}"
    left

let cross_site = [("Sec-Fetch-Site", "cross-site")]

let code_form = "<form method=\"post\" action=\"/code\">"

(* [at_time ~after] starts a code, waits [after] (manual time), then checks the
   code. It gives the reply of the check and the reports. *)
let at_time ~after =
  run ~manual:true (fun t app ->
      let jar = Jar.create () in
      let tok, _ = start jar t in
      let code = last_code app in
      T.advance t after ;
      let r = check jar t tok code in
      (r, !(app.reports)) )

let () =
  exit
  @@ Windtrap.run "email_codes"
       [ test "GET / makes a session: a cookie, the form and its token"
           (fun () ->
             let r = run (fun t _ -> get (Jar.create ()) t "/") in
             equal status Status.ok r.status ;
             contains ~sub:"<form method=\"post\" action=\"/email\">" r.body ;
             (* The T= line of the header comment finds the token with grep: the
                page has exactly one match, the value of _csrf. *)
             equal (list string) [token r] (Page.grep_runs 86 r.body) ;
             equal (option string) (Some "no-store") (header "cache-control" r) ;
             match set_cookies r with
             | [c] ->
                 is_true (String.starts_with ~prefix:(cookie_name ^ "=") c) ;
                 contains ~sub:"; Secure" c
             | l ->
                 failf "%d cookies" (List.length l) )
       ; test "an address with no account: the same page, and no mail"
           (fun () ->
             let ann, bob, mails, reports =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok, bob = start ~address:"bob@example.com" jar t in
                   let _, ann = start (Jar.create ()) t in
                   J.drain app.q ;
                   (* The handler started a code for bob too: a check takes one
                      of its attempts. *)
                   ignore (check jar t tok "00000000") ;
                   (ann, bob, !(app.mails), !(app.reports)) )
             in
             equal status Status.ok bob.status ;
             equal status ann.status bob.status ;
             equal string (without_token ann) (without_token bob) ;
             contains ~sub:code_form bob.body ;
             equal (list string)
               ["code check: Wrong {address= \"bob@example.com\"; left= 4}"]
               reports ;
             match mails with
             | [m] ->
                 contains ~sub:"To: ann@example.com\n" m
             | l ->
                 failf "%d mails" (List.length l) )
       ; test "POST /email of an account: the outbox gets a code of 8 digits"
           (fun () ->
             let mails =
               run (fun t app ->
                   ignore (start (Jar.create ()) t) ;
                   J.drain app.q ;
                   !(app.mails) )
             in
             match mails with
             | [m] ->
                 (* code_of takes 8 digits: a longer code makes another mail. *)
                 equal string (App.mail_text "ann@example.com" (code_of m)) m
             | l ->
                 failf "%d mails" (List.length l) )
       ; test "an incorrect code gets the form again, and a report" (fun () ->
             let r, home, reports =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok, _ = start jar t in
                   let code = last_code app in
                   let r = check jar t tok (other code) in
                   (r, get jar t "/", !(app.reports)) )
             in
             not_correct r ;
             contains ~sub:code_form r.body ;
             equal (list string) [wrong 4] reports ;
             guest home )
       ; test "the code signs the user in, with a new session ID" (fun () ->
             let r, before, after, home =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok, _ = start jar t in
                   let before = Jar.find cookie_name jar in
                   let r = check jar t tok (last_code app) in
                   let after = Jar.find cookie_name jar in
                   (r, before, after, get jar t "/") )
             in
             equal status Status.see_other r.status ;
             equal (option string) (Some "/") (header "location" r) ;
             is_true (Option.is_some before) ;
             is_false (before = after) ;
             signed_in_as "ann@example.com" home ;
             (* The second T= line: the page of the user has one match too. *)
             equal (list string) [token home] (Page.grep_runs 86 home.body) )
       ; test "the same code again: refused" (fun () ->
             let r, reports =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok, _ = start jar t in
                   let code = last_code app in
                   ignore (check jar t tok code) ;
                   (* The sign-in ended the old token: take the new one. *)
                   let tok = token (get jar t "/") in
                   let r = check jar t tok code in
                   (r, !(app.reports)) )
             in
             not_correct r ;
             equal (list string) ["code check: No_code"] reports )
       ; test "5 incorrect codes, then the correct code: refused" (fun () ->
             let r, home, reports =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok, _ = start jar t in
                   let code = last_code app in
                   for _ = 1 to 5 do
                     not_correct (check jar t tok (other code))
                   done ;
                   let r = check jar t tok code in
                   (r, get jar t "/", !(app.reports)) )
             in
             not_correct r ;
             guest home ;
             equal (list string)
               [ wrong 4
               ; wrong 3
               ; wrong 2
               ; wrong 1
               ; wrong 0
               ; "code check: No_code" ]
               reports )
       ; test "a fourth start in 10 min: no new mail, and the same page"
           (fun () ->
             let first, fourth, mails, reports =
               run (fun t app ->
                   let jar = Jar.create () in
                   let _, first = start jar t in
                   ignore (start jar t) ;
                   ignore (start jar t) ;
                   let _, fourth = start jar t in
                   J.drain app.q ;
                   (first, fourth, List.length !(app.mails), !(app.reports)) )
             in
             equal int 3 mails ;
             (* The session exists before the starts: no response sets a cookie,
                thus no cookie tells the refusal. *)
             equal (list string) [] (set_cookies first) ;
             equal (list string) [] (set_cookies fourth) ;
             equal status first.status fourth.status ;
             equal string (without_token first) (without_token fourth) ;
             equal (list string)
               ["code start refused for ann@example.com"]
               reports )
       ; test "the code works for 10 min" (fun () ->
             let r, _ = at_time ~after:(Span.s 599) in
             equal status Status.see_other r.status ;
             let r, reports = at_time ~after:(Span.s 600) in
             not_correct r ;
             equal (list string) ["code check: Expired"] reports )
       ; test "the code of one browser does not work in another session"
           (fun () ->
             let r, home, reports, own =
               run (fun t app ->
                   let a = Jar.create () and b = Jar.create () in
                   let tok_a, _ = start a t in
                   let code = last_code app in
                   let tok_b = token (get b t "/") in
                   let r = check b t tok_b code in
                   let home = get b t "/" in
                   let reports = !(app.reports) in
                   (r, home, reports, check a t tok_a code) )
             in
             not_correct r ;
             guest home ;
             equal (list string) ["code check: No_code"] reports ;
             (* The code was correct: it works in its own session. *)
             equal status Status.see_other own.status )
       ; test "POST /email without the token: 403, and no mail" (fun () ->
             let r, mails =
               run (fun t app ->
                   let jar = Jar.create () in
                   ignore (get jar t "/") ;
                   let r = post jar t "/email" [("email", "ann@example.com")] in
                   J.drain app.q ; (r, !(app.mails)) )
             in
             equal status Status.forbidden r.status ;
             equal (list string) [] mails )
       ; test "POST /code without the token: 403, also with the correct code"
           (fun () ->
             let r, home =
               run (fun t app ->
                   let jar = Jar.create () in
                   ignore (start jar t) ;
                   let r = post jar t "/code" [("code", last_code app)] in
                   (r, get jar t "/") )
             in
             equal status Status.forbidden r.status ;
             guest home )
       ; test "POST /email or /code with no session: 403" (fun () ->
             let e, c =
               run (fun t _ ->
                   let tok = token (get (Jar.create ()) t "/") in
                   let jar = Jar.create () in
                   ( post jar t "/email"
                       [("email", "ann@example.com"); ("_csrf", tok)]
                   , check jar t tok "12345678" ) )
             in
             equal status Status.forbidden e.status ;
             equal status Status.forbidden c.status )
       ; test "a POST from another site: 403, also with the token" (fun () ->
             let e, c, mails =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok = token (get jar t "/") in
                   let e =
                     post ~headers:cross_site jar t "/email"
                       [("email", "ann@example.com"); ("_csrf", tok)]
                   in
                   let tok, _ = start jar t in
                   let code = last_code app in
                   let c =
                     post ~headers:cross_site jar t "/code"
                       [("code", code); ("_csrf", tok)]
                   in
                   (e, c, List.length !(app.mails)) )
             in
             equal status Status.forbidden e.status ;
             equal status Status.forbidden c.status ;
             equal int 1 mails )
       ; test "the logout needs the token of the new page, then a guest"
           (fun () ->
             let old, r, home =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok, _ = start jar t in
                   ignore (check jar t tok (last_code app)) ;
                   (* The sign-in ended the token of the old page. *)
                   let old = post jar t "/logout" [("_csrf", tok)] in
                   let tok = token (get jar t "/") in
                   let r = post jar t "/logout" [("_csrf", tok)] in
                   (old, r, get jar t "/") )
             in
             equal status Status.forbidden old.status ;
             equal status Status.see_other r.status ;
             ( match set_cookies r with
             | [c] ->
                 contains ~sub:"Max-Age=0" c
             | l ->
                 failf "%d cookies" (List.length l) ) ;
             guest home )
       ; test "11 POSTs with no session: 10 get 403, the eleventh gets 429"
           (fun () ->
             (* [burst t] sends the 11 POSTs of the curl line. *)
             let burst t =
               List.init 11 (fun i ->
                   let path = Printf.sprintf "/code?n=%d" (i + 1) in
                   T.fetch t (request POST path) )
             in
             let expected =
               List.init 10 (fun _ -> Status.forbidden)
               @ [Status.too_many_requests]
             in
             let statuses = List.map (fun (r : T.reply) -> r.status) in
             run ~manual:true (fun t _ ->
                 let first = burst t in
                 equal (list status) expected (statuses first) ;
                 equal (option string) (Some "6")
                   (header "retry-after" (List.nth first 10)) ;
                 (* After 60 s, the bucket is full again. The two routes take
                    tokens of one bucket. *)
                 T.advance t (Span.s 60) ;
                 let mixed =
                   List.init 11 (fun i ->
                       let path = if i mod 2 = 0 then "/email" else "/code" in
                       T.fetch t (request POST path) )
                 in
                 equal (list status) expected (statuses mixed) ) )
       ; test "the address is in lowercase, with no spaces at its ends"
           (fun () ->
             let mails, home =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok, _ = start ~address:"  Ann@Example.COM " jar t in
                   ignore (check jar t tok (last_code app)) ;
                   (!(app.mails), get jar t "/") )
             in
             equal int 1 (List.length mails) ;
             contains ~sub:"To: ann@example.com\n" (List.hd mails) ;
             signed_in_as "ann@example.com" home )
       ; test "an address that is not valid: 400, and no mail" (fun () ->
             let statuses, mails =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok = token (get jar t "/") in
                   let send address =
                     let form = [("email", address); ("_csrf", tok)] in
                     (post jar t "/email" form).status
                   in
                   (* 321 bytes: Code.start takes at most 320. *)
                   let long = String.make 309 'a' ^ "@example.com" in
                   let l =
                     List.map send
                       [ "ann.example.com"
                       ; "ann@ann@example.com"
                       ; "@example.com"
                       ; "ann@"
                       ; "ann smith@example.com"
                       ; "ann@example.com\nBcc:eve@example.com"
                       ; "ann@example.com\127"
                       ; long ]
                   in
                   J.drain app.q ; (l, !(app.mails)) )
             in
             equal (list status)
               (List.init 8 (fun _ -> Status.bad_request))
               statuses ;
             equal (list string) [] mails )
       ; test "an address with a character of header text: 400, and no mail"
           (fun () ->
             let statuses, mails =
               run (fun t app ->
                   let jar = Jar.create () in
                   let tok = token (get jar t "/") in
                   let send address =
                     let form = [("email", address); ("_csrf", tok)] in
                     (post jar t "/email" form).status
                   in
                   (* One address for each character: ann,@example.com and so
                      on. *)
                   let one c =
                     send ("ann" ^ String.make 1 c ^ "@example.com")
                   in
                   let l =
                     List.map one [','; ';'; '<'; '>'; '"'; '('; ')'; '\\']
                   in
                   J.drain app.q ; (l, !(app.mails)) )
             in
             equal (list status)
               (List.init 8 (fun _ -> Status.bad_request))
               statuses ;
             equal (list string) [] mails )
       ; test "the limits of Code are for each address, not for each client"
           (fun () ->
             let fourth, ben, reports, mails =
               run (fun t app ->
                   let a = Jar.create () in
                   for _ = 1 to 3 do
                     ignore (start a t)
                   done ;
                   (* A new client gets no new code for the address. *)
                   let _, fourth = start (Jar.create ()) t in
                   J.drain app.q ;
                   let reports = !(app.reports) in
                   (* Another address gets a code in the first client. *)
                   let _, ben = start ~address:"ben@example.com" a t in
                   J.drain app.q ;
                   (fourth, ben, reports, !(app.mails)) )
             in
             equal status Status.ok fourth.status ;
             equal status Status.ok ben.status ;
             equal (list string)
               ["code start refused for ann@example.com"]
               reports ;
             equal int 4 (List.length mails) ;
             contains ~sub:"To: ben@example.com\n" (List.nth mails 3) )
       ; test "the eleventh check of an address in one hour: Too_many"
           (fun () ->
             let r, reports =
               run ~manual:true (fun t app ->
                   let jar = Jar.create () in
                   (* Each code takes 5 wrong codes, thus two codes take 10
                      checks. After 60 s, the bucket of the client is full
                      again: it does not refuse a POST. *)
                   let wrong_codes () =
                     let tok, _ = start jar t in
                     for _ = 1 to 5 do
                       ignore (check jar t tok "00000000")
                     done ;
                     T.advance t (Span.s 60)
                   in
                   wrong_codes () ;
                   wrong_codes () ;
                   let tok, _ = start jar t in
                   let r = check jar t tok (last_code app) in
                   (r, !(app.reports)) )
             in
             not_correct r ;
             equal int 11 (List.length reports) ;
             let last = List.nth reports 10 in
             is_true
               (String.starts_with
                  ~prefix:
                    "code check: Too_many {address= \"ann@example.com\"; wait="
                  last ) )
       ; test "GET / has a bucket of 20: the 21st GET gets 429, and no session"
           (fun () ->
             run ~manual:true (fun t app ->
                 let jar = Jar.create () in
                 let tok = token (get jar t "/") in
                 for _ = 2 to 20 do
                   equal status Status.ok (get jar t "/").status
                 done ;
                 let r = get (Jar.create ()) t "/" in
                 equal status Status.too_many_requests r.status ;
                 equal (option string) (Some "3") (header "retry-after" r) ;
                 equal (list string) [] (set_cookies r) ;
                 (* The POST routes have their own bucket. *)
                 let e =
                   post jar t "/email"
                     [("email", "ann@example.com"); ("_csrf", tok)]
                 in
                 equal status Status.ok e.status ;
                 J.drain app.q ;
                 equal int 1 (List.length !(app.mails)) ;
                 (* One more GET each 3 s. *)
                 T.advance t (Span.s 3) ;
                 equal status Status.ok (get jar t "/").status ) )
       ; test "a mail that fails: 5 attempts, then one report with no code"
           (fun () ->
             let attempts = ref 0 in
             let outbox _ = incr attempts ; failwith "no mail server" in
             let reports =
               run ~outbox (fun t app ->
                   ignore (start (Jar.create ()) t) ;
                   J.drain app.q ;
                   !(app.reports) )
             in
             equal int 5 !attempts ;
             equal (list string) ["code mail: job code-mail failed"] reports )
       ]
