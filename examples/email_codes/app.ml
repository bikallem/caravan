(* Sign-in with a code by email: the user gives an address, gets a code of 8
   digits, and types it in the same browser.

   caravan sends no mail. A job of the queue "sends" each code to an outbox. The
   outbox of main.ml prints the mail: it is a stand-in for development only. A
   real application sends the mail with SMTP, or with the HTTP API of a mail
   service through caravan.client.

   Run dune exec examples/email_codes/main.exe, then open http://localhost:8080/
   in a browser. The session cookie has the name __Host-session, thus it has
   Secure. Chrome and Firefox keep a Secure cookie over http for localhost, but
   some browsers do not keep it for 127.0.0.1.

   With curl, first set this shell variable:

   S=127.0.0.1:8080

   The GET of the page makes a session, and the form has its CSRF token. curl
   keeps the session cookie in the file jar. Copy the token to T:

   curl -i -c jar -b jar $S/

   T=$(curl -s -b jar $S/ | grep -o '[[:alnum:]_-]\{86\}')

   An address with no account gets the same page as an account, but no mail:

   curl -i -b jar -d "email=bob@example.com&_csrf=$T" $S/email

   ann@example.com and ben@example.com have accounts. The program prints the
   mail with the code:

   curl -i -b jar -d "email=ann@example.com&_csrf=$T" $S/email

   An incorrect code gets the form again. Copy the code of the mail to C, then
   sign in. The sign-in gives the session a new ID, thus curl keeps the new
   cookie:

   curl -i -b jar -d "code=00000000&_csrf=$T" $S/code

   curl -i -c jar -b jar -d "code=$C&_csrf=$T" $S/code

   curl -i -b jar $S/

   A POST without the token gets 403, and the program sends no mail:

   curl -i -b jar -d email=ann@example.com $S/email

   The sign-in ended the old token. Copy the token of the new page to T, then
   sign out:

   T=$(curl -s -b jar $S/ | grep -o '[[:alnum:]_-]\{86\}')

   curl -i -c jar -b jar -d "_csrf=$T" $S/logout

   Each client has a bucket of 10 POSTs to /email and /code, and one more each 6
   s. Wait 60 s, thus the bucket is full again. Then curl sends 11 POSTs with no
   session: 10 get 403, and the eleventh gets 429:

   curl -s -o /dev/null -w '%{http_code}\n' -X POST "$S/code?n=[1-11]"

   Each client also has a bucket of 20 GETs of /, and one more each 3 s. *)

open Weft
open Caravan
open Caravan_html
open Caravan_middleware
module Auth = Caravan_auth
module Code = Caravan_auth.Code
module J = Caravan_jobs

(* {1 The pages} *)

let page title children =
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head [El.meta ~at:[At.v "charset" "utf-8"] (); El.title title]
        ; El.body children ] ]

(* A POST form with the CSRF token. Csrf.token makes a session if the request
   has none, and gives the response Cache-Control: no-store. *)
let form req action children =
  El.form
    ~at:[At.method_ "post"; At.action action]
    ( El.input
        ~at:
          [ At.type_ "hidden"
          ; At.name "_csrf"
          ; At.value (Caravan_session.Csrf.token req) ]
        ()
    :: children )

let address_form req =
  form req "/email"
    [ El.label
        [ txt "Email "
        ; El.input ~at:[At.type_ "email"; At.name "email"; At.required] () ]
    ; El.button [txt "Send me a code"] ]

let code_form req =
  form req "/code"
    [ El.label
        [ txt "Code "
        ; El.input
            ~at:
              [ At.name "code"
              ; At.v "inputmode" "numeric"
              ; At.v "autocomplete" "one-time-code"
              ; At.required ]
            () ]
    ; El.button [txt "Sign in"] ]

(* The page after POST /email. It is the same for each address and for each
   result of Code.start: the page does not tell which addresses have an
   account. *)
let code_page req =
  response
    (page "Your code"
       [ El.p
           [ txt
               "If an account has this address, we sent it a code of 8 digits. \
                The code works for 10 min, only in this browser." ]
       ; code_form req ] )

let not_correct req =
  response
    (page "Your code"
       [ El.p [txt "The code is not correct."]
       ; code_form req
       ; El.p [El.a ~at:[At.href "/"] [txt "Get a new code"]] ] )

let home req =
  match Auth.user req with
  | Some u ->
      response
        (page "Signed in"
           [ El.p [txtf "Hello, %s." (Auth.User.id u)]
           ; form req "/logout" [El.button [txt "Sign out"]] ] )
  | None ->
      response
        (page "Sign in" [El.p [txt "Sign in with a code."]; address_form req])

let see_other path =
  Response.empty ~status:Status.see_other
    ~headers:(Headers.of_list [("Location", path)])
    ()

(* {1 The mail} *)

let mail_text address code =
  Printf.sprintf "To: %s\nSubject: Your sign-in code\n\nYour code is %s.\n"
    address code

(* The limits of Code use the address in lowercase, with no spaces at its ends.
   The lookup of the accounts must not be coarser: this example puts the whole
   address in lowercase. Code.start takes at most 320 bytes.

   Give one address to the mail library, not header text. Thus a valid address
   has exactly one @, with text before it and after it. It has no space, no
   control character, no DEL, no double quote and none of these characters: , ;
   < > ( ) \ *)
let normalize text = String.lowercase_ascii (String.trim text)

let valid address =
  let allowed c =
    c > ' ' && c <> '\127' && not (String.contains ",;<>\"()\\" c)
  in
  String.length address <= 320
  && String.for_all allowed address
  &&
  match String.split_on_char '@' address with
  | [local; domain] ->
      local <> "" && domain <> ""
  | _ ->
      false

(* {1 The handler} *)

type t = {queue: J.t; handler: Handler.t}

(* The two POST routes take tokens of one bucket for each client: 10, then one
   each 6 s. GET / has its own bucket: 20, then one each 3 s. The limits of Code
   are for each address, not for each client. The buckets are before the CSRF
   middleware, thus a POST that the CSRF middleware refuses takes a token too. A
   GET that its bucket refuses does not call Csrf.token, thus it makes no
   session. *)
let limit_requests () : Handler.middleware =
  let posts = Rate_limit.middleware ~burst:10 ~every:(Span.s 6) () in
  let pages = Rate_limit.middleware ~burst:20 ~every:(Span.s 3) () in
  fun next req ->
    match (Request.meth req, Request.path_only req) with
    | POST, ("/email" | "/code") ->
        posts next req
    | GET, "/" ->
        pages next req
    | _ ->
        next req

(* The limits of Code for each address, as functions of a key. These are the
   defaults of Code: 3 codes, then one each 10 min, and 10 checks, then one each
   hour. A check of a correct code takes a token too. *)
let code_limits () =
  let sends = Rate_limit.Limiter.make ~burst:3 ~every:(Span.s 600) () in
  let checks = Rate_limit.Limiter.make ~burst:10 ~every:(Span.s 3600) () in
  Code.make
    ~sends:(Rate_limit.Limiter.take sends)
    ~checks:(Rate_limit.Limiter.take checks)
    ()

(* [make ~key ~outbox ~report ~users] is the queue and the handler. [key] signs
   the session cookies and the CSRF tokens. [outbox mail] sends a mail. [report]
   gets a line for each refused start or check, and for each failed job. [users
   address] is the user of an address, if it has an account.

   The CSRF middleware exempts no route, also not the address form. A login form
   with a password has no session yet, thus it is exempt, and only Cross_origin
   protects it (see the README sections "CSRF" and "Users and policies"). Code
   keeps the code in the session. If no session exists before Code.start, only
   an Ok sets a cookie. Then a missing cookie tells an attacker that the limit
   refused the address. Thus the GET of the page makes the session, and the form
   has a token.

   With the token, a site that an old browser (with no Sec-Fetch-Site) opens
   cannot start a code in the session of the user. It also cannot sign the user
   in to the account of the attacker (a login CSRF).

   Each visitor of the page has a session in the store. A flood of new sessions
   removes the oldest sessions of memory_store (see caravan_session.mli). Thus
   GET / has a rate limit for each client. *)
let make ~key ~outbox ~report ~users =
  let codes = code_limits () in
  (* on_error gets the name of the job type, never its argument: the line has no
     code. *)
  let queue =
    J.create ~workers:2 ~capacity:1000
      ~on_error:(fun f -> report ("code mail: job " ^ f.name ^ " failed"))
      ()
  in
  (* The job finds the account, not the handler: thus the handler does the same
     steps for each address, and its response does not tell which addresses have
     an account. An address with no account gets no mail. A real application can
     send it a mail that tells that it has no account.

     The job has the default retry: at most 5 attempts. A durable queue keeps
     the argument, thus the code, in plain text in its store. The code works
     only in the session that started it. *)
  let send_code =
    J.job ~name:"code-mail" (fun (address, code) ->
        match users address with
        | Some _ ->
            outbox (mail_text address code)
        | None ->
            () )
  in
  let email req =
    match Request.form ~max_bytes:1024 req with
    | Error e ->
        Response.string ~status:(Form.error_status e)
          (Format.asprintf "%a\n" Form.pp_error e)
    | Ok f -> (
        let address =
          normalize (Option.value ~default:"" (Form.find "email" f))
        in
        if not (valid address) then
          Response.string ~status:Status.bad_request "Give a valid address.\n"
        else
          match Code.start codes req ~address with
          | Ok code ->
              ( match J.enqueue queue send_code (address, code) with
              | Ok () ->
                  ()
              | Error (`Full | `Stopped) ->
                  report ("no code mail for " ^ address) ) ;
              code_page req
          | Error _wait ->
              report ("code start refused for " ^ address) ;
              code_page req )
  in
  (* Code.check removes the code at Ok, thus a code works one time. Auth.sign_in
     gives the session a new ID. *)
  let code req =
    let text =
      match Request.form ~max_bytes:1024 req with
      | Ok f ->
          Option.value ~default:"" (Form.find "code" f)
      | Error _ ->
          ""
    in
    match Code.check codes req text with
    | Ok address -> (
      match users address with
      | Some u ->
          Auth.sign_in req u ; see_other "/"
      | None ->
          report ("code check: no account for " ^ address) ;
          not_correct req )
    | Error failure ->
        report (Format.asprintf "code check: %a" Code.pp_failure failure) ;
        not_correct req
  in
  let logout req = Auth.sign_out req ; see_other "/" in
  let routes req =
    match (Request.meth req, Request.path_only req) with
    | GET, "/" ->
        home req
    | POST, "/email" ->
        email req
    | POST, "/code" ->
        code req
    | POST, "/logout" ->
        logout req
    | _ ->
        Handler.not_found req
  in
  let handler =
    Handler.compose
      [ Cross_origin.middleware ()
      ; limit_requests ()
      ; Caravan_session.middleware ~keys:[key]
          ~store:(Caravan_session.memory_store ())
          ()
      ; Auth.middleware ~schemes:[Auth.Scheme.session ()] ()
      ; Caravan_session.Csrf.middleware () ]
      routes
  in
  {queue; handler}

(* The accounts of the demo. A real application keeps them in a database. *)
let demo_users address =
  match address with
  | "ann@example.com" | "ben@example.com" ->
      Some (Auth.User.make address)
  | _ ->
      None
