(* Sessions: a signed session cookie with a store of the application, CSRF
   tokens in the forms, an upload with a token, and a signed link.

   The login has no password: it is a demo only. The auth example shows users
   and policies.

   Run dune exec examples/sessions/main.exe, then open http://localhost:8080/ in
   a browser. The session cookie has the name __Host-session, thus it has
   Secure. Chrome and Firefox keep a Secure cookie over http for localhost, but
   some browsers do not keep it for 127.0.0.1.

   With curl, first set this shell variable:

   S=127.0.0.1:8080

   A page with no form makes no session, thus it sets no cookie:

   curl -i $S/about

   The login sets the cookie. curl keeps it in the file jar:

   curl -i -c jar -b jar -d name=ada $S/login

   curl -i -b jar $S/

   Each form of the page has the CSRF token. Copy it to T:

   T=$(curl -s -b jar $S/ | grep -o '[[:alnum:]_-]\{86\}' | head -n 1)

   curl -i -b jar -d "text=hello&_csrf=$T" $S/note

   curl -i -b jar -d text=hello $S/note

   curl -i -b jar -F "_csrf=$T" -F 'file=hi;filename=a.txt' $S/upload

   curl -i -b jar -F 'file=hi;filename=a.txt' $S/upload

   The page has a signed link to unsubscribe from the mail. Copy it to L. Its
   GET shows a form, and its POST does the change:

   L=$(curl -s -b jar $S/ | grep -o 'unsubscribe?t=[[:alnum:]._-]*')

   curl -i "$S/$L"

   curl -i -X POST "$S/$L"

   curl -i -c jar -b jar -d "_csrf=$T" $S/logout

   curl -i -b jar $S/ *)

open Caravan
open Caravan_html
module Session = Caravan_session

(* {1 The store} *)

(* A store of the application over a Hashtbl, for the example. A real
   application uses a database. A real store deletes the expired rows, for
   example with a recurring job. The functions run in many fibers and domains,
   thus a mutex protects the table. *)
module Table_store = struct
  type t = {lock: Mutex.t; rows: (string, string * Weft.Clock.posix) Hashtbl.t}

  let create () = {lock= Mutex.create (); rows= Hashtbl.create 64}

  let locked t f = Mutex.protect t.lock f

  let find t key = locked t (fun () -> Hashtbl.find_opt t.rows key)

  let create_row t key value ~expires =
    locked t (fun () -> Hashtbl.replace t.rows key (value, expires))

  (* [update] and [touch] change only a row that exists: an update after a
     remove does nothing. *)
  let update t key value ~expires =
    locked t (fun () ->
        if Hashtbl.mem t.rows key then
          Hashtbl.replace t.rows key (value, expires) )

  let touch t key ~expires =
    locked t (fun () ->
        match Hashtbl.find_opt t.rows key with
        | Some (value, _) ->
            Hashtbl.replace t.rows key (value, expires)
        | None ->
            () )

  (* [remove] is [true] if the row existed. *)
  let remove t key =
    locked t (fun () ->
        let found = Hashtbl.mem t.rows key in
        Hashtbl.remove t.rows key ; found )

  let store t =
    Session.store ~find:(find t) ~create:(create_row t) ~update:(update t)
      ~touch:(touch t) ~remove:(remove t)
end

(* {1 The pages} *)

let see_other path =
  Response.empty ~status:Status.see_other
    ~headers:(Headers.of_list [("Location", path)])
    ()

let bad_request text = Response.string ~status:Status.bad_request text

let page title children =
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head [El.meta ~at:[At.v "charset" "utf-8"] (); El.title title]
        ; El.body children ] ]

(* A POST form. [token] goes into the hidden field _csrf. *)
let form ?token ?(at = []) action children =
  let csrf =
    match token with
    | Some t ->
        El.input ~at:[At.type_ "hidden"; At.name "_csrf"; At.value t] ()
    | None ->
        empty
  in
  El.form ~at:([At.method_ "post"; At.action action] @ at) (csrf :: children)

let login_page =
  page "Sessions"
    [ El.p [txt "Hello, guest."]
    ; form "/login"
        [ El.label [txt "Name "; El.input ~at:[At.name "name"; At.required] ()]
        ; El.button [txt "Sign in"] ] ]

(* {1 The handlers} *)

(* The uploads of each user: a table of the application, with the user name as
   key. The data of a session has at most 64 KiB: a table of the application
   keeps the larger data.

   Session.id gives the ID of the session, but a login gives the session a new
   ID. Thus the ID is not a key for the data of a user. The example also does
   not log the ID: the session cookie has it, thus keep it out of the logs, as
   the cookie. *)
type uploads = {lock: Mutex.t; files: (string, string list) Hashtbl.t}

let uploads_of uploads user =
  Mutex.protect uploads.lock (fun () ->
      Option.value ~default:[] (Hashtbl.find_opt uploads.files user) )

(* [Signed] signs a value with the key of the sessions. The purpose
   "unsubscribe" binds the signature to this use: a value that the application
   signs for another purpose does not unsubscribe a user. To rotate, give
   [new_key; old_key] to the middleware and to verify. This link never expires:
   put an expiry in the value for other uses. *)
let unsubscribe_purpose = "unsubscribe"

(* The page of a user. The response gets Cache-Control: no-store, because the
   page has a CSRF token. *)
let user_page ~key ~uploads ~unsubscribed req user =
  let token = Session.Csrf.token req in
  let note = Option.value ~default:"(none)" (Session.find "note" req) in
  let link =
    "/unsubscribe?"
    ^ Form.encode
        [("t", Session.Signed.sign ~purpose:unsubscribe_purpose key user)]
  in
  let mail = if unsubscribed user then "off" else "on" in
  page "Sessions"
    [ El.p [txtf "Hello, %s." user]
    ; El.p [txtf "Note: %s" note]
    ; form ~token "/note"
        [ El.label [txt "Note "; El.input ~at:[At.name "text"] ()]
        ; El.button [txt "Save"] ]
    ; El.ul (List.map (fun f -> El.li [txt f]) (uploads_of uploads user))
    ; form ~token
        ~at:[At.v "enctype" "multipart/form-data"]
        "/upload"
        [ El.input ~at:[At.type_ "file"; At.name "file"; At.required] ()
        ; El.button [txt "Upload"] ]
    ; El.p [txtf "Mail: %s. " mail; El.a ~at:[At.href link] [txt "Unsubscribe"]]
    ; form ~token "/logout" [El.button [txt "Sign out"]] ]

let home ~key ~uploads ~unsubscribed req =
  match Session.find "user" req with
  | Some user ->
      response (user_page ~key ~uploads ~unsubscribed req user)
  | None ->
      response login_page

(* A page with no form and no Session.set: it makes no session, thus the
   response has no cookie. *)
let about _req = Response.string "This example shows sessions.\n"

let valid_name name =
  String.length name >= 1
  && String.length name <= 20
  && String.for_all (function 'a' .. 'z' -> true | _ -> false) name

(* The login. A login after another user starts a new session: Session.clear
   ends the session of the other user, then Session.set makes a new session with
   a new ID. Thus the note of the other user does not go to the new user.

   A login of the same user keeps the data. If a session exists, Session.renew
   gives it a new ID: an ID that an attacker gave to the browser before the
   login is then of no use (session fixation). With no session, Session.set
   makes a new session with a new ID.

   The CSRF middleware exempts this route, and Cross_origin protects it. *)
let login req =
  match Request.form ~max_bytes:1024 req with
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
  | Ok form -> (
    match Form.find "name" form with
    | Some name when valid_name name ->
        ( match Session.find "user" req with
        | Some other when other <> name ->
            Session.clear req
        | _ ->
            Session.renew req ) ;
        Session.set "user" name req ;
        see_other "/"
    | _ ->
        bad_request "name: 1 to 20 letters from a to z\n" )

(* The CSRF middleware checked the token of the form. It refuses a request with
   no session, and in this example only a login makes a session. Thus only a
   signed-in user gets here. An empty text removes the note. *)
let note req =
  match Request.form ~max_bytes:4096 req with
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
  | Ok form -> (
    match Form.find "text" form with
    | None | Some "" ->
        Session.remove "note" req ;
        Response.string "Note removed.\n"
    | Some text ->
        Session.set "note" text req ;
        Response.string "Note saved.\n" )

(* The CSRF middleware exempts this route: it refuses each body that is not a
   form, and a multipart body is not a form. Thus the handler reads the parts,
   and calls Csrf.check on the field _csrf before it changes the state. A
   request with no session or a bad token gets 403. *)
let upload ~uploads req =
  match Request.multipart_form ~max_bytes:(64 * 1024) req with
  | Error e ->
      Response.string ~status:(Multipart.error_status e)
        (Format.asprintf "%a\n" Multipart.pp_error e)
  | Ok fields -> (
    match (Multipart.find "_csrf" fields, Session.find "user" req) with
    | Some {filename= None; data= token; _}, Some user
      when Session.Csrf.check token req -> (
      match Multipart.find "file" fields with
      | Some {filename= Some name; data; _} ->
          let file = Printf.sprintf "%s: %d bytes" name (String.length data) in
          Mutex.protect uploads.lock (fun () ->
              let files =
                Option.value ~default:[] (Hashtbl.find_opt uploads.files user)
              in
              Hashtbl.replace uploads.files user (files @ [file]) ) ;
          Response.string (file ^ "\n")
      | _ ->
          bad_request "Send a file.\n" )
    | _ ->
        Response.string ~status:Status.forbidden "Forbidden\n" )

(* The logout. Session.clear ends the session in the store, and the response
   expires the cookie: the old cookie then gives a guest. *)
let logout req = Session.clear req ; see_other "/"

(* The link of a mail: it has the user and its signature. A mail scanner can
   open a link, thus a GET only shows a form, and the POST of the form does the
   change (as RFC 8058). The person who opens the link can have no session: thus
   the CSRF middleware exempts this route, and the signature is the proof. A bad
   signature, or a signature for another purpose, gets 400. *)
let unsubscribe ~key ~add req =
  let signed =
    Option.bind (Request.query req) (Form.find_text "t")
    |> Option.value ~default:""
  in
  let user = Session.Signed.verify ~purpose:unsubscribe_purpose [key] signed in
  match (Request.meth req, user) with
  | _, None ->
      bad_request "This link is not valid.\n"
  | POST, Some user ->
      add user ;
      Response.string (Printf.sprintf "%s gets no more mail.\n" user)
  | _, Some user ->
      response
        (page "Unsubscribe"
           [ El.p [txtf "Stop the mail to %s?" user]
           ; form (Request.path req) [El.button [txt "Unsubscribe"]] ] )

(* {1 The handler} *)

(* The paths that the CSRF middleware does not check. Cross_origin checks them
   too. *)
let exempt req =
  List.mem (Request.path_only req) ["/login"; "/upload"; "/unsubscribe"]

(* [make ~key] is the handler. [key] signs the session cookies, the CSRF tokens
   and the links.

   A session ends at most 20 min after its last request (~idle). It can end 10
   min after its last request: the middleware moves the limit only when more
   than half of it passed. A session also ends 8 h after the login that made it,
   also with requests (~absolute, 8 h is the default). Then the user signs in
   again.

   The order: Cross_origin refuses a POST that a browser sends from another
   site, then the session middleware finds the session, then the CSRF middleware
   checks the token of each POST. *)
let make ~key =
  let uploads = {lock= Mutex.create (); files= Hashtbl.create 16} in
  let unsubscribed = Hashtbl.create 16 in
  let lock = Mutex.create () in
  let add user =
    Mutex.protect lock (fun () -> Hashtbl.replace unsubscribed user ())
  in
  let is_unsubscribed user =
    Mutex.protect lock (fun () -> Hashtbl.mem unsubscribed user)
  in
  let routes req =
    match (Request.meth req, Request.path_only req) with
    | GET, "/" ->
        home ~key ~uploads ~unsubscribed:is_unsubscribed req
    | GET, "/about" ->
        about req
    | POST, "/login" ->
        login req
    | POST, "/note" ->
        note req
    | POST, "/upload" ->
        upload ~uploads req
    | POST, "/logout" ->
        logout req
    | (GET | POST), "/unsubscribe" ->
        unsubscribe ~key ~add req
    | _ ->
        Handler.not_found req
  in
  Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Session.middleware
        ~idle:(Weft.Span.s (20 * 60))
        ~absolute:(Weft.Span.s (8 * 3600))
        ~keys:[key]
        ~store:(Table_store.store (Table_store.create ()))
        ()
    ; Session.Csrf.middleware ~exempt () ]
    routes
