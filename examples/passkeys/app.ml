(* Passkeys (WebAuthn): a sign-up, the registration of a passkey, and a sign-in
   with the passkey.

   The sign-up has no password and no check of the person: it is a demo only.
   Each new name makes a user and signs it in. Then the user adds a passkey.
   After the sign-out, the passkey signs the user in again. A real application
   adds a passkey to an account that it knows, for example after a code by
   email.

   Run dune exec examples/passkeys/main.exe, then open http://localhost:8080/ in
   a browser. Use localhost, not 127.0.0.1: the RP ID localhost does not match
   the host 127.0.0.1, and an IP address cannot be an RP ID. The browser asks
   for a PIN, a fingerprint or a face. The users and the passkeys are in memory:
   a restart forgets them, but the browser or the phone keeps the passkey.
   Delete it there. The session cookie has the name __Host-session, thus it has
   Secure. Chrome and Firefox keep a Secure cookie over http for localhost.

   With curl, first set this shell variable:

   S=localhost:8080

   A visitor gets the options of a sign-in, with a new challenge. The response
   sets the cookie of the session that keeps the challenge:

   curl -i -X POST $S/passkey/sign-in-options

   The sign-up of the demo signs the user in. curl keeps the cookie in the file
   jar:

   curl -i -c jar -b jar -d name=ada $S/sign-up

   The options of a registration need the CSRF token of the page in the field
   X-CSRF-Token. Without it, they get 403:

   curl -i -b jar -X POST $S/passkey/register-options

   Copy the token of the page to T, then send it:

   T=$(curl -s -b jar $S/ | grep -o '[[:alnum:]_-]\{86\}' | head -n 1)

   curl -i -b jar -H "X-CSRF-Token: $T" -X POST $S/passkey/register-options

   Only an authenticator can make the response to the options. The test of this
   example has a software authenticator. *)

open Weft
open Caravan
open Caravan_html
module Auth = Caravan_auth
module Passkey = Caravan_auth.Passkey
module User = Caravan_auth.User

(* {1 The tables} *)

(* The users and the passkeys of the example, in memory. A real application
   keeps them in a database. The handlers run in many fibers and domains, thus a
   mutex protects the tables. The table [names] has the name of each user, by
   its user handle. The table [credentials] has each passkey, by its credential
   ID. *)
type db =
  { lock: Mutex.t
  ; names: (string, string) Hashtbl.t
  ; credentials: (string, Passkey.stored) Hashtbl.t }

let db () =
  { lock= Mutex.create ()
  ; names= Hashtbl.create 16
  ; credentials= Hashtbl.create 16 }

let locked db f = Mutex.protect db.lock f

let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i]) ) )

(* [add_user db name] is the user handle of a new user [name], or [None] if a
   user has the name [name]. The handle is 16 random bytes, in hex. It has no
   personal data: the authenticator keeps it and gives it back at each sign-in.
   It is also the user ID of the session. *)
let add_user db name =
  let handle = hex (Mirage_crypto_rng.generate 16) in
  locked db (fun () ->
      if Hashtbl.fold (fun _ n taken -> taken || n = name) db.names false then
        None
      else (
        Hashtbl.replace db.names handle name ;
        Some handle ) )

let user_name db handle = locked db (fun () -> Hashtbl.find_opt db.names handle)

let credential db id = locked db (fun () -> Hashtbl.find_opt db.credentials id)

let registered db id = locked db (fun () -> Hashtbl.mem db.credentials id)

(* [add_credential db id stored] adds a passkey, and is [false] if the table has
   [id]: then the table does not change. A database does this with a UNIQUE
   index of the credential ID, because two registrations at the same time can
   both pass the check of [registered]. *)
let add_credential db id stored =
  locked db (fun () ->
      if Hashtbl.mem db.credentials id then false
      else (
        Hashtbl.replace db.credentials id stored ;
        true ) )

let credential_ids db handle =
  locked db (fun () ->
      Hashtbl.fold
        (fun id (s : Passkey.stored) ids ->
          if s.user_handle = handle then id :: ids else ids )
        db.credentials [] )

let set_sign_count db id count =
  locked db (fun () ->
      match Hashtbl.find_opt db.credentials id with
      | Some s ->
          Hashtbl.replace db.credentials id {s with sign_count= count}
      | None ->
          () )

(* {1 The relying party} *)

(* The relying party is this site. The RP ID localhost accepts the origin
   http://localhost:8080, for development. In production, the RP ID is the
   domain of the site, for example example.com, with the origin
   https://example.com. A response from another origin is refused. *)
let rp =
  Passkey.rp ~id:"localhost" ~name:"caravan passkeys"
    ~origins:["http://localhost:8080"]

(* The passkey is the only check of a sign-in, thus the authenticator must
   verify the user: a PIN, a fingerprint or a face. With `Required, a response
   with no user verification is refused, at the registration and at the
   sign-in. *)
let verification = `Required

(* {1 The handlers} *)

let see_other path =
  Response.empty ~status:Status.see_other
    ~headers:(Headers.of_list [("Location", path)])
    ()

let json text = Response.string ~content_type:"application/json" text

let user handle name = User.make ~claims:[("name", name)] handle

let name u = Option.value ~default:"" (User.claim "name" u)

(* [report] gets the reason of a refusal. The browser gets no detail. *)
let refuse ~report what reason =
  report (Printf.sprintf "passkey %s refused: %s" what reason) ;
  Response.string ~status:Status.bad_request "The passkey was refused.\n"

let error e = Format.asprintf "%a" Passkey.pp_error e

(* [with_body req f] gives [f] the body of [req], with a limit of 64 KiB. A
   larger body gets 413. Passkey.register and Passkey.authenticate refuse a
   larger text. But the body streams, and without ~max, Body.to_string keeps up
   to 8 MiB (Config.max_body) in memory first. *)
let with_body req f =
  match Body.to_string ~max:(64 * 1024) (Request.body req) with
  | text ->
      f text
  | exception Body.Too_large ->
      Response.string ~status:Status.content_too_large "Too large\n"

let valid_name name =
  String.length name >= 1
  && String.length name <= 20
  && String.for_all (function 'a' .. 'z' -> true | _ -> false) name

(* POST /sign-up, the demo: a new user with a name, signed in. A name that is
   not valid gets 400. A name that a user has gets 409. Without the 409, two
   users have the same name, and the page cannot tell them apart. *)
let sign_up ~db req =
  match Request.form ~max_bytes:1024 req with
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
  | Ok form -> (
    match Form.find "name" form with
    | Some name when valid_name name -> (
      match add_user db name with
      | Some handle ->
          Auth.sign_in req (user handle name) ;
          see_other "/"
      | None ->
          Response.string ~status:Status.conflict
            "A user has this name. Sign in with its passkey.\n" )
    | _ ->
        Response.string ~status:Status.bad_request
          "name: 1 to 20 letters from a to z\n" )

(* POST /passkey/register-options, for the signed-in user. [exclude] has the
   credential IDs of the passkeys of the user: an authenticator with one of them
   does not make a second passkey. *)
let register_options ~db req =
  let u = Option.get (Auth.user req) in
  json
    (Passkey.creation_options ~verification
       ~exclude:(credential_ids db (User.id u))
       rp req ~user_id:(User.id u) ~user_name:(name u) ~display_name:(name u) )

(* POST /passkey/register, with the JSON of credential.toJSON() of
   navigator.credentials.create. The challenge of the response goes from the
   session, thus a response passes only one time. *)
let register ~db ~report req =
  with_body req (fun text ->
      match Passkey.register rp req ~registered:(registered db) text with
      | Ok c when add_credential db c.id c.stored ->
          Response.string "The passkey is saved.\n"
      | Ok _ ->
          refuse ~report "registration" "the credential ID is in the table"
      | Error e ->
          refuse ~report "registration" (error e) )

(* POST /passkey/sign-in-options. With no ~user, each passkey of this site can
   sign in: the browser shows the passkeys, and the person picks one. For a
   visitor, request_options makes the session that keeps the challenge. *)
let sign_in_options req = json (Passkey.request_options ~verification rp req)

(* POST /passkey/sign-in, with the JSON of credential.toJSON() of
   navigator.credentials.get.

   The authenticator adds 1 or more to its counter at each sign-in, and the
   table keeps the last counter. A counter that does not grow sets [cloned]: two
   authenticators can have the key. This example refuses the sign-in and reports
   it. Passkeys that sync always send 0, and then [cloned] is false. *)
let sign_in ~db ~report req =
  with_body req (fun text ->
      match Passkey.authenticate rp req ~lookup:(credential db) text with
      | Error e ->
          refuse ~report "sign-in" (error e)
      | Ok a when a.cloned ->
          refuse ~report "sign-in"
            "the counter did not grow: the authenticator can be a copy"
      | Ok a -> (
          set_sign_count db a.credential_id a.sign_count ;
          match user_name db a.user_handle with
          | Some n ->
              Auth.sign_in req (user a.user_handle n) ;
              Response.string "Signed in.\n"
          | None ->
              refuse ~report "sign-in" "no user has the user handle" ) )

let sign_out req = Auth.sign_out req ; see_other "/"

(* {1 The page} *)

(* The script of the page. It reads the options with
   parseCreationOptionsFromJSON and parseRequestOptionsFromJSON, and sends
   credential.toJSON(). It shows each text with textContent, never as HTML. A
   page with a CSRF token sends it in X-CSRF-Token. *)
let script =
  {|const token = document.querySelector('meta[name="csrf-token"]')?.content;
const message = document.getElementById("message");
const post = async (path, body) => {
  const headers = { "Content-Type": "application/json" };
  if (token) headers["X-CSRF-Token"] = token;
  const res = await fetch(path, { method: "POST", headers, body });
  if (!res.ok) throw new Error(await res.text());
  return res;
};
const on = (id, f) => {
  const button = document.getElementById(id);
  if (button) button.onclick = async () => {
    try { await f(); } catch (e) { message.textContent = e.message; }
  };
};
on("add", async () => {
  const options = await (await post("/passkey/register-options", "")).json();
  const credential = await navigator.credentials.create({
    publicKey: PublicKeyCredential.parseCreationOptionsFromJSON(options) });
  const res =
    await post("/passkey/register", JSON.stringify(credential.toJSON()));
  message.textContent = await res.text();
});
on("sign-in", async () => {
  const options = await (await post("/passkey/sign-in-options", "")).json();
  const credential = await navigator.credentials.get({
    publicKey: PublicKeyCredential.parseRequestOptionsFromJSON(options) });
  await post("/passkey/sign-in", JSON.stringify(credential.toJSON()));
  location.assign("/");
});|}

let page ?(head = []) children =
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head
            ( [ El.meta ~at:[At.v "charset" "utf-8"] ()
              ; El.title "caravan passkeys" ]
            @ head )
        ; El.body
            ( children
            @ [El.p ~at:[At.id "message"; At.aria "live" "polite"] []]
            @ [El.script script] ) ] ]

(* The page of a visitor: the sign-up of the demo, and the sign-in with a
   passkey. It calls no Csrf.token, thus it makes no session. *)
let visitor_page =
  page
    [ El.p [txt "Hello, visitor."]
    ; El.button ~at:[At.id "sign-in"] [txt "Sign in with a passkey"]
    ; El.form
        ~at:[At.method_ "post"; At.action "/sign-up"]
        [ El.label [txt "Name "; El.input ~at:[At.name "name"; At.required] ()]
        ; El.button [txt "Sign up (demo, no password)"] ] ]

(* The page of a user. The CSRF token is in a meta element for the script, and
   in the field _csrf of the sign-out form. The token gives the response
   Cache-Control: no-store. *)
let user_page req u =
  let token = Caravan_session.Csrf.token req in
  page
    ~head:[El.meta ~at:[At.name "csrf-token"; At.v "content" token] ()]
    [ El.p [txtf "Hello, %s." (name u)]
    ; El.button ~at:[At.id "add"] [txt "Add a passkey"]
    ; El.form
        ~at:[At.method_ "post"; At.action "/sign-out"]
        [ El.input ~at:[At.type_ "hidden"; At.name "_csrf"; At.value token] ()
        ; El.button [txt "Sign out"] ] ]

let home req =
  match Auth.user req with
  | Some u ->
      response (user_page req u)
  | None ->
      response visitor_page

(* {1 The handler} *)

(* The routes that a visitor can POST. *)
let public = ["/sign-up"; "/passkey/sign-in-options"; "/passkey/sign-in"]

(* [make ~key ~report ?db ()] is the handler. [key] signs the session cookies
   and the CSRF tokens. [report] gets a line for each refused passkey. [db] has
   the users and the passkeys (new tables by default).

   The order: Cross_origin, the session, the users, then the CSRF tokens.

   A signed-in user has a session, and the page has its token. The CSRF
   middleware checks the routes of the user: the options of a registration, the
   registration and the sign-out.

   A visitor has no session, thus its page has no token: a token for each
   visitor needs a session for each visitor of the page. Thus the CSRF
   middleware exempts the routes of [public], and Cross_origin protects them, as
   a login form.

   A sign-in needs the challenge that its options put in the session. A page of
   another site cannot read the challenge, thus it cannot make a sign-in in the
   session of a browser.

   [validate] reads the user of the session in the table at each request. A user
   that is no longer in the table signs out.

   Each call of the options can make a session, and each sign-up makes a user.
   Thus a rate limit of each client protects the memory: the sign-up and the two
   routes of the options share one bucket of 10 requests, with one more request
   each 6 s. *)
let make ~key ~report ?(db = db ()) () =
  let limit =
    Caravan_middleware.Rate_limit.middleware ~burst:10 ~every:(Span.s 6) ()
  in
  let signed_in = Auth.require () in
  let validate u = Option.map (user (User.id u)) (user_name db (User.id u)) in
  let routes req =
    match (Request.meth req, Request.path_only req) with
    | GET, "/" ->
        home req
    | POST, "/sign-up" ->
        limit (sign_up ~db) req
    | POST, "/passkey/register-options" ->
        signed_in (limit (register_options ~db)) req
    | POST, "/passkey/register" ->
        signed_in (register ~db ~report) req
    | POST, "/passkey/sign-in-options" ->
        limit sign_in_options req
    | POST, "/passkey/sign-in" ->
        sign_in ~db ~report req
    | POST, "/sign-out" ->
        sign_out req
    | _ ->
        Handler.not_found req
  in
  Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Caravan_session.middleware ~keys:[key]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Auth.middleware ~schemes:[Auth.Scheme.session ~validate ()] ()
    ; Caravan_session.Csrf.middleware
        ~exempt:(fun r -> List.mem (Request.path_only r) public)
        () ]
    routes
