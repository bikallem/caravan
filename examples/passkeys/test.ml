(* The tests of the passkeys example. Each request of the header comment of
   app.ml has a test here, with the same request. A software authenticator makes
   the responses of a browser: copy it for the tests of your application.

   Each test sets a seeded generator first, and runs on one domain: the
   simulator runs on one OS thread, thus the Fortuna generator is correct here.
   The jar of the tests keeps the cookies, as curl -c jar -b jar. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module App = Example_passkeys.App
module Jar = Example_support.Jar
module Page = Example_support.Page
module P256 = Mirage_crypto_ec.P256

(* {1 Base64url} *)

(* WebAuthn sends bytes in JSON as base64url with no padding (RFC 4648 §5). *)
let b64u s =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

let of_b64u s =
  Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet s

(* {1 CBOR} *)

(* A small CBOR encoder (RFC 8949), with only the types of an attestation object
   and of a COSE key. *)
type cbor =
  | Int of int
  | Bytes of string
  | Text of string
  | Map of (cbor * cbor) list

let rec cbor v =
  (* The head of an item: its major type, then a number of at most 16 bits. *)
  let head major n =
    let c x = String.make 1 (Char.chr x) in
    if n < 24 then c ((major lsl 5) lor n)
    else if n < 256 then c ((major lsl 5) lor 24) ^ c n
    else c ((major lsl 5) lor 25) ^ c (n lsr 8) ^ c (n land 255)
  in
  match v with
  | Int i when i >= 0 ->
      head 0 i
  | Int i ->
      head 1 (-1 - i)
  | Bytes b ->
      head 2 (String.length b) ^ b
  | Text t ->
      head 3 (String.length t) ^ t
  | Map l ->
      head 5 (List.length l)
      ^ String.concat "" (List.map (fun (k, v) -> cbor k ^ cbor v) l)

(* {1 A software authenticator} *)

(* An authenticator with one ES256 key (ECDSA with P-256 and SHA-256). A real
   authenticator keeps its key in hardware or in a password manager.

   [id] is the credential ID. [user] is the user handle of the registration, in
   base64url. [count] is the signature counter. [step] is the change of the
   counter at each sign-in. A passkey that syncs has the step 0, thus it always
   sends 0.

   This authenticator does not read the list [excludeCredentials] of the
   options: it registers its key again, as an old authenticator can. *)
type authenticator =
  { key: P256.Dsa.priv
  ; id: string
  ; mutable user: string
  ; mutable count: int
  ; step: int }

let authenticator ?(step = 1) id =
  let key, _ = P256.Dsa.generate () in
  {key; id; user= ""; count= 0; step}

let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

(* The public key as COSE (RFC 9053): kty EC2 (1: 2), alg ES256 (3: -7), crv
   P-256 (-1: 1), then x (-2) and y (-3). The octets of the public key are 0x04,
   x and y, of 32 bytes each. *)
let cose_key a =
  let pub = P256.Dsa.(pub_to_octets (pub_of_priv a.key)) in
  cbor
    (Map
       [ (Int 1, Int 2)
       ; (Int 3, Int (-7))
       ; (Int (-1), Int 1)
       ; (Int (-2), Bytes (String.sub pub 1 32))
       ; (Int (-3), Bytes (String.sub pub 33 32)) ] )

(* The flags of the authenticator data. *)
let up = 0x01 (* the user is present *)

let uv = 0x04 (* the user is verified: a PIN or a biometric check *)

let at = 0x40 (* the attested credential data follows *)

(* The authenticator data (WebAuthn §6.1): the SHA-256 of the RP ID, the flags,
   the counter (4 bytes, big-endian), then [rest]. *)
let auth_data ~rp_id ~flags ~count rest =
  sha256 rp_id
  ^ String.make 1 (Char.chr flags)
  ^ String.init 4 (fun i -> Char.chr ((count lsr (8 * (3 - i))) land 255))
  ^ rest

(* The attested credential data: the AAGUID (16 zero bytes: no model), the
   length of the credential ID (2 bytes), the ID, then the COSE key. *)
let attested a =
  let n = String.length a.id in
  String.make 16 '\x00'
  ^ String.init 2 (fun i -> Char.chr ((n lsr (8 * (1 - i))) land 255))
  ^ a.id ^ cose_key a

(* The DER form of an ECDSA signature: SEQUENCE { INTEGER r, INTEGER s }. An
   INTEGER has no zero bytes first, but gets one if its high bit is set. One
   length byte: correct for values under 128 bytes, as for ES256. *)
let der_signature (r, s) =
  let tlv tag v =
    String.make 1 tag ^ String.make 1 (Char.chr (String.length v)) ^ v
  in
  let int v =
    let rec strip v =
      if String.length v > 1 && v.[0] = '\x00' then
        strip (String.sub v 1 (String.length v - 1))
      else v
    in
    let v = strip v in
    tlv '\x02' (if Char.code v.[0] >= 0x80 then "\x00" ^ v else v)
  in
  tlv '\x30' (int r ^ int s)

(* The page of this example has this origin. *)
let origin = "http://localhost:8080"

(* The client data that the browser makes, as JSON text. *)
let client_data ~typ ~challenge ~origin =
  Yojson.Safe.to_string
    (`Assoc
       [ ("type", `String typ)
       ; ("challenge", `String challenge)
       ; ("origin", `String origin) ] )

(* The JSON of credential.toJSON(): the credential ID, and the members of
   [response]. Each byte string is in base64url. *)
let credential_json a response =
  Yojson.Safe.to_string
    (`Assoc
       [ ("id", `String (b64u a.id))
       ; ("rawId", `String (b64u a.id))
       ; ("type", `String "public-key")
       ; ("response", `Assoc response) ] )

(* The text of a member of a JSON object, at a path of names. *)
let member names text =
  let json = Yojson.Safe.from_string text in
  Yojson.Safe.Util.(
    to_string (List.fold_left (fun j name -> member name j) json names) )

(* [create a options] is the JSON of credential.toJSON() of
   navigator.credentials.create, for the options of the server. The
   authenticator keeps the user handle of the options. *)
let create ?(origin = origin) ?(flags = up lor uv lor at) a options =
  a.user <- member ["user"; "id"] options ;
  let data =
    auth_data
      ~rp_id:(member ["rp"; "id"] options)
      ~flags ~count:a.count (attested a)
  in
  let attestation =
    cbor
      (Map
         [ (Text "fmt", Text "none")
         ; (Text "attStmt", Map [])
         ; (Text "authData", Bytes data) ] )
  in
  let client =
    client_data ~typ:"webauthn.create"
      ~challenge:(member ["challenge"] options)
      ~origin
  in
  credential_json a
    [ ("clientDataJSON", `String (b64u client))
    ; ("attestationObject", `String (b64u attestation))
    ; ("transports", `List [`String "internal"]) ]

(* [get a options] is the JSON of credential.toJSON() of
   navigator.credentials.get. The counter changes by the step. The signature
   covers the authenticator data and the SHA-256 of the client data. *)
let get ?(origin = origin) ?(flags = up lor uv) a options =
  a.count <- a.count + a.step ;
  let data =
    auth_data ~rp_id:(member ["rpId"] options) ~flags ~count:a.count ""
  in
  let client =
    client_data ~typ:"webauthn.get"
      ~challenge:(member ["challenge"] options)
      ~origin
  in
  let signature =
    der_signature (P256.Dsa.sign ~key:a.key (sha256 (data ^ sha256 client)))
  in
  credential_json a
    [ ("clientDataJSON", `String (b64u client))
    ; ("authenticatorData", `String (b64u data))
    ; ("signature", `String (b64u signature))
    ; ("userHandle", `String a.user) ]

(* {1 The browser} *)

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan passkeys example"
       (module Mirage_crypto_rng.Fortuna) )

let key = Caravan_session.Key.of_string (String.make 32 'k')

let cookie_name = "__Host-session"

(* [run ?manual ?report ?db f] calls [f] with a new application in its own
   run. *)
let run ?manual ?(report = ignore) ?db f =
  seed () ;
  T.run ?manual (App.make ~key ~report ?db ()) f

(* A browser: its cookies, and the CSRF token of its last page. *)
type browser = {jar: Jar.t; mutable token: string option}

let browser () = {jar= Jar.create (); token= None}

let header name (r : T.reply) = Headers.find name r.headers

let set_cookies (r : T.reply) = Headers.find_all "set-cookie" r.headers

(* The token of a page, if the page has one. *)
let token_of (r : T.reply) =
  match Page.csrf_token r.body with
  | tok ->
      Some tok
  | exception Not_found ->
      None

(* [load b t] gets the page /, and keeps its token. *)
let load b t =
  let r = Jar.fetch b.jar t (Request.make GET "/") in
  b.token <- token_of r ;
  r

(* [post b t path body] is a POST of the script of the page: a JSON body, with
   the token of the page in X-CSRF-Token. *)
let post ?(headers = []) b t path body =
  let token =
    match b.token with Some tok -> [("X-CSRF-Token", tok)] | None -> []
  in
  let headers =
    Headers.of_list ((("Content-Type", "application/json") :: token) @ headers)
  in
  Jar.fetch b.jar t
    (Request.make ~headers ~body:(Body.of_string body) POST path)

(* The request of curl -c jar -b jar -d name=ada $S/sign-up, then the page. *)
let sign_up ?(headers = []) b t name =
  let req = Request.make ~headers:(Headers.of_list headers) POST "/sign-up" in
  let r = Jar.fetch b.jar t (Request.with_form [("name", name)] req) in
  ignore (load b t) ;
  r

(* The sign-out form of the page, with its field _csrf. *)
let sign_out b t =
  let token = Option.value ~default:"" b.token in
  let req =
    Request.with_form [("_csrf", token)] (Request.make POST "/sign-out")
  in
  let r = Jar.fetch b.jar t req in
  ignore (load b t) ;
  r

(* The button "Add a passkey": the options, then the response of [a]. *)
let add_passkey ?origin ?flags b t a =
  let options = post b t "/passkey/register-options" "" in
  post b t "/passkey/register" (create ?origin ?flags a options.body)

(* The button "Sign in with a passkey": the options, then the response of [a],
   then the page. *)
let sign_in ?origin ?flags b t a =
  let options = post b t "/passkey/sign-in-options" "" in
  let r = post b t "/passkey/sign-in" (get ?origin ?flags a options.body) in
  ignore (load b t) ;
  r

(* [with_passkey t] is a browser [b] and an authenticator [a]. In [b], the user
   [name] (ada by default) signed up, added the passkey of [a], then signed
   out. *)
let with_passkey ?(name = "ada") ?step t =
  let b = browser () in
  ignore (sign_up b t name) ;
  let a = authenticator ?step ("credential of " ^ name) in
  let r = add_passkey b t a in
  equal string "The passkey is saved.\n" r.body ;
  ignore (sign_out b t) ;
  (b, a)

let hello name (r : T.reply) =
  contains ~sub:(Printf.sprintf "Hello, %s." name) r.body

let refused = "The passkey was refused.\n"

(* A list of reports, in order, and the function that adds a report. *)
let new_reports () =
  let l = ref [] in
  (l, fun line -> l := !l @ [line])

let () =
  exit
  @@ Windtrap.run "passkeys"
       [ test "the sign-in options of a visitor: a challenge and a session"
           (fun () ->
             (* curl -i -X POST $S/passkey/sign-in-options *)
             let r =
               run (fun t ->
                   T.fetch t (Request.make POST "/passkey/sign-in-options") )
             in
             equal status Status.ok r.status ;
             equal (option string) (Some "application/json")
               (header "content-type" r) ;
             equal int 43 (String.length (member ["challenge"] r.body)) ;
             equal string "required" (member ["userVerification"] r.body) ;
             equal string "localhost" (member ["rpId"] r.body) ;
             equal int 1 (List.length (set_cookies r)) ;
             contains ~sub:(cookie_name ^ "=") (List.hd (set_cookies r)) )
       ; test "the page of a visitor makes no session and has no token"
           (fun () ->
             let r = run (fun t -> load (browser ()) t) in
             equal status Status.ok r.status ;
             contains ~sub:"Sign in with a passkey" r.body ;
             equal (list string) [] (set_cookies r) ;
             not_contains ~sub:"_csrf" r.body ;
             not_contains ~sub:"<meta name=\"csrf-token\"" r.body )
       ; test "the sign-up signs a new user in: the page has the token"
           (fun () ->
             (* curl -i -c jar -b jar -d name=ada $S/sign-up *)
             let r, page =
               run (fun t ->
                   let b = browser () in
                   let r = sign_up b t "ada" in
                   (r, load b t) )
             in
             equal status Status.see_other r.status ;
             equal (option string) (Some "/") (header "location" r) ;
             hello "ada" page ;
             equal (option string) (Some "no-store")
               (header "cache-control" page) ;
             (* The T= line takes the first match of grep: the token. The meta
                element has the same token as the field _csrf. *)
             let tok = Option.get (token_of page) in
             equal string tok (List.hd (Page.grep_runs 86 page.body)) ;
             let meta = {|<meta name="csrf-token" content="|} in
             contains ~sub:(meta ^ tok ^ {|"|}) page.body )
       ; test "a sign-up with the name of a user: 409, and no session"
           (fun () ->
             let r =
               run (fun t ->
                   ignore (sign_up (browser ()) t "ada") ;
                   let b = browser () in
                   let r = sign_up b t "ada" in
                   hello "visitor" (load b t) ;
                   r )
             in
             equal status Status.conflict r.status ;
             equal (list string) [] (set_cookies r) )
       ; test "a name that is not valid: 400, and no session" (fun () ->
             let rs =
               run (fun t ->
                   List.map
                     (fun name -> sign_up (browser ()) t name)
                     ["Ada"; ""; String.make 21 'a'; "ada 1"] )
             in
             List.iter
               (fun (r : T.reply) ->
                 equal status Status.bad_request r.status ;
                 equal (list string) [] (set_cookies r) )
               rs )
       ; test "a user that is no longer in the table signs out" (fun () ->
             let db = App.db () in
             let before, after =
               run ~db (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   let before = load b t in
                   App.locked db (fun () -> Hashtbl.reset db.names) ;
                   (before, load b t) )
             in
             hello "ada" before ; hello "visitor" after )
       ; test "the routes of a user without X-CSRF-Token or _csrf: 403"
           (fun () ->
             (* curl -i -b jar -X POST $S/passkey/register-options *)
             let statuses, page =
               run (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   let send path =
                     (Jar.fetch b.jar t (Request.make POST path)).status
                   in
                   let statuses =
                     List.map send
                       [ "/passkey/register-options"
                       ; "/passkey/register"
                       ; "/sign-out" ]
                   in
                   (statuses, load b t) )
             in
             equal (list status)
               [Status.forbidden; Status.forbidden; Status.forbidden]
               statuses ;
             hello "ada" page )
       ; test "the registration options, with the token of the page" (fun () ->
             (* The T= line, then the curl line with "X-CSRF-Token: $T". *)
             let r =
               run (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   let token = Option.get b.token in
                   Jar.fetch b.jar t
                     (Request.make
                        ~headers:(Headers.of_list [("X-CSRF-Token", token)])
                        POST "/passkey/register-options" ) )
             in
             equal status Status.ok r.status ;
             equal string "ada" (member ["user"; "name"] r.body) ;
             equal string "localhost" (member ["rp"; "id"] r.body) ;
             equal string "required"
               (member ["authenticatorSelection"; "userVerification"] r.body) ;
             (* The user handle: 16 random bytes in hex, with no name. *)
             let handle = of_b64u (member ["user"; "id"] r.body) in
             equal int 32 (String.length handle) ;
             is_true
               (String.for_all
                  (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
                  handle ) )
       ; test "a registration stores a credential of the user" (fun () ->
             let db = App.db () in
             let r, a =
               run ~db (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   let a = authenticator "credential 1" in
                   (add_passkey b t a, a) )
             in
             equal status Status.ok r.status ;
             equal string "The passkey is saved.\n" r.body ;
             let handle = of_b64u a.user in
             equal (option string) (Some "ada") (App.user_name db handle) ;
             match App.credential db "credential 1" with
             | Some s ->
                 equal string handle s.user_handle ;
                 equal int 0 s.sign_count
             | None ->
                 fail "no credential" )
       ; test "the options of a user exclude its passkeys" (fun () ->
             let r =
               run (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   ignore (add_passkey b t (authenticator "credential 1")) ;
                   post b t "/passkey/register-options" "" )
             in
             contains
               ~sub:(Printf.sprintf {|"id":"%s"|} (b64u "credential 1"))
               r.body )
       ; test "a second use of a registration: refused" (fun () ->
             let reports, report = new_reports () in
             let first, second =
               run ~report (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   let options = post b t "/passkey/register-options" "" in
                   let response =
                     create (authenticator "credential 1") options.body
                   in
                   let first = post b t "/passkey/register" response in
                   (first, post b t "/passkey/register" response) )
             in
             equal status Status.ok first.status ;
             equal status Status.bad_request second.status ;
             equal string refused second.body ;
             equal (list string)
               ["passkey registration refused: No_ceremony"]
               !reports )
       ; test "one authenticator registered two times: Registered" (fun () ->
             let reports, report = new_reports () in
             let first, second =
               run ~report (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   let a = authenticator "credential 1" in
                   let first = add_passkey b t a in
                   (* New options, thus a new challenge. *)
                   (first, add_passkey b t a) )
             in
             equal status Status.ok first.status ;
             equal status Status.bad_request second.status ;
             equal (list string)
               ["passkey registration refused: Registered"]
               !reports )
       ; test "localhost does not take 127.0.0.1, and an IP is not an RP ID"
           (fun () ->
             let rp id origin () =
               ignore (Caravan_auth.Passkey.rp ~id ~name:"x" ~origins:[origin])
             in
             let invalid f =
               match f () with
               | () ->
                   fail "no Invalid_argument"
               | exception Invalid_argument _ ->
                   ()
             in
             invalid (rp "localhost" "http://127.0.0.1:8080") ;
             invalid (rp "127.0.0.1" "https://127.0.0.1") )
       ; test "the origin http://127.0.0.1:8080: refused" (fun () ->
             let reports, report = new_reports () in
             let r =
               run ~report (fun t ->
                   let b, a = with_passkey t in
                   sign_in ~origin:"http://127.0.0.1:8080" b t a )
             in
             equal status Status.bad_request r.status ;
             equal (list string)
               ["passkey sign-in refused: Bad_origin \"http://127.0.0.1:8080\""]
               !reports )
       ; test "a sign-in signs the user in, and stores the counter" (fun () ->
             let db = App.db () in
             let r, before, after =
               run ~db (fun t ->
                   let b, a = with_passkey t in
                   let before = load b t in
                   let r = sign_in b t a in
                   (r, before, load b t) )
             in
             hello "visitor" before ;
             equal status Status.ok r.status ;
             equal string "Signed in.\n" r.body ;
             hello "ada" after ;
             equal (option int) (Some 1)
               (Option.map
                  (fun (s : Caravan_auth.Passkey.stored) -> s.sign_count)
                  (App.credential db "credential of ada") ) )
       ; test "a sign-in in a new browser, with no user name" (fun () ->
             let r, page =
               run (fun t ->
                   let _, a = with_passkey t in
                   let b = browser () in
                   let r = sign_in b t a in
                   (r, load b t) )
             in
             equal status Status.ok r.status ;
             hello "ada" page )
       ; test "a second use of one challenge: refused" (fun () ->
             let reports, report = new_reports () in
             let first, second =
               run ~report (fun t ->
                   let b, a = with_passkey t in
                   let options = post b t "/passkey/sign-in-options" "" in
                   let response = get a options.body in
                   let first = post b t "/passkey/sign-in" response in
                   (first, post b t "/passkey/sign-in" response) )
             in
             equal status Status.ok first.status ;
             equal status Status.bad_request second.status ;
             equal string refused second.body ;
             equal (list string)
               ["passkey sign-in refused: No_ceremony"]
               !reports )
       ; test "the challenge of another browser: refused" (fun () ->
             let reports, report = new_reports () in
             let r, page =
               run ~report (fun t ->
                   let _, a = with_passkey t in
                   let victim = browser () in
                   let attacker = browser () in
                   let options =
                     post attacker t "/passkey/sign-in-options" ""
                   in
                   ignore (post victim t "/passkey/sign-in-options" "") ;
                   let r =
                     post victim t "/passkey/sign-in" (get a options.body)
                   in
                   (r, load victim t) )
             in
             equal status Status.bad_request r.status ;
             hello "visitor" page ;
             equal (list string)
               ["passkey sign-in refused: No_ceremony"]
               !reports )
       ; test "another origin: refused at the registration and the sign-in"
           (fun () ->
             let reports, report = new_reports () in
             let db = App.db () in
             (* Another site on the same host, for example another application
                in development. *)
             let other = "http://localhost:3000" in
             let register, sign, page =
               run ~report ~db (fun t ->
                   let b, a = with_passkey t in
                   ignore (sign_up b t "bob") ;
                   let register =
                     add_passkey ~origin:other b t (authenticator "bob 1")
                   in
                   ignore (sign_out b t) ;
                   let sign = sign_in ~origin:other b t a in
                   (register, sign, load b t) )
             in
             equal status Status.bad_request register.status ;
             equal status Status.bad_request sign.status ;
             equal (option string) None
               (Option.map (fun _ -> "") (App.credential db "bob 1")) ;
             hello "visitor" page ;
             equal (list string)
               [ "passkey registration refused: Bad_origin \"" ^ other ^ "\""
               ; "passkey sign-in refused: Bad_origin \"" ^ other ^ "\"" ]
               !reports )
       ; test "no user verification: refused at a registration and a sign-in"
           (fun () ->
             let reports, report = new_reports () in
             let register, sign, page =
               run ~report (fun t ->
                   let b, a = with_passkey t in
                   ignore (sign_up b t "bob") ;
                   let register =
                     add_passkey ~flags:(up lor at) b t (authenticator "bob 1")
                   in
                   ignore (sign_out b t) ;
                   let sign = sign_in ~flags:up b t a in
                   (register, sign, load b t) )
             in
             equal status Status.bad_request register.status ;
             equal status Status.bad_request sign.status ;
             hello "visitor" page ;
             equal (list string)
               [ "passkey registration refused: Not_verified"
               ; "passkey sign-in refused: Not_verified" ]
               !reports )
       ; test "a counter that does not grow: cloned, refused and reported"
           (fun () ->
             let reports, report = new_reports () in
             let first, copy, page =
               run ~report (fun t ->
                   let b, a = with_passkey t in
                   (* A copy of the authenticator, with the same key and the
                      same counter. *)
                   let copy = {a with count= a.count} in
                   let first = sign_in b t a in
                   ignore (sign_out b t) ;
                   let copy = sign_in b t copy in
                   (first, copy, load b t) )
             in
             equal status Status.ok first.status ;
             equal status Status.bad_request copy.status ;
             equal string refused copy.body ;
             hello "visitor" page ;
             equal (list string)
               [ "passkey sign-in refused: the counter did not grow: the \
                  authenticator can be a copy" ]
               !reports )
       ; test "a passkey that syncs sends 0 each time: not cloned" (fun () ->
             let first, second =
               run (fun t ->
                   let b, a = with_passkey ~step:0 t in
                   let first = sign_in b t a in
                   ignore (sign_out b t) ;
                   (first, sign_in b t a) )
             in
             equal status Status.ok first.status ;
             equal status Status.ok second.status )
       ; test "the routes of a visitor, from another site: 403" (fun () ->
             let cross = [("Sec-Fetch-Site", "cross-site")] in
             let signup, options, sign, page =
               run (fun t ->
                   let _, a = with_passkey t in
                   let b = browser () in
                   let signup = sign_up ~headers:cross b t "bob" in
                   let options =
                     post ~headers:cross b t "/passkey/sign-in-options" ""
                   in
                   let own = post b t "/passkey/sign-in-options" "" in
                   let sign =
                     post ~headers:cross b t "/passkey/sign-in" (get a own.body)
                   in
                   (signup, options, sign, load b t) )
             in
             equal status Status.forbidden signup.status ;
             equal (list string) [] (set_cookies signup) ;
             equal status Status.forbidden options.status ;
             equal (list string) [] (set_cookies options) ;
             equal status Status.forbidden sign.status ;
             hello "visitor" page )
       ; test "the script: the JSON of WebAuthn, the token, textContent"
           (fun () ->
             let r = run (fun t -> load (browser ()) t) in
             List.iter
               (fun sub -> contains ~sub r.body)
               [ "PublicKeyCredential.parseCreationOptionsFromJSON(options)"
               ; "PublicKeyCredential.parseRequestOptionsFromJSON(options)"
               ; "JSON.stringify(credential.toJSON())"
               ; {|if (token) headers["X-CSRF-Token"] = token;|}
               ; "message.textContent = e.message;" ] ;
             not_contains ~sub:"innerHTML" r.body )
       ; test "the sign-up and the options share a bucket of 10, +1 each 6 s"
           (fun () ->
             run ~manual:true (fun t ->
                 let b = browser () in
                 equal status Status.see_other (sign_up b t "ada").status ;
                 for _ = 1 to 8 do
                   equal status Status.ok
                     (post b t "/passkey/sign-in-options" "").status
                 done ;
                 equal status Status.ok
                   (post b t "/passkey/register-options" "").status ;
                 let full = post b t "/passkey/sign-in-options" "" in
                 equal status Status.too_many_requests full.status ;
                 equal status Status.too_many_requests
                   (sign_up (browser ()) t "bob").status ;
                 equal status Status.too_many_requests
                   (post b t "/passkey/register-options" "").status ;
                 T.advance t (Span.s 5) ;
                 equal status Status.too_many_requests
                   (post b t "/passkey/sign-in-options" "").status ;
                 T.advance t (Span.s 1) ;
                 equal status Status.ok
                   (post b t "/passkey/sign-in-options" "").status ;
                 equal status Status.too_many_requests
                   (post b t "/passkey/sign-in-options" "").status ) )
       ; test "a body over 64 KiB: 413" (fun () ->
             let r =
               run (fun t ->
                   post (browser ()) t "/passkey/sign-in"
                     (String.make ((64 * 1024) + 1) ' ') )
             in
             equal status Status.content_too_large r.status )
       ; test "the sign-out ends the session" (fun () ->
             let r, page =
               run (fun t ->
                   let b = browser () in
                   ignore (sign_up b t "ada") ;
                   let r = sign_out b t in
                   (r, load b t) )
             in
             equal status Status.see_other r.status ;
             equal (option string) (Some "/") (header "location" r) ;
             hello "visitor" page )
       ; test "a new handler, as after a restart, forgets the passkeys"
           (fun () ->
             let reports, report = new_reports () in
             let a = run (fun t -> snd (with_passkey t)) in
             let r = run ~report (fun t -> sign_in (browser ()) t a) in
             equal status Status.bad_request r.status ;
             equal (list string)
               ["passkey sign-in refused: Unknown_credential"]
               !reports )
       ; test "add_credential refuses an ID that the table has" (fun () ->
             let db = App.db () in
             let stored n : Caravan_auth.Passkey.stored =
               { public_key= "key"
               ; sign_count= n
               ; user_handle= "user"
               ; backup_eligible= false }
             in
             is_true (App.add_credential db "id" (stored 1)) ;
             is_false (App.add_credential db "id" (stored 2)) ;
             equal (option int) (Some 1)
               (Option.map
                  (fun (s : Caravan_auth.Passkey.stored) -> s.sign_count)
                  (App.credential db "id") ) ) ]
