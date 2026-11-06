(* The tests of the api example. Each test runs the handler through the real
   server and client of caravan, in the simulator of weft, on one domain. Each
   run first sets a seeded generator, then makes a new Dev_jwt key pair and a
   new application. The posix clock of the simulator is t0. *)

open Windtrap
open Weft
open Caravan
open Example_dev
module T = Caravan_testing
module J = Caravan_auth.Jwt
module App = Example_api.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan api tests"
       (module Mirage_crypto_rng.Fortuna) )

(* The time of the tests: 2025-06-15. *)
let t0 = 1_750_000_000

let internal_token = "the internal token of the tests, 0123456789"

type env = {t: T.t; dev: Dev_jwt.t; reports: string list ref; store: App.store}

(* [at t s] sets the posix clock of the simulator to [s] seconds after t0. *)
let at t s =
  Weft_sim.set_posix (T.sim t) (Time.posix_of_unix_s (float_of_int (t0 + s)))

(* [run ~jwt_keys f] calls [f] with a new application. [jwt_keys] are the keys
   of the JWK Set of the Dev_jwt key pair by default. *)
let run ?jwt_keys f =
  seed () ;
  let dev = Dev_jwt.make () in
  let jwt_keys =
    match jwt_keys with
    | Some keys ->
        keys
    | None ->
        Result.get_ok (J.Key.of_jwks (Dev_jwt.jwks dev))
  in
  let reports = ref [] and store = App.store () in
  let report l = reports := !reports @ [l] in
  let app = App.make ~store ~jwt_keys ~internal_token ~report () in
  T.run app (fun t ->
      at t 0 ;
      f {t; dev; reports; store} )

(* [claims sub] is the JSON text of the claims of a token for [sub]. The token
   expires at [exp] (one hour after t0 by default). *)
let claims ?(scope = "orders:read") ?(exp = t0 + 3600) ?(roles = []) sub =
  let roles =
    if roles = [] then []
    else [("roles", `List (List.map (fun r -> `String r) roles))]
  in
  Yojson.Safe.to_string
    (`Assoc
       ( [ ("iss", `String App.issuer)
         ; ("aud", `String App.audience)
         ; ("sub", `String sub)
         ; ("scope", `String scope)
         ; ("exp", `Int exp) ]
       @ roles ) )

(* [token e sub] is a token of the identity provider for [sub], signed with the
   Dev_jwt key pair (ES256). *)
let token ?scope ?exp ?roles e sub =
  Dev_jwt.sign e.dev (claims ?scope ?exp ?roles sub)

let base64url s =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

(* [hs256 secret claims] is a token signed with HMAC-SHA256 and [secret], as a
   provider with a shared secret signs it. *)
let hs256 secret claims =
  let input =
    base64url {|{"alg":"HS256","typ":"JWT"}|} ^ "." ^ base64url claims
  in
  let mac = Digestif.SHA256.(to_raw_string (hmac_string ~key:secret input)) in
  input ^ "." ^ base64url mac

(* The fields of the curl lines: "$A $T", "X-API-Key: $K" and the others. *)
let bearer v = ("Authorization", "Bearer " ^ v)

let x_api_key v = ("X-API-Key", v)

let x_internal v = ("X-Internal-Token", v)

let call ?(fields = []) e meth path =
  T.fetch e.t (Request.make ~headers:(Headers.of_list fields) meth path)

let header name (r : T.reply) = Headers.find name r.headers

let expect ?(code = Status.ok) ?body (r : T.reply) =
  equal ~msg:"status" status code r.status ;
  Option.iter (fun b -> equal ~msg:"body" string b r.body) body

let me user scheme = Printf.sprintf {|{"user":"%s","scheme":"%s"}|} user scheme

(* [new_key e tok] is the ID and the key that POST /keys gives to [tok]. *)
let new_key e tok =
  let r = call ~fields:[bearer tok] e POST "/keys" in
  expect ~code:Status.created r ;
  match Yojson.Safe.from_string r.body with
  | `Assoc [("id", `String id); ("key", `String key)] ->
      (id, key)
  | _ ->
      failf "not a key: %s" r.body

(* [keys_of_env vars] is App.keys_of_env with the environment [vars]. *)
let keys_of_env vars = App.keys_of_env (fun name -> List.assoc_opt name vars)

(* [show r] is a text of a result of keys_of_env, for the checks. *)
let show = function
  | Ok (App.From_env {jwt_keys; internal_token}) ->
      let n = List.length jwt_keys in
      Printf.sprintf "From_env %d key%s, %s" n
        (if n = 1 then "" else "s")
        internal_token
  | Ok (Development {internal_token}) ->
      "Development " ^ Option.value ~default:"None" internal_token
  | Error e ->
      "Error " ^ e

(* [from_env vars] is the keys and the internal token of From_env. *)
let from_env vars =
  match keys_of_env vars with
  | Ok (App.From_env {jwt_keys; internal_token}) ->
      (jwt_keys, internal_token)
  | r ->
      failf "not From_env: %s" (show r)

(* [statuses jwt_keys tokens] is the status of GET /api/me for each of [tokens],
   with the application of [jwt_keys]. *)
let statuses jwt_keys tokens =
  run ~jwt_keys (fun e ->
      List.map
        (fun tok -> (call ~fields:[bearer tok] e GET "/api/me").status)
        tokens )

let challenge = {|Bearer realm="api"|}

let bad_token = {|Bearer realm="api", error="invalid_token"|}

let () =
  exit
  @@ Windtrap.run "api"
       [ test "a token reads /api/me: the user and the scheme" (fun () ->
             run (fun e ->
                 let ada = token e "ada" in
                 let r = call ~fields:[bearer ada] e GET "/api/me" in
                 expect ~body:(me "ada" "Bearer") r ;
                 equal (option string) (Some "application/json")
                   (header "content-type" r) ) )
       ; test "keys_of_env: JWKS gives the keys of the JWK Set" (fun () ->
             seed () ;
             let provider = Dev_jwt.make () and other = Dev_jwt.make () in
             let jwt_keys, token =
               from_env
                 [ ("JWKS", Dev_jwt.jwks provider)
                 ; ("INTERNAL_TOKEN", internal_token) ]
             in
             equal string internal_token token ;
             equal (list status)
               [Status.ok; Status.unauthorized]
               (statuses jwt_keys
                  [ Dev_jwt.sign provider (claims "ada")
                  ; Dev_jwt.sign other (claims "ada") ] ) )
       ; test "keys_of_env: JWT_SECRET gives a key for the HS256 tokens"
           (fun () ->
             (* The form of the text of openssl rand -base64 48. *)
             let secret = Base64.encode_string (String.init 48 Char.chr) in
             equal int 64 (String.length secret) ;
             let jwt_keys, _ =
               from_env
                 [("JWT_SECRET", secret); ("INTERNAL_TOKEN", internal_token)]
             in
             equal (list status)
               [Status.ok; Status.unauthorized]
               (statuses jwt_keys
                  [ hs256 secret (claims "ada")
                  ; hs256 (String.make 64 'x') (claims "ada") ] ) ;
             let secret_of n =
               show
                 (keys_of_env
                    [ ("JWT_SECRET", String.make n 's')
                    ; ("INTERNAL_TOKEN", internal_token) ] )
             in
             equal string "Error JWT_SECRET must have 32 bytes or more."
               (secret_of 31) ;
             equal string ("From_env 1 key, " ^ internal_token) (secret_of 32) )
       ; test "keys_of_env: with JWKS and JWT_SECRET, JWKS wins" (fun () ->
             seed () ;
             let provider = Dev_jwt.make () in
             let secret = String.make 32 's' in
             let env secret =
               [ ("JWKS", Dev_jwt.jwks provider)
               ; ("JWT_SECRET", secret)
               ; ("INTERNAL_TOKEN", internal_token) ]
             in
             let jwt_keys, _ = from_env (env secret) in
             equal (list status)
               [Status.ok; Status.unauthorized]
               (statuses jwt_keys
                  [ Dev_jwt.sign provider (claims "ada")
                  ; hs256 secret (claims "ada") ] ) ;
             (* main does not read a JWT_SECRET that is too short. *)
             equal string
               ("From_env 1 key, " ^ internal_token)
               (show (keys_of_env (env "x"))) )
       ; test "keys_of_env: with neither, the development mode" (fun () ->
             equal string "Development None" (show (keys_of_env [])) ;
             equal string
               ("Development " ^ internal_token)
               (show (keys_of_env [("INTERNAL_TOKEN", internal_token)])) )
       ; test "keys_of_env: a missing or short INTERNAL_TOKEN, a bad JWKS"
           (fun () ->
             let missing =
               "Error JWKS or JWT_SECRET is set, thus INTERNAL_TOKEN must be \
                set too."
             and short = "Error INTERNAL_TOKEN must have 32 bytes or more."
             and token n = ("INTERNAL_TOKEN", String.make n 't') in
             let check want env = equal string want (show (keys_of_env env)) in
             check missing [("JWKS", "x")] ;
             check missing [("JWT_SECRET", "x")] ;
             check short [("JWKS", "x"); token 31] ;
             check short [("JWT_SECRET", String.make 32 's'); token 31] ;
             check short [token 31] ;
             check ("Development " ^ String.make 32 't') [token 32] ;
             (* The error does not show the text of JWKS. *)
             check "Error JWKS: not JSON" [("JWKS", "SECRET"); token 32] ;
             check "Error JWKS: no key" [("JWKS", {|{"keys":[]}|}); token 32] )
       ; test "no credentials: 401 with the Bearer challenge" (fun () ->
             run (fun e ->
                 let r = call e GET "/api/me" in
                 expect ~code:Status.unauthorized r ;
                 equal (option string) (Some challenge)
                   (header "www-authenticate" r) ) )
       ; test "POST /keys shows the key one time, with no-store" (fun () ->
             run (fun e ->
                 let r = call ~fields:[bearer (token e "ada")] e POST "/keys" in
                 expect ~code:Status.created r ;
                 equal (option string) (Some "no-store")
                   (header "cache-control" r) ;
                 match Yojson.Safe.from_string r.body with
                 | `Assoc [("id", `String "1"); ("key", `String key)] ->
                     is_true ~msg:"the prefix"
                       (String.starts_with ~prefix:"orders_" key)
                 | _ ->
                     failf "not a key: %s" r.body ) )
       ; test "a key works in Authorization and in X-API-Key" (fun () ->
             run (fun e ->
                 let _, k = new_key e (token e "ada") in
                 expect ~body:(me "ada" "ApiKey")
                   (call ~fields:[bearer k] e GET "/api/me") ;
                 expect ~body:(me "ada" "ApiKeyHeader")
                   (call ~fields:[x_api_key k] e GET "/api/me") ) )
       ; test "a key has the scope of the token that made it" (fun () ->
             run (fun e ->
                 let _, reader = new_key e (token e "ada") in
                 let _, writer =
                   new_key e (token ~scope:"orders:read orders:write" e "bob")
                 in
                 expect (call ~fields:[x_api_key reader] e GET "/api/orders") ;
                 expect ~code:Status.forbidden
                   (call ~fields:[x_api_key reader] e POST "/api/orders") ;
                 expect ~code:Status.created
                   (call ~fields:[x_api_key writer] e POST "/api/orders") ) )
       ; test "a request with a key cannot make a key: 403" (fun () ->
             run (fun e ->
                 let _, k = new_key e (token e "ada") in
                 expect ~code:Status.forbidden
                   (call ~fields:[bearer k] e POST "/keys") ;
                 expect ~code:Status.forbidden
                   (call ~fields:[x_api_key k] e POST "/keys") ) )
       ; test "orders:read reads the orders, a POST must have orders:write"
           (fun () ->
             run (fun e ->
                 let reader = token e "ada" in
                 expect ~body:{|[{"id":1,"item":"tea"}]|}
                   (call ~fields:[bearer reader] e GET "/api/orders") ;
                 expect ~code:Status.forbidden
                   (call ~fields:[bearer reader] e POST "/api/orders") ;
                 let writer = token ~scope:"orders:write" e "ada" in
                 expect ~code:Status.created
                   (call ~fields:[bearer writer] e POST "/api/orders") ) )
       ; test "a token with a . is a JWT, not a key" (fun () ->
             run (fun e ->
                 let _, k = new_key e (token e "ada") in
                 let r = call ~fields:[bearer (k ^ ".x")] e GET "/api/me" in
                 expect ~code:Status.unauthorized r ;
                 equal (option string) (Some bad_token)
                   (header "www-authenticate" r) ;
                 match !(e.reports) with
                 | [line] ->
                     is_true ~msg:line
                       (String.starts_with ~prefix:"Bearer: Malformed" line) ;
                     not_contains ~msg:"the key" ~sub:k line
                 | l ->
                     failf "the reports: %s" (String.concat "; " l) ) )
       ; test "a revoked key gets 401 in the two fields" (fun () ->
             run (fun e ->
                 let ada = token e "ada" in
                 let id, k = new_key e ada in
                 (* The first request puts the key into the cache. *)
                 expect (call ~fields:[bearer k] e GET "/api/me") ;
                 expect (call ~fields:[x_api_key k] e GET "/api/me") ;
                 expect ~code:Status.no_content
                   (call ~fields:[bearer ada] e DELETE ("/keys/" ^ id)) ;
                 let r = call ~fields:[bearer k] e GET "/api/me" in
                 expect ~code:Status.unauthorized r ;
                 equal (option string) (Some bad_token)
                   (header "www-authenticate" r) ;
                 expect ~code:Status.unauthorized
                   (call ~fields:[x_api_key k] e GET "/api/me") ) )
       ; test "the key of another user gets 404, and the key works" (fun () ->
             run (fun e ->
                 let id, k = new_key e (token e "ada") in
                 let bob = token e "bob" in
                 expect ~code:Status.not_found
                   (call ~fields:[bearer bob] e DELETE ("/keys/" ^ id)) ;
                 expect (call ~fields:[bearer k] e GET "/api/me") ) )
       ; test "a token expires after 1 h and the leeway of 60 s" (fun () ->
             run (fun e ->
                 let ada = token e "ada" in
                 at e.t (3600 + 59) ;
                 expect (call ~fields:[bearer ada] e GET "/api/me") ;
                 at e.t (3600 + 60) ;
                 let r = call ~fields:[bearer ada] e GET "/api/me" in
                 expect ~code:Status.unauthorized r ;
                 equal (option string) (Some bad_token)
                   (header "www-authenticate" r) ;
                 equal (list string) ["Bearer: Expired"] !(e.reports) ) )
       ; test "a key expires after 90 days" (fun () ->
             run (fun e ->
                 let _, k = new_key e (token e "ada") in
                 let day = 24 * 3600 in
                 at e.t ((90 * day) - 1) ;
                 expect (call ~fields:[x_api_key k] e GET "/api/me") ;
                 at e.t (90 * day) ;
                 expect ~code:Status.unauthorized
                   (call ~fields:[x_api_key k] e GET "/api/me") ;
                 equal (list string)
                   ["ApiKeyHeader: an expired key"]
                   !(e.reports) ) )
       ; test "the cache keeps the user of a key for 30 s, not for X-API-Key"
           (fun () ->
             run (fun e ->
                 let _, k = new_key e (token e "ada") in
                 let me f = expect (call ~fields:[f k] e GET "/api/me") in
                 me bearer ;
                 me bearer ;
                 equal ~msg:"two requests" int 1 e.store.reads ;
                 Time.sleep Mono (Span.s 31) ;
                 me bearer ;
                 equal ~msg:"after 31 s" int 2 e.store.reads ;
                 me x_api_key ;
                 me x_api_key ;
                 equal ~msg:"X-API-Key" int 4 e.store.reads ) )
       ; test "DELETE /keys revokes the key in the body, with no credentials"
           (fun () ->
             run (fun e ->
                 let _, k = new_key e (token e "ada") in
                 (* The first request puts the key into the cache. *)
                 expect (call ~fields:[bearer k] e GET "/api/me") ;
                 let revoke body =
                   T.fetch e.t
                     (Request.make ~body:(Body.of_string body) DELETE "/keys")
                 in
                 expect ~code:Status.no_content (revoke k) ;
                 expect ~code:Status.unauthorized
                   (call ~fields:[bearer k] e GET "/api/me") ;
                 expect ~code:Status.not_found (revoke k) ;
                 expect ~code:Status.bad_request (revoke "orders_x") ) )
       ; test "DELETE /keys revokes only the key in the body" (fun () ->
             run (fun e ->
                 let _, ada = new_key e (token e "ada") in
                 let _, bob = new_key e (token e "bob") in
                 let revoke body =
                   T.fetch e.t
                     (Request.make ~body:(Body.of_string body) DELETE "/keys")
                 in
                 expect ~code:Status.no_content (revoke ada) ;
                 expect ~code:Status.unauthorized
                   (call ~fields:[bearer ada] e GET "/api/me") ;
                 expect ~body:(me "bob" "ApiKey")
                   (call ~fields:[bearer bob] e GET "/api/me") ;
                 (* A key of the format of this API that is not in the table:
                    404, and the key of bob stays. *)
                 let other =
                   (Caravan_auth.Api_key.generate App.key_format).key
                 in
                 expect ~code:Status.not_found (revoke other) ;
                 expect ~body:(me "bob" "ApiKey")
                   (call ~fields:[bearer bob] e GET "/api/me") ) )
       ; test "in Authorization, the cache keeps an expired key for 30 s more"
           (fun () ->
             run (fun e ->
                 let _, k = new_key e (token e "ada") in
                 let day = 24 * 3600 in
                 at e.t ((90 * day) - 1) ;
                 expect (call ~fields:[bearer k] e GET "/api/me") ;
                 at e.t (90 * day) ;
                 expect (call ~fields:[bearer k] e GET "/api/me") ;
                 expect ~code:Status.unauthorized
                   (call ~fields:[x_api_key k] e GET "/api/me") ;
                 Time.sleep Mono (Span.s 29) ;
                 expect (call ~fields:[bearer k] e GET "/api/me") ;
                 Time.sleep Mono (Span.s 2) ;
                 expect ~code:Status.unauthorized
                   (call ~fields:[bearer k] e GET "/api/me") ) )
       ; test "the sixth request of one user gets 429, then one each second"
           (fun () ->
             run (fun e ->
                 let ada = token e "ada" in
                 let me n =
                   call ~fields:[bearer ada] e GET ("/api/me?n=" ^ n)
                 in
                 List.iter (fun n -> expect (me n)) ["1"; "2"; "3"; "4"; "5"] ;
                 let r = me "6" in
                 expect ~code:Status.too_many_requests r ;
                 equal (option string) (Some "1") (header "retry-after" r) ;
                 Time.sleep Mono (Span.s 1) ;
                 expect (me "7") ;
                 expect ~code:Status.too_many_requests (me "8") ) )
       ; test "the keys and the tokens of a user share the burst" (fun () ->
             run (fun e ->
                 let ada = token e "ada" in
                 (* Request 1. *)
                 let _, k = new_key e ada in
                 (* Requests 2 to 5. *)
                 List.iter
                   (fun f -> expect (call ~fields:[f] e GET "/api/me"))
                   [bearer k; bearer k; x_api_key k; x_api_key k] ;
                 expect ~code:Status.too_many_requests
                   (call ~fields:[bearer ada] e GET "/api/me") ;
                 (* The limit is for each user, not for each client. *)
                 expect (call ~fields:[bearer (token e "bob")] e GET "/api/me") ) )
       ; test "the 51st request of a client gets 429, with no credentials"
           (fun () ->
             run (fun e ->
                 let home n = call e GET ("/?n=" ^ string_of_int n) in
                 for n = 1 to 50 do
                   expect ~code:Status.not_found (home n)
                 done ;
                 expect ~code:Status.too_many_requests (home 51) ;
                 Time.sleep Mono (Span.ms 100) ;
                 expect ~code:Status.not_found (home 52) ;
                 expect ~code:Status.too_many_requests (home 53) ) )
       ; test "the client limit is before the lookup of a key" (fun () ->
             run (fun e ->
                 for _ = 1 to 51 do
                   let k = (Caravan_auth.Api_key.generate App.key_format).key in
                   ignore (call ~fields:[x_api_key k] e GET "/api/me")
                 done ;
                 (* 50 lookups, and no lookup for the request with 429. *)
                 equal int 50 (List.length !(e.reports)) ;
                 equal (list string)
                   ["ApiKeyHeader: an unknown key"]
                   (List.sort_uniq compare !(e.reports)) ) )
       ; test "the internal token: 200 on /internal" (fun () ->
             run (fun e ->
                 let good = x_internal internal_token in
                 expect ~body:{|{"status":"ok"}|}
                   (call ~fields:[good] e GET "/internal") ;
                 let r = call ~fields:[x_internal "bad"] e GET "/internal" in
                 expect ~code:Status.unauthorized r ;
                 equal (option string) (Some "Internal")
                   (header "www-authenticate" r) ;
                 equal (list string)
                   ["Internal: a bad internal token"]
                   !(e.reports) ;
                 (* Two fields get Failed, also with the good token. *)
                 expect ~code:Status.unauthorized
                   (call ~fields:[good; good] e GET "/internal") ;
                 equal (list string)
                   ["Internal: a bad internal token"]
                   (List.tl !(e.reports)) ;
                 expect ~code:Status.forbidden
                   (call ~fields:[bearer (token e "ada")] e GET "/internal") ;
                 (* A role does not let a token in. *)
                 let t = token ~roles:["internal"] e "internal-service" in
                 expect ~code:Status.forbidden
                   (call ~fields:[bearer t] e GET "/internal") ) )
       ; test "make refuses an internal token of less than 32 bytes" (fun () ->
             let make n () =
               let (_ : Handler.t) =
                 App.make ~jwt_keys:[] ~internal_token:(String.make n 'a')
                   ~report:ignore ()
               in
               ()
             in
             List.iter
               (fun n ->
                 raises_match
                   (function Invalid_argument _ -> true | _ -> false)
                   (make n) )
               [0; 31] ;
             make 32 () ) ]
