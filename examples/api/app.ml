(* A JSON API with four authentication schemes: API keys in Authorization or in
   X-API-Key, JSON Web Tokens (JWTs) of an identity provider, and a token for an
   internal service. Scopes protect the orders, and rate limits protect the
   store of the keys.

   Run dune exec examples/api/main.exe. If the environment variable JWKS has the
   JWK Set of an identity provider, the API verifies the tokens of that
   provider. If JWT_SECRET has a shared secret, the API verifies the HS256
   tokens of that secret (see keys_of_env). If JWKS and JWT_SECRET are both set,
   main uses JWKS only. With JWKS or JWT_SECRET, INTERNAL_TOKEN must have the
   internal token. If INTERNAL_TOKEN is set, it must have 32 bytes or more. If
   one of these values is missing or bad, main writes the problem on stderr and
   stops with the exit code 2.

   With neither JWKS nor JWT_SECRET, main makes a development key pair (never
   for production), and prints T, a token for the user ada with the scope
   orders:read. The token expires in 1 h. If INTERNAL_TOKEN is not set either,
   main also prints I, a random internal token. Set T and I as main prints them,
   and these shell variables:

   S=127.0.0.1:8080 A='Authorization: Bearer'

   Each user has a burst of 5 requests. If you paste many lines at one time,
   wait 1 s between the requests of one user.

   A request with the token reads /api/me. The body shows the user and the
   scheme:

   curl -i -H "$A $T" $S/api/me

   A request with no credentials gets 401 with WWW-Authenticate: Bearer
   realm="api":

   curl -i $S/api/me

   POST /keys makes an API key for the user of the token. The response shows the
   key one time, with Cache-Control: no-store. The table of the keys keeps only
   the hash of the key. Set K to the key. The ID of this first key is 1. The key
   expires after 90 days:

   curl -i -X POST -H "$A $T" $S/keys

   The key works in Authorization and in X-API-Key. The key has the user and the
   scope of the token:

   curl -i -H "$A $K" $S/api/me

   curl -i -H "X-API-Key: $K" $S/api/me

   Only the user of a token can make a key. Thus a leaked key cannot make more
   keys, and a request with a key gets 403:

   curl -i -X POST -H "$A $K" $S/keys

   The scope orders:read lets the user read the orders, but a POST must have the
   scope orders:write. Thus the POST gets 403:

   curl -i -H "$A $T" $S/api/orders

   curl -i -X POST -H "$A $T" $S/api/orders

   A token with a "." is never an API key. Thus the bearer scheme verifies this
   token as a JWT, and refuses it: 401 with error="invalid_token". The report on
   stderr has "Bearer: Malformed" and the reason, but not the token:

   curl -i -H "$A $K.x" $S/api/me

   DELETE revokes the key of the user: 204. Then the key gets 401 in the two
   fields. The key of another user gets 404:

   curl -i -X DELETE -H "$A $T" $S/keys/1

   curl -i -H "$A $K" $S/api/me

   curl -i -H "X-API-Key: $K" $S/api/me

   Anyone with the text of a key can revoke it, with no credentials: for example
   a secret scanner that finds a leaked key. Make a new key, set K to it, and
   send it in the body: 204. A key that is not in the table gets 404:

   curl -i -X DELETE -d "$K" $S/keys

   After 1 h and the leeway of 60 s, the token is expired: 401 with
   WWW-Authenticate: Bearer, and the report on stderr is "Bearer: Expired".

   Each user has a burst of 5 requests, and one more request each second. The
   keys and the tokens of a user share the burst. Wait 5 s, thus the bucket is
   full. Then the sixth request in one second gets 429:

   curl -so /dev/null -w '%{http_code}\n' -H "$A $T" "$S/api/me?n=[1-6]"

   Each client address has a burst of 50 requests, and one more request each 100
   ms. Wait 5 s. Then the 51st request in one second gets 429, also with no
   credentials:

   curl -so /dev/null -w '%{http_code}\n' "$S/?n=[1-51]" | sort | uniq -c

   The internal service sends its token in X-Internal-Token: 200 on /internal.
   Another value gets 401 with WWW-Authenticate: Internal. A user of another
   scheme gets 403:

   curl -i -H "X-Internal-Token: $I" $S/internal

   curl -i -H "X-Internal-Token: bad" $S/internal

   curl -i -H "$A $T" $S/internal *)

open Weft
open Caravan
module Auth = Caravan_auth

(* The issuer and the audience of the tokens. With the JWK Set of a real
   identity provider, put its issuer and the audience of this API here. *)
let issuer = "https://id.example"

let audience = "api://orders"

(* The keys of this API start with "orders_". A secret scanner finds a leaked
   key by this prefix. *)
let key_format = Auth.Api_key.format ~prefix:"orders_"

(* {1 The table of the keys} *)

(* A row of the table. The row has the hash of the key, never the key: a copy of
   the table gives no key. *)
type row =
  {id: string; owner: string; scope: string; hash: string; expires: Clock.posix}

(* A key expires 90 days after its creation. *)
let lifetime = Span.s (90 * 24 * 3600)

(* A table in memory, for the example. A real API keeps the rows in a database,
   with an index on the hash. The domains of the server share the table: thus a
   mutex protects it. [reads] counts the reads by the hash, for the tests. *)
type store =
  { mutex: Mutex.t
  ; rows: (string, row) Hashtbl.t
  ; mutable next: int
  ; mutable reads: int }

let store () =
  {mutex= Mutex.create (); rows= Hashtbl.create 16; next= 1; reads= 0}

let add store ~owner ~scope hash =
  let expires = Option.get (Time.add Posix (Time.now Posix) lifetime) in
  Mutex.protect store.mutex (fun () ->
      let id = string_of_int store.next in
      store.next <- store.next + 1 ;
      Hashtbl.replace store.rows hash {id; owner; scope; hash; expires} ;
      id )

let find store hash =
  Mutex.protect store.mutex (fun () ->
      store.reads <- store.reads + 1 ;
      Hashtbl.find_opt store.rows hash )

(* [remove store hash] removes the row of [hash], if any, and gives it. *)
let remove store hash =
  Mutex.protect store.mutex (fun () ->
      let row = Hashtbl.find_opt store.rows hash in
      Hashtbl.remove store.rows hash ;
      row )

(* [remove_where store f] removes the row that [f] accepts, if any, and gives
   it. It reads each row: a database uses an index. *)
let remove_where store f =
  Mutex.protect store.mutex (fun () ->
      let row =
        Hashtbl.fold
          (fun _ r found -> if f r then Some r else found)
          store.rows None
      in
      Option.iter (fun r -> Hashtbl.remove store.rows r.hash) row ;
      row )

(* The user of a key: the owner, with the scope of the token that made the key.
   A failure of a real database must raise: the server then sends 500. An Error
   gives 401, and the client can then discard a good key. *)
let lookup store hash =
  match find store hash with
  | Some row
    when Span.compare (Time.diff Posix row.expires (Time.now Posix)) Span.zero
         > 0 ->
      Ok (Auth.User.make ~claims:[("scope", row.scope)] row.owner)
  | Some _ ->
      Error "an expired key"
  | None ->
      Error "an unknown key"

(* {1 The schemes} *)

(* The scheme of the internal service: a shared token in X-Internal-Token.
   Eqaf.equal compares the two tokens in a time that does not change with the
   position of the first different byte. Thus the time of a response does not
   tell an attacker the token byte by byte. The length of the token is not a
   secret. Two X-Internal-Token fields get Failed. The challenge gives 401 with
   WWW-Authenticate: Internal, because each 401 must have this field. *)
let internal_scheme token =
  Auth.Scheme.make ~name:"Internal"
    ~authenticate:(fun req ->
      match Headers.find_all "x-internal-token" (Request.headers req) with
      | [] ->
          Auth.No_credentials
      | [v] when Eqaf.equal v token ->
          Authenticated (Auth.User.make "internal-service")
      | _ ->
          Failed "a bad internal token" )
    ~challenge:(fun ~failure:_ _req ->
      Response.string ~status:Status.unauthorized
        ~headers:(Headers.of_list [("WWW-Authenticate", "Internal")])
        "Unauthorized\n" )

(* The order of the schemes is important. The API key scheme on Authorization is
   before the bearer scheme: it gives No_credentials for a JWT, but the bearer
   scheme gives Failed for a key. The cache keeps the user of a key for 30 s.
   Give the cache to one scheme only: here, the scheme on Authorization. Each
   request with X-API-Key reads the store. In Authorization, the cache can keep
   an expired key for 30 s more. *)
let schemes ~store ~cache ~jwt_keys ~internal_token =
  let lookup = lookup store in
  [ Auth.Api_key.scheme ~cache ~lookup key_format
  ; Auth.Api_key.scheme ~name:"ApiKeyHeader" ~field:"X-API-Key" ~lookup
      key_format
  ; Auth.Scheme.bearer ~issuer:(One_of [issuer]) ~audience:(One_of [audience])
      ~keys:(fun () -> jwt_keys)
      ()
  ; internal_scheme internal_token ]

(* {1 The handlers} *)

let json ?status ?(headers = Headers.empty) v =
  Response.string ?status ~headers ~content_type:"application/json"
    (Yojson.Safe.to_string v)

(* The user of a request after require. *)
let user req = Option.get (Auth.user req)

let me req =
  let u = user req in
  json
    (`Assoc
       [ ("user", `String (Auth.User.id u))
       ; ("scheme", `String (Auth.User.scheme u)) ] )

let orders _req = json (`List [`Assoc [("id", `Int 1); ("item", `String "tea")]])

let create_order _req = json ~status:Status.created (`Assoc [("id", `Int 2)])

(* POST /keys shows the key one time. No cache keeps the response. *)
let create_key store req =
  let u = user req in
  let k = Auth.Api_key.generate key_format in
  let scope = Option.value ~default:"" (Auth.User.claim "scope" u) in
  let id = add store ~owner:(Auth.User.id u) ~scope k.hash in
  json ~status:Status.created
    ~headers:(Headers.of_list [("Cache-Control", "no-store")])
    (`Assoc [("id", `String id); ("key", `String k.key)])

let error status text = json ~status (`Assoc [("error", `String text)])

(* A revocation removes the row first, then the entry of the cache. In the other
   order, a lookup between the two steps can put the key back into the cache. *)
let revoke cache removed =
  match removed with
  | Some row ->
      Auth.Api_key.forget cache row.hash ;
      Response.empty ()
  | None ->
      error Status.not_found "no such key"

(* DELETE /keys/:id revokes a key of the user. *)
let revoke_id store cache req =
  let owner = Auth.User.id (user req) in
  let id = Caravan_router.param "id" req in
  revoke cache (remove_where store (fun r -> r.id = id && r.owner = owner))

(* DELETE /keys revokes the key in the body. Api_key.hash gives None for a text
   that is not a key of this API: 400. The hash finds the row. *)
let revoke_text store cache req =
  match
    Auth.Api_key.hash key_format (Body.to_string ~max:256 (Request.body req))
  with
  | Some hash ->
      revoke cache (remove store hash)
  | None | (exception Body.Too_large) ->
      error Status.bad_request "not a key"

let internal _req = json (`Assoc [("status", `String "ok")])

(* {1 The policies} *)

(* [scheme_is name] accepts the users of the scheme [name]. The identity
   provider can give a token any roles. Thus /internal checks the scheme of the
   user, not a role. *)
let scheme_is name u = Auth.User.scheme u = name

(* [api ?policy h] is [h] for the users that [policy] accepts. A request with no
   user gets the Bearer challenge. *)
let api ?policy h = Auth.require ?policy ~challenge:"Bearer" () h

(* {1 The application} *)

(* The text of a failure of the bearer scheme is the Jwt error, with
   Jwt.pp_error. A failure text never has the token or the key. *)
let report_failures ~report next req =
  ( match Auth.failure req with
  | Some (scheme, text) ->
      report (scheme ^ ": " ^ text)
  | None ->
      () ) ;
  next req

(* The rate limit of each user. A request with no user has no key, thus no limit
   of this middleware. *)
let user_key req = Option.map Auth.User.id (Auth.user req)

(* [make ?store ~jwt_keys ~internal_token ~report ()] is the API. [store] is the
   table of the keys (a new table by default). [jwt_keys] verify the tokens.
   [report] gets a line for each failure of a scheme.

   The rate limit of each client is before Caravan_auth.middleware: a client
   with many random keys cannot do many reads of the store. The rate limit of
   each user is after it, because its key is the user. *)
let make ?(store = store ()) ~jwt_keys ~internal_token ~report () =
  (* A short token is easy to guess, and an empty token lets in each request
     with an empty X-Internal-Token. *)
  if String.length internal_token < 32 then
    invalid_arg "App.make: an internal token of less than 32 bytes" ;
  let cache = Auth.Api_key.cache (Span.s 30) in
  let only_tokens = scheme_is "Bearer" in
  let routes =
    Caravan_router.(
      handler
        [ get "/api/me" (api me)
        ; get "/api/orders" (api ~policy:(Auth.scope "orders:read") orders)
        ; post "/api/orders"
            (api ~policy:(Auth.scope "orders:write") create_order)
        ; post "/keys" (api ~policy:only_tokens (create_key store))
        ; delete "/keys/:id" (api ~policy:only_tokens (revoke_id store cache))
        ; delete "/keys" (revoke_text store cache)
        ; get "/internal"
            (Auth.require ~challenge:"Internal" ~policy:(scheme_is "Internal")
               () internal ) ] )
  in
  Handler.compose
    [ Caravan_middleware.Rate_limit.middleware ~burst:50 ~every:(Span.ms 100) ()
    ; Auth.middleware
        ~schemes:(schemes ~store ~cache ~jwt_keys ~internal_token)
        ()
    ; report_failures ~report
    ; Caravan_middleware.Rate_limit.middleware ~burst:5 ~every:(Span.s 1)
        ~key:user_key () ]
    routes

(* {1 The keys from the environment} *)

(* The keys of the tokens and the internal token. [Development] tells main to
   make a development key pair. It has INTERNAL_TOKEN, if it is set. *)
type keys =
  | From_env of {jwt_keys: Auth.Jwt.Key.t list; internal_token: string}
  | Development of {internal_token: string option}

(* [keys_of_env getenv] reads JWKS, JWT_SECRET and INTERNAL_TOKEN with [getenv],
   for example Sys.getenv_opt. If JWKS and JWT_SECRET are both set, it uses JWKS
   and ignores JWT_SECRET. [Error] has the problem, for main. If JWKS or
   JWT_SECRET is set, a missing INTERNAL_TOKEN gives [Error] for each value of
   JWKS and JWT_SECRET.

   JWT_SECRET is text, because an environment variable cannot hold a NUL byte.
   The bytes of the text are the secret, and they must be 32 or more. Make the
   text with openssl rand -base64 48: 64 characters from 48 random bytes. Do not
   use a password. If a provider gives the secret as the k member of a JWK, use
   Jwt.Key.of_secret_base64url. If a provider gives a PEM key in place of a JWK
   Set, use Jwt.Key.of_pem. *)
let keys_of_env getenv =
  match (getenv "JWKS", getenv "JWT_SECRET", getenv "INTERNAL_TOKEN") with
  | Some _, _, None | None, Some _, None ->
      Error "JWKS or JWT_SECRET is set, thus INTERNAL_TOKEN must be set too."
  | _, _, Some token when String.length token < 32 ->
      Error "INTERNAL_TOKEN must have 32 bytes or more."
  | Some text, _, Some internal_token -> (
    match Auth.Jwt.Key.of_jwks text with
    | Ok [] ->
        Error "JWKS: no key"
    | Ok jwt_keys ->
        Ok (From_env {jwt_keys; internal_token})
    | Error e ->
        Error ("JWKS: " ^ e) )
  | None, Some secret, Some internal_token -> (
    match Auth.Jwt.Key.of_secret secret with
    | key ->
        Ok (From_env {jwt_keys= [key]; internal_token})
    | exception Invalid_argument _ ->
        Error "JWT_SECRET must have 32 bytes or more." )
  | None, None, internal_token ->
      Ok (Development {internal_token})
