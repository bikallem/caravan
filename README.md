# caravan

An HTTP/1.1 server, client and WebSocket library for OCaml 5, built on
[weft](https://github.com/bikallem/weft). weft is a concurrency library
with fibers, structured concurrency, cancellation, CML events and a
deterministic simulator. The design leaves room for HTTP/2, HTTP/3 and
a Traefik-like reverse proxy.

```ocaml
let () =
  Caravan.Server.run (module Weft_os) (fun _req ->
      Caravan.Response.string "Hello!\n" )
```

`Caravan.Server.run` serves the handler on 127.0.0.1:8080 until the
process gets SIGINT or SIGTERM. `Weft_os` (library `weft.os`) is the
world of weft for the OS of the build: Linux, macOS, FreeBSD or Windows.
On another system, `Weft_os.run` raises `Failure`. The build compiles
this example (examples/dune).

> **Status:** the HTTP/1.1 server, the client and WebSocket are complete
> but young, and their API can change. There is no proxy yet.

caravan and weft are not on opam. Get them from their repositories, or
use the package `caravan` of the Nix flake. Then add the libraries to
the dune stanza of your executable:

```dune
(executable
 (name main)
 (libraries caravan weft weft.os))
```

Add `caravan.client` for the client and `caravan.websocket` for
WebSocket.

## What the server does

- A strict RFC 9112 parser, written from scratch: CRLF only, no
  obs-fold, Content-Length and Transfer-Encoding rules that resist
  request smuggling, and limits on the request line, the header section
  and bodies (414, 431, 413).
- Streaming request and response bodies, with backpressure: chunked
  encoding with trailers, `Expect: 100-continue`, keep-alive and
  pipelining.
- Header, idle, read and write timeouts. A lingering close, so that a
  connection reset does not lose a response. A maximum number of
  connections.
- Graceful shutdown on SIGINT or SIGTERM: stop accepting, let requests
  in progress finish within a grace period, then cancel them. A second
  signal aborts. A third signal gets its default action, thus it also
  ends a handler that ignores the cancel. A handler can see the stop with
  `Conn_info.stopping`. `Server.run` does this in an OS world of weft.
- `Response.upgrade` hands the connection over after a `101 Switching
  Protocols` or a `2xx` to `CONNECT`, the basis for WebSocket and
  tunnels.
- A handler whose client disconnects is cancelled, along with what it
  started, such as an upstream request.

Handlers are plain functions, `Request.t -> Response.t`, and each
request runs in its own fiber, in parallel across domains.

A middleware is a handler that wraps another, and `Handler.compose`
puts middleware around a handler. Typed attributes (`Attrs`) let a
middleware give data to the handlers after it, for example the user of
a token. A handler can also give data back to an outer middleware in
its response, for example a route for an access log. Attributes never
go on the wire:

```ocaml
let user : string Attrs.key = Attrs.key ~name:"user" ()

let auth : Handler.middleware =
 fun next req -> next (Request.with_attr user "ann" req)

let hello req =
  match Request.find_attr user req with
  | Some name -> Response.string ("Hello, " ^ name ^ "!\n")
  | None -> Response.string ~status:Status.unauthorized ""

let handler = Handler.compose [auth] hello
```

`examples/middleware.ml` has a full example.

The libraries `caravan`, `caravan.client`, `caravan.websocket`,
`caravan.router`, `caravan.middleware`, `caravan.static`,
`caravan.tls` and `caravan.session` depend on no world of weft.
They run in each world, for example the OS world
(`Weft.run (module Weft_os)`) and the simulator of weft
(`Weft_sim.run`). `caravan.testing` runs only in the simulator.

## Routing

The `caravan.router` library gives a handler for each method and path
pattern:

```ocaml
let app =
  Caravan_router.(
    handler
      [ get "/" (fun _ -> Caravan.Response.string "home\n")
      ; group "/users"
          [ get "" (fun _ -> Caravan.Response.string "all users\n")
          ; get "/:id" (fun req ->
                Caravan.Response.string ("user " ^ param "id" req ^ "\n") ) ]
      ] )
```

- A parameter `:id` matches one segment, and `**` matches the rest of
  the path. The router decodes each segment after the split.
- `/users` and `/users/` are different paths.
- A literal comes before a parameter, and a parameter before `**`, also
  when the order of the list is different.
- The router gives `405` with `Allow`, a `204` with `Allow` to
  `OPTIONS`, and the `GET` route to `HEAD`. It gives `400` to a path
  with `.` or `..` segments. But a decoded value can contain `../`
  (from `%2F`), thus a parameter or the rest is not a safe file path.
- A group can have its own middleware. The template of the route
  (`Caravan_router.template`) is on the request and on the response,
  for an access log.

`examples/router.ml` has a full example.

## Middleware

The `caravan.middleware` library has middleware for the requests that
come through a reverse proxy, request IDs, an access log, CORS, an
error page, the HTTPS redirect, HSTS and a protection against CSRF:

```ocaml
open Caravan_middleware

let app handler =
  Caravan.Handler.compose
    [ Forwarded.middleware ~trusted:(Forwarded.trust_ips ["10.0.0.1"]) ()
    ; Request_id.middleware ()
    ; Access_log.middleware
        ~log:(fun e -> print_string (Access_log.line e ^ "\n"))
        ()
    ; Hsts.middleware ()
    ; Https_redirect.middleware ()
    ; Cors.middleware
        ~origins:(Cors.Only ["https://app.example"])
        ~allow_headers:["content-type"]
        ()
    ; Error_page.middleware
        ~report:(fun _req e _bt -> prerr_endline (Printexc.to_string e))
        ()
    ; Cross_origin.middleware
        ~trusted_origins:["https://app.example"]
        () ]
    handler
```

- `Forwarded` reads `X-Forwarded-For` and `X-Forwarded-Proto` of a
  trusted proxy, and `X-Forwarded-Host` if you turn it on. It walks the
  elements from the right, at most `limit` hops (1 by default), thus it
  does not read a value that a client adds before the elements of the
  trusted proxies. `Forwarded.client` gives the client, and the request
  gets the scheme and the authority that the proxy tells. The proxy
  must set or overwrite each field that is on, and a bad value gives the
  client `Unknown`, never the proxy.
- `Request_id` keeps a clean `X-Request-Id` of the request, or makes a
  new ID. The ID goes on the request and on the response.
- `Access_log` gives one entry for each request at the end of its
  response: the client, the request ID, the route, the status, the
  bytes, the duration, and if the body was complete, incomplete,
  failed, cancelled (the client left), not sent or upgraded.
  `Access_log.line` gives one line of text.
- `Cors` answers the preflights itself, and adds the CORS fields to the
  responses for the permitted origins (`Any`, `Only` a list, or
  `Matching` a predicate), with a correct `Vary`. It refuses `Any` with
  credentials. CORS is not access control: the handler runs for each
  origin, and only the read by the page is blocked. `Cors.permits` gives
  the same check to `Caravan_websocket.accept ~origin`.
- `Error_page` gives a page (by default a plain 500) for an exception of
  the handler, after a call of `report`. A cancel and the error of a
  request body go to the server as before.
- `Https_redirect` sends each `http` request to `https` with a 307 (308
  is better in production, when HTTPS works). A request with no valid
  authority, or with a path that does not start with `/`, gets 400.
- `Hsts` adds `Strict-Transport-Security` (365 days by default) to the
  responses of `https` requests, but not for `localhost` (also
  `*.localhost`), `127.0.0.1` and `[::1]`.
- `Cross_origin` refuses (403) a request with a method other than GET,
  HEAD and OPTIONS that a browser sends from another origin, as
  `CrossOriginProtection` of Go 1.25. It reads `Sec-Fetch-Site`, or else
  `Origin`, and needs no token. A `same-site` request is refused too:
  thus a page of `https://app.example.com` that sends a POST to
  `api.example.com` needs `https://app.example.com` in
  `trusted_origins`. Put the CORS origins with credentials there too.
  `exempt` lets a path, for example a webhook, take cross-origin POSTs.
- The order: `Forwarded` first (it gives the scheme of the client), then
  `Request_id` and `Access_log` (so that each redirect has a log entry),
  then `Hsts`, `Https_redirect`, `Cors`, `Error_page` and
  `Cross_origin` (a refusal gets its CORS fields), then the sessions
  and the router. A middleware that changes the path goes after
  `Https_redirect`, so that the redirect keeps the path of the client.

## Queries and forms

`Request.query_params` gives the pairs of the query, and
`Request.form` reads an `application/x-www-form-urlencoded` body:

```ocaml
open Caravan

let search req =
  match Form.find "q" (Request.query_params req) with
  | Some q ->
      Response.string ("results for " ^ q ^ "\n")
  | None ->
      Response.string ~status:Status.bad_request "no q\n"

let login req =
  match Request.form ~max_bytes:4096 req with
  | Ok f ->
      let user = Option.value ~default:"" (Form.find "user" f) in
      Response.string ("hello " ^ user ^ "\n")
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
```

- A pair list (`Form.t`) keeps the order and the duplicates.
  `Form.find` gives the first value, and `Form.find_all` gives all of
  them. Names are case-sensitive.
- The parse follows the WHATWG URL Standard: `+` is a space, and an
  incorrect `%` stays as it is. The parse never fails.
- `Request.form` reads the body, thus call it one time. It gives
  `Unsupported_media_type`, `Too_large` (1 MiB by default) or
  `Too_many_fields` (1000 by default). `Form.error_status` gives `415`
  or `413`. A body that declares a length larger than the limit is not
  read, thus the client gets no `100 Continue`.
- `Request.with_form` gives a request a form body, for the client or for
  a test.

## Cookies

`Request.cookie` reads a cookie of the request, and
`Response.add_cookie` sends a `Set-Cookie` field:

```ocaml
open Caravan

let theme = Cookie.make ~secure:true ~max_age:(30 * 24 * 3600) "theme" ""

let show req =
  match Request.cookie "theme" req with
  | Some t ->
      Response.string ("theme " ^ t ^ "\n")
  | None ->
      Response.string "no theme\n"

let set req =
  match Form.find "t" (Request.query_params req) with
  | Some (("light" | "dark") as t) ->
      Response.string "set\n"
      |> Response.add_cookie (Cookie.with_value t theme)
  | _ ->
      Response.string ~status:Status.bad_request "t: light or dark\n"

let clear _ =
  Response.string "cleared\n" |> Response.add_cookie (Cookie.expired theme)
```

- `Cookie.make` validates the name, the value and the attributes. It
  raises `Invalid_argument` for a value with a space or a `;`, for
  `SameSite=None` with no `Secure`, or for a `__Host-` name with a
  domain. Encode other bytes, for example with base64url.
- The defaults are `Path=/`, `HttpOnly` and `SameSite=Lax`. A cookie
  that a script reads needs `~http_only:false`. Use `~secure:true` in
  production.
- `Cookie.with_value` keeps the attributes, and `Cookie.expired` gives
  the cookie that removes it from the user agent.
- `Request.cookie` gives the first cookie with the name. The user agent
  gives the order, and a sibling subdomain can set a cookie with the
  same name: use a `__Host-` name for a security cookie.
- `Cookie.pp` shows the length of the value, not the value.

## Sessions

The optional `caravan.session` library keeps a session for each client,
with a signed cookie and a store:

```ocaml
let () = Mirage_crypto_rng_unix.use_default ()

(* A key of 32 random bytes, from the configuration of all the
   processes. *)
let key = Caravan_session.Key.of_base64url (Sys.getenv "SESSION_KEY")

let app =
  Caravan.Handler.compose
    [ Caravan_session.middleware ~keys:[key]
        ~store:(Caravan_session.memory_store ())
        () ]
    (fun req ->
      match Caravan.Request.path req with
      | "/login" ->
          (* A new ID after a login: no session fixation. *)
          Caravan_session.renew req ;
          Caravan_session.set "user" "ada" req ;
          Caravan.Response.string "welcome"
      | "/logout" ->
          Caravan_session.clear req ;
          Caravan.Response.string "bye"
      | _ ->
          Caravan.Response.string
            (Option.value ~default:"guest" (Caravan_session.find "user" req)) )
```

- The cookie is `__Host-session` by default: `Secure`, `HttpOnly`,
  `SameSite=Lax`, `Path=/`. A response that sets it gets
  `Cache-Control: no-store`.
- A session ends at most 30 min after the last request (between 15 and
  30 min: the limit moves only after half of it), and 8 h after its
  creation. `clear` ends it on the server: a request that runs at the
  same time cannot bring it back.
- `Caravan_session.store` connects a store of the application, for
  example a database. The store keys are hashes of the IDs.
- `Caravan_session.Signed` signs other values with the same keys. The
  first key signs, and the other keys verify: a rotation.
- Make no session for an anonymous user: a session starts at the first
  `set` (or `id`).

## CSRF

`Caravan_middleware.Cross_origin` stops the cross-origin requests of
the browsers with no token. `Caravan_session.Csrf` adds tokens bound to
the session, also for older browsers:

```ocaml
let () = Mirage_crypto_rng_unix.use_default ()

let key = Caravan_session.Key.of_base64url (Sys.getenv "SESSION_KEY")

let app =
  Caravan.Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Caravan_session.middleware ~keys:[key]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Caravan_session.Csrf.middleware () ]
    (fun req ->
      match Caravan.Request.meth req with
      | GET ->
          Caravan.Response.string
            ~headers:(Caravan.Headers.of_list [("Content-Type", "text/html")])
            ( "<form method=post>\
               <input type=hidden name=_csrf value=\""
            ^ Caravan_session.Csrf.token req
            ^ "\"><button>Send</button></form>" )
      | _ ->
          Caravan.Response.string "sent" )
```

- The order: `Cross_origin`, then the session, then `Csrf`.
- `Csrf.token` gives a new text for each call (a random mask against
  BREACH), and the response gets `Cache-Control: no-store`. A login
  (`renew`) and a logout (`clear`) end the tokens.
- The middleware reads the token from `X-CSRF-Token`, or from the field
  `_csrf` of an urlencoded form. The handler gets the same bytes of the
  form. Another body (JSON, multipart) needs the header.
- A request with no session has no token: by default it gets 403. A
  login form for anonymous users has no session: exempt it, and
  `Cross_origin` protects it.
- Use `Csrf` only for the routes of the cookie sessions. A JSON API
  with a bearer token needs no token against CSRF.

## Authentication

The optional `caravan.auth` library verifies JSON Web Tokens, for
example the bearer tokens of an identity provider:

```ocaml
(* The keys of the identity provider: its JWK Set, from the
   configuration. *)
let keys =
  match Caravan_auth.Jwt.Key.of_jwks (Sys.getenv "JWKS") with
  | Ok keys -> keys
  | Error e -> failwith e

(* The user of a request with "Authorization: Bearer <token>". The
   name of the scheme is case-insensitive (RFC 9110 §11.1). *)
let user req =
  match Caravan.Headers.find "authorization" (Caravan.Request.headers req) with
  | Some v
    when String.starts_with ~prefix:"bearer " (String.lowercase_ascii v) -> (
      let token = String.sub v 7 (String.length v - 7) in
      match
        Caravan_auth.Jwt.verify
          ~issuer:(One_of ["https://id.example"])
          ~audience:(One_of ["api://orders"])
          ~keys token
      with
      | Ok claims -> Caravan_auth.Jwt.subject claims
      | Error _ -> None )
  | _ -> None
```

- The algorithms are HS256, RS256, ES256 and EdDSA (Ed25519). A key
  has one algorithm, thus a token cannot choose another one. `none` is
  always refused.
- The keys come from a JWK Set, a PEM (a public key or a certificate),
  or a secret of 32 bytes or more. A key never comes from a token.
- `verify` checks the header and the signature first, then `exp`
  (required by default) and `nbf` with a leeway of 60 s, then `iss`
  and `aud`. The issuer and the audience are required arguments: `Any`
  skips a check.
- The JSON must be strict RFC 8259: no comments, no `NaN`, no
  duplicate member names.

## Users and policies

`Caravan_auth.middleware` finds the user of each request with a list of
schemes, and `Caravan_auth.require` protects the routes:

```ocaml
let () = Mirage_crypto_rng_unix.use_default ()

let session_key =
  Caravan_session.Key.of_base64url (Sys.getenv "SESSION_KEY")

let jwks =
  match Caravan_auth.Jwt.Key.of_jwks (Sys.getenv "JWKS") with
  | Ok keys -> keys
  | Error e -> failwith e

let bearer =
  Caravan_auth.Scheme.bearer
    ~issuer:(One_of ["https://id.example"])
    ~audience:(One_of ["api://orders"])
    ~keys:(fun () -> jwks)
    ()

let session = Caravan_auth.Scheme.session ~login:"/login" ()

let routes =
  Caravan_router.(
    handler
      [ get "/" (fun _ -> Caravan.Response.string "home")
      ; post "/login" (fun req ->
            (* Check the password of the form here. *)
            Caravan_auth.sign_in req
              (Caravan_auth.User.make ~roles:["admin"] "ada") ;
            Caravan.Response.string "welcome" )
      ; post "/logout" (fun req ->
            Caravan_auth.sign_out req ;
            Caravan.Response.string "bye" )
      ; group
          ~middleware:
            [Caravan_auth.require ~policy:(Caravan_auth.role "admin") ()]
          "/admin"
          [get "" (fun _ -> Caravan.Response.string "admin")] ] )

let app =
  Caravan.Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Caravan_session.middleware ~keys:[session_key]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Caravan_auth.middleware ~schemes:[session; bearer] ()
    ; Caravan_session.Csrf.middleware
        ~exempt:(fun req ->
          Caravan.Request.path_only req = "/login"
          ||
          match Caravan_auth.user req with
          | Some u -> Caravan_auth.User.scheme u = "Bearer"
          | None -> false )
        () ]
    routes
```

- A scheme gives `Authenticated`, `No_credentials` (the next scheme)
  or `Failed` (no user, and no other scheme). The schemes run at the
  first call of `user`, `failure`, `challenge` or `require`: a public
  `GET` costs no check. The user is that of the session at the start of
  the request: a `sign_in` in the handler shows in the next request.
  The middleware never refuses a request.
- `require` gives the challenge of the scheme to a request with no
  user, and 403 to a user that the policy refuses (`authenticated`,
  `role`, `scope`, `all`, `any`). The first scheme gives the
  challenge: here the session, with a 303 to the login page for the
  navigation of a browser and a 401 for the other requests. On the
  routes of an API, use `require ~challenge:"Bearer"`. A response of
  the protected handler with no `Cache-Control` gets
  `Cache-Control: private`.
- `sign_in` gives the session a new ID. A second `sign_in` ends the
  old session and its data. `User.scheme` is the scheme that
  authenticated the user for this request: the bearer users are exempt
  from the CSRF tokens, because a browser never sends a bearer token by
  itself.
- The login form is exempt from the CSRF tokens: an anonymous user has
  no session yet. `Cross_origin` protects it, but a browser older than
  2020 can send it from another site (a login CSRF).
- After a login, redirect only to `Caravan_auth.return_path`.

## Compression

The optional `caravan.compress` library compresses the responses with
gzip or deflate, with the pure OCaml library decompress:

```ocaml
let app handler =
  Caravan.Handler.compose
    [ Caravan_compress.middleware ~level:6 ()
    ; Caravan_static.middleware ~root:"/srv/www" () ]
    handler
```

- The coding comes from `Accept-Encoding` (gzip on a tie). A response
  that depends on it gets `Vary: Accept-Encoding`.
- Only text types (`text/*` except `text/event-stream`, JSON,
  JavaScript, XML, SVG) with a known length of 2 KiB to 8 MiB are
  compressed. A stream is not: decompress cannot flush a part of a
  stream. `~streams:true` compresses streams too, but the client then
  gets the bytes in large blocks.
- A compressed response loses `Content-Length` and `Accept-Ranges`, and
  its ETag becomes weak (`W/"t"`, or `W/"t-deflate"` for deflate). The
  304 of `caravan.static` gets the same ETag. `If-Match` never matches a weak ETag: give the routes that use
  it the attribute, with
  `Caravan.Response.with_attr Caravan_compress.skip ()`.
- Each compressed response costs about 770 KiB of memory and 0.1 to
  0.2 ms while it goes out. At most 64 run at the same time
  (`max_active`): the other responses go out with no compression.
- BREACH: do not compress a page with a secret and text of the client.
- For pure OCaml, add `checkseum.ocaml` to the libraries of your
  executable: the default checksums of decompress have C code.

## File uploads

`Request.multipart` streams the parts of a `multipart/form-data` body,
and `Request.multipart_form` keeps them in memory:

```ocaml
open Caravan

(* Counts the bytes of each file, with no copy in memory. *)
let upload req =
  let buf = Bytes.create 65536 in
  let rec size body n =
    match Body.read body buf 0 (Bytes.length buf) with
    | 0 ->
        n
    | k ->
        size body (n + k)
  in
  let count acc part =
    match Multipart.filename part with
    | Some _ ->
        (Multipart.name part, size (Multipart.body part) 0) :: acc
    | None ->
        acc
  in
  match Request.multipart req ~init:[] count with
  | Ok files ->
      Response.string
        (String.concat ""
           (List.map (fun (n, s) -> Printf.sprintf "%s: %d bytes\n" n s) files) )
  | Error e ->
      Response.string ~status:(Multipart.error_status e)
        (Format.asprintf "%a\n" Multipart.pp_error e)
```

- The body of a part is valid only in the callback: copy it to keep it.
  If the request body ends before the part ends, its read raises
  `Body.Truncated`, thus a copy does not end as if it were complete.
- The limits are `max_parts` (1000 by default) and `max_header_bytes`
  (16 KiB by default, for the head of each part). `multipart_form` also
  has `max_bytes` (8 MiB by default).
- A file name comes from the client: it is not a safe path.
- `Request.with_multipart` gives a request a multipart body, for the
  client or for a test.

## Static files

The `caravan.static` library serves the files of a directory. It reads
them with `Weft.File`, on the thread pool of weft, thus a read does not
stop the domain:

```ocaml
open Caravan

(* /assets/app.css is the file public/app.css. Other paths go to the
   handler. *)
let app handler =
  Handler.compose
    [ Caravan_static.middleware ~prefix:"/assets" ~root:"public"
        ~cache_control:(fun rel ->
          if rel = "index.html" then Some "no-cache" else Some "max-age=3600" )
        () ]
    handler
```

- A response has `ETag`, `Last-Modified` and `X-Content-Type-Options:
  nosniff`. `If-None-Match` and `If-Modified-Since` give `304`, and
  `If-Match` and `If-Unmodified-Since` give `412`.
- A `GET` with one byte range gets `206`, and a range out of the file
  gets `416`. `If-Range` is supported. More than one satisfiable range
  gives the full file.
- The middleware refuses `..`, `%2F`, a backslash, the reserved names
  of Windows, and the names that start with a dot (`~dotfiles:true`
  permits them). The rules are the same on each OS.
- A directory gets its `index.html`. A path of a directory with an
  `index.html` and with no `/` at the end gets a `301` to the path with
  a `/`.
- Each `GET` under the prefix, with a permitted path, costs one open
  of a file on the pool, also when the handler gets the request. Thus give a prefix, or use
  `Caravan_static.dir` on a route.
- The library follows symbolic links: do not let a client make files in
  the root.
- With `caravan.router`, `Caravan_static.dir ~root (Caravan_router.rest req)
  req` serves a route `/static/**`. `Caravan_static.file` serves one
  file.

## TLS

The optional `caravan.tls` library serves HTTPS with
[ocaml-tls](https://github.com/mirleft/ocaml-tls), a TLS library in
pure OCaml. Its engine does the handshake of each connection, then
gives the TLS flow to the HTTP/1.1 engine:

```ocaml
let read_file path = In_channel.with_open_bin path In_channel.input_all

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let () =
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let certs = ok (X509.Certificate.decode_pem_multiple (read_file "cert.pem")) in
  let key = ok (X509.Private_key.decode_pem (read_file "key.pem")) in
  let tls = ok (Tls.Config.server ~certificates:(`Single (certs, key)) ()) in
  Caravan.Server.run
    ~engine:(Caravan_tls.engine tls Caravan.Server.http1)
    (module Weft_os)
    (fun req -> Caravan.Response.string (Caravan.Request.scheme req))
```

- A request has the scheme `https`, and `Conn_info.alpn` tells the ALPN
  protocol. The ALPN list of the configuration can have only
  `http/1.1`, and its lowest version must be TLS 1.2 or later.
- The handshake has the deadline `header_timeout`, and a stop of the
  server ends it. A handshake that fails is not an error of the server:
  the server reports nothing.
- `Caravan_tls.server` and `Caravan_tls.client` do a handshake on any
  `Caravan.Flow.t`, for example for a test or a proxy. The client checks
  the `peer_name` of its `Tls.Config`: give one for each server on the
  Internet.
- A handshake can read at most 64 KiB. A session permits 64 small
  records with no plaintext (for example a KeyUpdate), and one more for
  each 16 KiB of plaintext.
- A client that ends the TCP connection with no `close_notify` gives
  `Flow.Closed` to a read: it can be a truncation.

## WebSocket

The `caravan.websocket` library adds WebSocket
([RFC 6455](https://www.rfc-editor.org/rfc/rfc6455)) to the server. A
handler gives the response of `Caravan_websocket.accept`:

```ocaml
let handler req =
  Caravan_websocket.accept req (fun ws ->
      let rec echo () =
        Caravan_websocket.(send ws (recv ws));
        echo ()
      in
      echo ())
```

- `accept` examines the request and sends the `101` response. By
  default, it permits browser pages only from the origin of the server.
- `recv` gives full messages. The library joins fragments, sends a Pong
  for each Ping, and examines the UTF-8 of text. It examines each part
  of a text frame as it arrives, thus incorrect text closes the
  connection before the end of the frame or of the message.
- Many fibers can send at the same time, and `close` closes the
  connection from any fiber, with a time limit.
- When the server stops, each connection sends a Close frame with the
  code 1001 and waits for the reply of the peer, with the same time
  limit.
- A write timeout (30 s by default) stops a send to a peer that reads
  nothing. A read timeout (30 s by default) closes a connection when a
  frame stops in the middle. An optional idle timeout closes a connection
  when no frame comes in time.
- All the cases of the [Autobahn](https://github.com/crossbario/autobahn-testsuite)
  test suite give OK, or INFORMATIONAL for three cases, except the
  compression cases: caravan does not accept `permessage-deflate`.

## Client

The `caravan.client` library is an HTTP/1.1 client. It uses the parser
and the writer of the server:

```ocaml
Weft.run (module Weft_os) @@ fun () ->
Caravan_client.run @@ fun client ->
Caravan_client.with_response client
  (Caravan.Request.make ~authority:"example.com" Caravan.Method.GET "/")
  (fun resp -> Caravan.Body.to_string (Caravan_client.body resp))
```

- The client keeps idle connections for each host and port, and gives a
  connection back when the response body ends.
- The request body goes while the caller reads the response. The writer
  of the body belongs to the client, not to the caller. Thus a server
  handler can forward a request and stream the two bodies at the same
  time.
- `Dns.resolve` of weft does the DNS lookup on the thread pool of the
  world, thus a lookup does not stop the fibers of its domain.
- Time limits for the connect, the response head, each read and each
  write. One retry of an idempotent request without a body when the
  server closed an idle connection.
- Version 1 has no TLS, no proxy support, no redirects, no cookies and
  no content codings.

## Testing

The `caravan.testing` library runs a handler through the real server
and client, in the simulator of weft. The time is virtual, thus a test
of a 30 s timeout takes no real time, and a run is the same for a seed:

```ocaml
let hi _req = Caravan.Response.string "hi"

let () =
  Caravan_testing.run hi (fun t ->
      let reply = Caravan_testing.get t "/" in
      assert (reply.body = "hi") )

(* One run for each seed, on 4 virtual domains. A failure tells its seed:
   replay it with ~replay. *)
let () =
  Caravan_testing.race ~seeds:20 hi (fun t ->
      assert ((Caravan_testing.get t "/").body = "hi") )

(* Manual time: only advance moves the time. The request goes in the
   background while the test advances the time. *)
let () =
  let slow _req =
    Weft.Time.sleep Mono (Weft.Span.s 20) ;
    Caravan.Response.string "late"
  in
  Caravan_testing.run ~manual:true slow (fun t ->
      let p =
        Caravan_testing.background t (fun () -> Caravan_testing.get t "/")
      in
      Caravan_testing.advance t (Weft.Span.s 20) ;
      assert ((Weft.Promise.await p).body = "late") )
```

- An error of the handler fails the run, unless the test takes it with
  `take_errors`.
- In auto mode, a guard (10 min of virtual time by default) stops a test
  that waits for nothing. In manual mode, such a test raises
  `Weft_sim.Deadlock`.
- `background` runs a request (or other code) while the test continues.
  At the end of the test, the run cancels it, as a client that leaves. A
  failure of a background fiber must be awaited.
- `run ~stop_on_signals:true` stops the server at SIGINT and SIGTERM, as
  `Server.run` does, and `signal` sends a signal. Thus a test can check a
  graceful stop.
- `race` runs the test for each seed and stops at the first failure.
  A replay is exact if the handler and the test keep no state from one
  run to the next.
- `Caravan_testing.sim` gives the simulator, for latency, network faults
  and names (`Weft_sim.set_dns`).

## Requirements

- OCaml >= 5.5.0
- [weft](https://github.com/bikallem/weft) (not on opam)
- [http-date](https://ocaml.org/p/http-date/latest) >= 0.2
- dune >= 3.20
- For tests: windtrap and ppx_windtrap >= 0.2.0, from the fork
  [bikallem/windtrap](https://github.com/bikallem/windtrap).

The Nix flake gives all of these, at the commits that flake.nix and
flake.lock pin.

## Development

The repository includes a Nix flake. Its dev shell provides OCaml 5.5,
dune, weft, http-date, windtrap, ppx_windtrap, ocaml-lsp, utop,
ocamlformat and odoc. `nix build` builds the caravan package (the six
libraries, with no tests).

With [direnv](https://direnv.net) and
[nix-direnv](https://github.com/nix-community/nix-direnv) installed,
run this once and the environment then loads whenever you enter the
directory:

```sh
direnv allow
```

Without direnv, enter the shell manually:

```sh
nix develop
```

Then:

```sh
make build      # dune build
make test       # all test suites (dune test --force)
make fmt        # format with ocamlformat (dune fmt)
make mutate     # mutation testing of the suites of test/, in parallel
make mutate MUTATE=websocket/   # only the mutants of the files under a prefix
make coverage   # test coverage report of all suites
make autobahn   # the Autobahn WebSocket test suite, with podman or docker
```

The suites of test/ run in the simulator of weft, thus they use no real
time and no real network. The suite of test/linux runs on Linux in the
OS world of weft, with real sockets, signals and child processes.

The examples use `Weft_os`, but we test them only on Linux. Try the
example server, and stop it with Ctrl-C:

```sh
dune exec examples/hello.exe
curl -i localhost:8080/hello
```

For WebSocket, run `dune exec examples/ws_echo.exe` and open
<http://localhost:8080/> in a browser. While the example server runs,
`dune exec examples/fetch.exe -- http://localhost:8080/hello` fetches a
page with the client.

## License

[MPL-2.0](LICENSE)
