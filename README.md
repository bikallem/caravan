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
`caravan.router` and `caravan.middleware` depend on no world of weft.
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
come through a reverse proxy, request IDs, an access log, CORS and an
error page:

```ocaml
open Caravan_middleware

let app handler =
  Caravan.Handler.compose
    [ Forwarded.middleware ~trusted:(Forwarded.trust_ips ["10.0.0.1"]) ()
    ; Request_id.middleware ()
    ; Access_log.middleware
        ~log:(fun e -> print_string (Access_log.line e ^ "\n"))
        ()
    ; Cors.middleware
        ~origins:(Cors.Only ["https://app.example"])
        ~allow_headers:["content-type"]
        ()
    ; Error_page.middleware
        ~report:(fun _req e _bt -> prerr_endline (Printexc.to_string e))
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
let () =
  Caravan_testing.run
    (fun _req -> Caravan.Response.string "hi")
    (fun t ->
      let reply = Caravan_testing.get t "/" in
      assert (reply.body = "hi") )
```

- An error of the handler fails the run, unless the test takes it with
  `take_errors`.
- A guard (10 min of virtual time by default) stops a test that waits
  for nothing.
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
