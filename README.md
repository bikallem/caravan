# caravan

An HTTP server for OCaml 5, built on
[affect](https://erratique.ch/software/affect), a concurrency library
with structured concurrency and cancellation. The HTTP/1.1 server and
WebSocket are available at this time. The design leaves room for HTTP/2,
HTTP/3, a client and a Traefik-like reverse proxy.

```ocaml
let () = Caravan.Server.run (fun _req -> Caravan.Response.string "Hello!\n")
```

> **Status:** the HTTP/1.1 server and WebSocket are complete but young,
> and their API can change. There is no client or proxy yet.

## What the server does

- A strict RFC 9112 parser, written from scratch: CRLF only, no
  obs-fold, Content-Length and Transfer-Encoding rules that resist
  request smuggling, and limits on the request line, the header section
  and bodies (414, 431, 413).
- Streaming request and response bodies, with backpressure: chunked
  encoding with trailers, `Expect: 100-continue`, keep-alive and
  pipelining.
- Header, idle, read and write timeouts; a lingering close so that
  responses are not lost to connection resets; a cap on connections.
- Graceful shutdown on SIGINT or SIGTERM: stop accepting, let requests
  in progress finish within a grace period, then cancel them. A second
  signal aborts. A handler can see the stop with `Conn_info.stopping`.
- `Response.upgrade` hands the connection over after a `101 Switching
  Protocols` or a `2xx` to `CONNECT`, the basis for WebSocket and
  tunnels.
- A handler whose client disconnects is cancelled, along with what it
  started, such as an upstream request.

Handlers are plain functions, `Request.t -> Response.t`, and each
request runs in its own asynchronous call, in parallel across domains.

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
- A system thread does the DNS lookup, thus a lookup does not stop the
  fibers of its domain.
- Time limits for the connect, the response head, each read and each
  write. One retry of an idempotent request without a body when the
  server closed an idle connection.
- Version 1 has no TLS, no proxy support, no redirects, no cookies and
  no content codings.

## Requirements

- OCaml >= 5.5.0
- [affect](https://ocaml.org/p/affect/latest) >= 0.0.0
- [http-date](https://ocaml.org/p/http-date/latest) >= 0.2
- dune >= 3.20
- For tests: [windtrap](https://github.com/invariant-hq/windtrap) and
  ppx_windtrap >= 0.2.0

## Development

The repository includes a Nix flake that provides OCaml 5.5, dune,
affect, http-date, windtrap, ocaml-lsp, utop, ocamlformat and odoc.

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
make test       # all test suites
make fmt        # format with ocamlformat (dune fmt)
make mutate     # mutation testing across all suites, in parallel
make mutate MUTATE=websocket/   # only the mutants of the files under a prefix
make coverage   # test coverage report
make autobahn   # the Autobahn WebSocket test suite, with podman or docker
```

Try the example server, and stop it with Ctrl-C:

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
