# caravan

An HTTP server for OCaml 5, built on
[affect](https://erratique.ch/software/affect), a concurrency library
with structured concurrency and cancellation. The HTTP/1.1 server works
today; the design leaves room for HTTP/2, HTTP/3, WebSocket, a client
and a Traefik-like reverse proxy.

```ocaml
let () = Caravan.Server.run (fun _req -> Caravan.Response.string "Hello!\n")
```

> **Status:** the HTTP/1.1 server is complete but young, and its API may
> still change. There is no client, WebSocket or proxy yet.

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
  signal aborts.
- `Response.upgrade` hands the connection over after a `101 Switching
  Protocols` or a `2xx` to `CONNECT`, the basis for WebSocket and
  tunnels.
- A handler whose client disconnects is cancelled, along with what it
  started, such as an upstream request.

Handlers are plain functions, `Request.t -> Response.t`, and each
request runs in its own asynchronous call, in parallel across domains.

## Requirements

- OCaml >= 5.5.0
- [affect](https://ocaml.org/p/affect/latest) >= 0.0.0
- [http-date](https://ocaml.org/p/http-date/latest) >= 0.2
- dune >= 3.20
- For tests: [windtrap](https://github.com/invariant-hq/windtrap) and
  ppx_windtrap >= 0.2.0

## Development

The repository includes a Nix flake that provides OCaml 5.5, dune,
affect, http-date, windtrap, ocaml-lsp, utop, ocp-indent and odoc.

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
make fmt        # indent with ocp-indent
make mutate     # mutation testing across all suites (slow)
make coverage   # test coverage report
```

Try the example server, and stop it with Ctrl-C:

```sh
dune exec examples/hello.exe
curl -i localhost:8080/hello
```

## License

[MPL-2.0](LICENSE)
