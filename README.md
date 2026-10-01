# affect-http

An HTTP/1.1 server and client library for OCaml, built on
[affect](https://erratique.ch/software/affect), a concurrency model
for OCaml 5 with structured concurrency and cancellation.

> **Status:** early development. The project is a skeleton and has no
> usable API yet.

## Requirements

- OCaml >= 5.5.0
- [affect](https://ocaml.org/p/affect/latest) >= 0.0.0
- dune >= 3.20

## Development

The repository includes a Nix flake that provides OCaml 5.5, dune,
affect, ocaml-lsp, utop, ocamlformat and odoc.

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

Then build, test and format with dune:

```sh
dune build
dune test
dune fmt
```

The flake also builds affect on its own, since it is not yet in
nixpkgs:

```sh
nix build .#affect
```

## License

[MPL-2.0](LICENSE)
