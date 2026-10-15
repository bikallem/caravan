{
  description = "caravan: OCaml 5.5 development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    weft = {
      url = "github:bikallem/weft";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      weft,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            # The fork bikallem/windtrap at a commit of its main branch. The
            # fork has changes after the release 0.2.0 of invariant-hq, and it
            # has no release.
            windtrapFork = {
              version = "0.2.0-unstable-2026-10-15";
              src = pkgs.fetchFromGitHub {
                owner = "bikallem";
                repo = "windtrap";
                rev = "8d5d2a47270bc5fef6ac7633bf30d5612464c5b4";
                hash = "sha256-WQHAi9WlLyS4xdFWiwbmIzDL1IgVdKmqfwOWhSWd4MQ=";
              };
              meta = {
                homepage = "https://github.com/bikallem/windtrap";
                license = pkgs.lib.licenses.isc;
              };
            };
            # http-date's pinned commit declares (lang dune 3.24), so it is
            # built with this dune and the dev shell uses it too. Other
            # packages keep nixpkgs' dune.
            dune_3_24 = pkgs.ocaml-ng.ocamlPackages_5_5.dune_3.overrideAttrs (_: rec {
              version = "3.24.2";
              src = pkgs.fetchurl {
                url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
                hash = "sha256-RyeYaRsCFtr1OHCfD0cDs2F+8krQhmyQlgaLqrpNdio=";
              };
            });
            ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope (
              final: prev: {
                # Not on opam yet: the package of the weft flake.
                weft = final.callPackage weft.lib.package { };

                # Not packaged in nixpkgs yet: https://ocaml.org/p/affect/latest
                affect = final.callPackage (
                  {
                    lib,
                    fetchurl,
                    topkg,
                    buildTopkgPackage,
                    cmdliner,
                    withCmdliner ? true,
                  }:
                  buildTopkgPackage rec {
                    pname = "affect";
                    version = "0.0.0";

                    minimalOCamlVersion = "5.5.0";

                    src = fetchurl {
                      url = "https://erratique.ch/software/affect/releases/affect-${version}.tbz";
                      hash = "sha512-syj2aW5gxInajMLo+tBTfKZjSjn8DF3lhA1PmFYfEQYX73m6goXuhCfdmZCMaz05EUlzWYzdxd7ZGrzOO5G5pg==";
                    };

                    propagatedBuildInputs = lib.optional withCmdliner cmdliner;

                    buildPhase = ''
                      runHook preBuild
                      ${topkg.run} build --with-cmdliner ${lib.boolToString withCmdliner}
                      runHook postBuild
                    '';

                    meta = {
                      description = "Streamlined and natural concurrency model for OCaml";
                      homepage = "https://erratique.ch/software/affect";
                      license = lib.licenses.isc;
                    };
                  }
                ) { };

                # nixpkgs ships windtrap 0.1.0 and no ppx_windtrap. The two
                # packages are built from the same source of the fork.
                windtrap = final.buildDunePackage {
                  pname = "windtrap";
                  inherit (windtrapFork) version src;
                  minimalOCamlVersion = "5.0";
                  meta = windtrapFork.meta // {
                    description = "One library for all your OCaml tests";
                  };
                };

                ppx_windtrap = final.buildDunePackage {
                  pname = "ppx_windtrap";
                  inherit (windtrapFork) version src;
                  minimalOCamlVersion = "5.0";
                  propagatedBuildInputs = [
                    final.windtrap
                    final.ppxlib
                  ];
                  meta = windtrapFork.meta // {
                    description = "Inline tests, coverage and mutation testing for windtrap";
                  };
                };

                # Not packaged in nixpkgs yet: https://ocaml.org/p/http-date/latest
                # Pinned to an untagged commit (after v0.2) until v0.3 is
                # released. It declares (lang dune 3.24) and nixpkgs has dune
                # 3.23.1, so only this package builds with a newer dune.
                http-date = (final.buildDunePackage.override { dune_3 = dune_3_24; }) {
                  pname = "http-date";
                  version = "0.2-unstable-2026-09-30";
                  minimalOCamlVersion = "4.11";
                  src = pkgs.fetchFromGitHub {
                    owner = "bikallem";
                    repo = "http-date";
                    rev = "8924a84d249a2773f12d79c442bf133eff5d549f";
                    hash = "sha256-6TsG2TvD7z+HRpLRwXq+m+p9ouAHOIvZpsIRMkD52qA=";
                  };
                  meta = {
                    description = "RFC 9110 HTTP date decoder and encoder";
                    homepage = "https://github.com/bikallem/http-date";
                    license = pkgs.lib.licenses.mpl20;
                  };
                };
              }
            );
          in
          f { inherit pkgs ocamlPackages dune_3_24; }
        );
    in
    {
      packages = forAllSystems (
        { ocamlPackages, ... }:
        {
          inherit (ocamlPackages) affect;
          default = ocamlPackages.affect;
        }
      );

      devShells = forAllSystems (
        {
          pkgs,
          ocamlPackages,
          dune_3_24,
        }:
        {
          default = pkgs.mkShell {
            packages = with ocamlPackages; [
              # Toolchain (http-date at its pinned commit needs dune 3.24)
              ocaml
              dune_3_24
              findlib

              # Dependencies (weft here is the flake input, so use ocamlPackages.weft)
              affect
              ocamlPackages.weft
              http-date

              # Test dependencies
              windtrap
              ppx_windtrap

              # Dev tools
              ocaml-lsp
              utop
              ocamlformat
              odoc
            ];
          };
        }
      );

      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);
    };
}
