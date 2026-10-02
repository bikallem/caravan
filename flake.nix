{
  description = "affect-http: OCaml 5.5 development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
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
            windtrapRelease = rec {
              version = "0.2.0";
              src = pkgs.fetchurl {
                url = "https://github.com/invariant-hq/windtrap/releases/download/v${version}/windtrap-${version}.tbz";
                hash = "sha256-LmGob4yXUCwfilk8KOJV1ZGkSxUzxu7YzQyEevo9dyo=";
              };
              meta = {
                homepage = "https://github.com/invariant-hq/windtrap";
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

                # nixpkgs ships windtrap 0.1.0 and no ppx_windtrap. Both 0.2.0
                # packages are built from the same release tarball.
                windtrap = final.buildDunePackage {
                  pname = "windtrap";
                  inherit (windtrapRelease) version src;
                  minimalOCamlVersion = "5.0";
                  meta = windtrapRelease.meta // {
                    description = "One library for all your OCaml tests";
                  };
                };

                ppx_windtrap = final.buildDunePackage {
                  pname = "ppx_windtrap";
                  inherit (windtrapRelease) version src;
                  minimalOCamlVersion = "5.0";
                  propagatedBuildInputs = [
                    final.windtrap
                    final.ppxlib
                  ];
                  meta = windtrapRelease.meta // {
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

              # Dependencies
              affect
              http-date

              # Test dependencies
              windtrap
              ppx_windtrap

              # Dev tools
              ocaml-lsp
              utop
              ocp-indent
              odoc
            ];
          };
        }
      );

      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);
    };
}
