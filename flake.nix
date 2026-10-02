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
              }
            );
          in
          f { inherit pkgs ocamlPackages; }
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
        { pkgs, ocamlPackages }:
        {
          default = pkgs.mkShell {
            packages = with ocamlPackages; [
              # Toolchain
              ocaml
              dune_3
              findlib

              # Dependencies
              affect

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
