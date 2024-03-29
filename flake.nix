{
  description = "Chorewheel";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix, unstable }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        ghcVersion = "924";
        devDrv = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler."ghc${ghcVersion}"
            cabal-install
            hpack
            hlint
            hpack
            ghcid
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [ ghcVersion ];
            })
            haskellPackages.fourmolu
            zlib
            postgresql
            docker-compose
            inotify-tools
            lsof
            expect
            entr
            haskellPackages.hoogle
          ];
        };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            chorewheel = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc${ghcVersion}";

              # We don't use this (standard?) source for tools because we'd
              # rather get cached versions from nixpkgs than build with our
              # pinned dependencies.
              #
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = # let
                #   modules = [
                #     ({ lib, ... }: {
                #       # https://github.com/input-output-hk/haskell.nix/issues/829
                #       config.dontStrip = false;
                #       # https://github.com/input-output-hk/haskell.nix/issues/1177
                #       config.reinstallableLibGhc = true;
                #       # options.nonReinstallablePkgs =
                #       #   lib.mkOption { apply = x: [ "exceptions" "stm" ] ++ x; };
                #     })
                #   ];
                # in {
                {
                  #   cabal = { };
                  #   hlint = {};
                  #   haskell-language-server = {
                  #     inherit modules;
                  #   };
                  #     fourmolu = {};
                };

              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                cabal-install
                hlint
                (pkgs.haskell-language-server.override {
                  supportedGhcVersions = [ ghcVersion ];
                })
                haskell.packages.ghc924.fourmolu_0_8_2_0
                postgresql
              ];

            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        unstablePkgs = import nixpkgs { inherit system; };
        flake = pkgs.chorewheel.flake { };
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."chorewheel:exe:chorewheel";
        # We use a separate derivation for the shell so that you don't need to
        # build all of our packages just to use cached binaries.
        # devShell = devDrv;
      });
}
