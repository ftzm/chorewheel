{
  description = "chorewheel";

  # Fix:
  # It looks like in nix 2.6.1 the follows directive only looks in the root
  # flake (regression?), which means that the nixpkgs.follows in the haskellNix
  # flake can't resolve because the nixpkgs.follows /there/ can't find a
  # nixpkgs-unstable /here/. We can work around this by making a
  # nixpkgs-unstable input here that follows the one in the haskellNix flake,
  # which means that everything ends up using the intended version of nixpkgs.
  inputs.nixpkgs-unstable.follows = "haskellNix/nixpkgs-unstable";

  # Original
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs =
            self.haskell-nix.project' rec {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell = {
                tools = {
                  hpack = "latest";
                  cabal = "latest";
                  ghcid = "latest";
                  haskell-language-server = "latest";
                  hlint = "latest";
                  # See https://github.com/input-output-hk/haskell.nix/issues/1337
                  ormolu = {
                    version = "latest";
                    modules = [ ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "Cabal"; }; }) ];
                  };
                };
                ## ormolu that uses ImportQualifiedPost.
                ## To use, remove ormolu from the shell.tools section above, and uncomment the following lines.
                # buildInputs =
                #   let
                #     ormolu = pkgs.haskell-nix.tool compiler-nix-name "ormolu" "latest";
                #     ormolu-wrapped = pkgs.writeShellScriptBin "ormolu" ''
                #       ${ormolu}/bin/ormolu --ghc-opt=-XImportQualifiedPost $@
                #     '';
                #   in
                #   [ ormolu-wrapped ];
              };
            };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            overlay
          ];
        };
        flake = pkgs.hsPkgs.flake { };
      in
      flake // { defaultPackage = flake.packages."chorewheel:exe:chorewheel"; }
    );
}
