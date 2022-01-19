let
  pkgs = import (import ./nix/sources.nix).nixpkgs {};
  compilerVersion = "ghc8107";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
  compiler.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
          pkgs.docker-compose
          pkgs.postgresql
        ]);
  }
