let
  pkgs = import (import ./nix/sources.nix).nixpkgs {};
  compilerVersion = "ghc8107";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
 compiler.developPackage {
   root = ./.;
   overrides = with pkgs.haskell.lib; self: super: {
     postgresql-syntax = dontCheck (markUnbroken (super.postgresql-syntax));
     headed-megaparsec = dontCheck (markUnbroken (super.headed-megaparsec));
   };
   source-overrides = {
      headed-megaparsec = "0.2.0.1";
   };
   modifier = drv:
     pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
       [ cabal-install
         ghcid
         pkgs.docker-compose
         pkgs.postgresql
         hlint
       ]);
 }
