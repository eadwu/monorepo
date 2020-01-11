{ pkgs ? import ./nixpkgs.nix { } }:

let
  haskellNix = import ./haskell.nix { };
  nixpkgs = (import pkgs.nixpkgs (import haskellNix));
  haskell = nixpkgs.pkgsCross.musl64.haskell-nix;

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [ ];
    modules = [
      {
        packages.transformers-compat.flags.five-three = true; # transformers >= 0.5.3
        packages.time-locale-compat.flags.old-locale = false; # time >= 1.5
      }

      {
        packages.boxpub.configureFlags = with nixpkgs; [
          "--disable-shared"
          "--disable-executable-dynamic"
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--extra-lib-dirs=${zlib.static}/lib"
          "--extra-lib-dirs=${glibc.static}/lib"
          "--extra-lib-dirs=${gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ];
      }
    ];
  };
in
  pkgSet.config.hsPkgs // { inherit nixpkgs; iohaskell = haskell; }
