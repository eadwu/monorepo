{ pkgs ? import ./nixpkgs.nix { } }:

let
  haskellNix = import ./haskell.nix { };
  nixpkgs = (import pkgs.nixpkgs (import haskellNix));
  haskell = nixpkgs.pkgsCross.musl64.haskell-nix;

  pkgSet = with haskell; mkCabalProjectPkgSet {
    plan-pkgs = (importAndFilterProject (callCabalProjectToNix {
      src = cleanSourceHaskell { src = ./..; name = "boxpub-src"; };
      index-state = "2020-01-21T00:00:00Z";
      index-sha256 = "0vyf6sixww31ckh2mp7b7ilcipfzgg05wsizismzjdw3sy03n9in";
      plan-sha256 = "1vs7jcgg72snyl3ckvpdgikj2xbvylkbnbk4rlcda23ihdjwq35p";
    })).pkgs;
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
