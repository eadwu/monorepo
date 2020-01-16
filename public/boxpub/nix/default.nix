{ pkgs ? import ./nixpkgs.nix { } }:

let
  haskellNix = import ./haskell.nix { };
  nixpkgs = (import pkgs.nixpkgs (import haskellNix));
  haskell = nixpkgs.pkgsCross.musl64.haskell-nix;

  pkgSet = with haskell; mkCabalProjectPkgSet {
    plan-pkgs = (importAndFilterProject (callCabalProjectToNix {
      src = cleanSourceHaskell { src = ./..; name = "boxpub-src"; };
      index-state = "2020-01-11T00:00:00Z";
      index-sha256 = "0vabvmgpz9p7i7fipkfdap7ird9mw285k10wd3mzkrlss7x2dlcn";
      plan-sha256 = "1byrxx61lwv39rh2jnrn5snbc30cw5xz7li5ac46ww5h6z5yd62q";
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
