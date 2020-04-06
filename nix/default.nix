{ compiler }:

let
  _sources = import ./sources.nix;
  haskellNix = import _sources."haskell.nix" {};

  _pkgs = with haskellNix; import sources.nixpkgs-1909 nixpkgsArgs;
  pkgs = _pkgs.pkgsCross.musl64;

  pkgSet = with pkgs.haskell-nix; mkCabalProjectPkgSet {
    plan-pkgs = (importAndFilterProject (callCabalProjectToNix {
      src = cleanSourceHaskell { src = ./..; name = "boxpub-source"; };
      index-state = "2020-01-21T00:00:00Z";
      index-sha256 = "0vyf6sixww31ckh2mp7b7ilcipfzgg05wsizismzjdw3sy03n9in";
      plan-sha256 = "1vs7jcgg72snyl3ckvpdgikj2xbvylkbnbk4rlcda23ihdjwq35p";
      ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${compiler};
    })).pkgs;
    pkg-def-extras = [ ];
    modules = [
      {
        packages.transformers-compat.flags.five-three = true; # transformers >= 0.5.3
        packages.time-locale-compat.flags.old-locale = false; # time >= 1.5
      }

      {
        packages.boxpub.configureFlags = with pkgs; [
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
in {
  inherit (pkgSet.config) hsPkgs;
  nixpkgs = pkgs;
}
