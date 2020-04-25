{ compiler, ... }@args:

let
  _sources = import ./sources.nix;
  haskellNix = import _sources."haskell.nix" {};

  _args = builtins.removeAttrs args [ "nixpkgs" "compiler" ];
  nixpkgs = if (args ? nixpkgs)
    then args.nixpkgs
    else if (_sources ? nixpkgs)
      then _sources.nixpkgs
      else haskellNix.sources.nixpkgs-1909;

  _pkgs = import nixpkgs (haskellNix.nixpkgsArgs // _args);
  pkgs = _pkgs.pkgsCross.musl64;

  pkgSet = with pkgs.haskell-nix; mkCabalProjectPkgSet {
    plan-pkgs = (importAndFilterProject (callCabalProjectToNix {
      src = cleanSourceHaskell { src = ./..; name = "boxpub-source"; };
      index-state = "2020-04-25T00:00:00Z";
      index-sha256 = "0zsczchlvpp9vqqqwq698g5qvp49x9pvzfgjl2mrrsrz6nzk2mig";
      plan-sha256 = "1rvnq4m0mvd4r94gfrh46ranrd3zbwswkvlqdajkbzx4khyqk3si";
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
