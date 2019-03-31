{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;

let
  hies = (import (builtins.fetchTarball {
    url = "https://github.com/domenkozar/hie-nix/tarball/master";
  }) { }).hies;

  boxpub = (import ./default.nix { }).boxpub;
  ghc = haskell.packages."${compiler}".ghcWithPackages (ps: boxpub.buildInputs);
in stdenv.mkDerivation {
  name = "boxpub";

  buildInputs = [
    ghc
    hies
    haskell.packages."${compiler}".hlint
    haskell.packages."${compiler}".cabal-install
  ];
}
