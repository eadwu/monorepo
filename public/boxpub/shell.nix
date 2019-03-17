{ pkgs ? import <nixpkgs> { }, compiler ? "ghc864" }:

with pkgs;

let
  hies = (import (builtins.fetchTarball {
    url = "https://github.com/domenkozar/hie-nix/tarball/master";
  }) { }).hies;

  boxpub = (import ./default.nix { }).boxpub;
  ghc = haskell.packages."${compiler}".ghcWithPackages (ps: boxpub.propagatedBuildInputs);
in stdenv.mkDerivation {
  name = "boxpub";

  buildInputs = [
    cargo
    rustc

    ghc
    hies
    cabal2nix
    haskell.packages."${compiler}".hlint
    haskell.packages."${compiler}".cabal-install
  ];
}
