{ pkgs ? import <nixpkgs> { }, compiler ? "ghc863" }:

with pkgs;

stdenv.mkDerivation {
  name = "boxpub";

  buildInputs = [
    cargo
    rustc

    cabal2nix
    haskell.packages."${compiler}".ghc
    haskell.packages."${compiler}".cabal-install
  ];
}
