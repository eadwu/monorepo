{ nixpkgs ? import ./nixpkgs.nix { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;

let
  hie = (import (builtins.fetchTarball {
    url = "https://github.com/infinisil/all-hies/tarball/master";
  }) { }).versions."${compiler}";

  boxpub = (import ./default.nix { }).boxpub;
  ghc = haskell.packages."${compiler}".ghcWithPackages (ps: boxpub.buildInputs);
in stdenv.mkDerivation {
  name = "boxpub";

  buildInputs = [
    ghc
    hie
    haskell.packages."${compiler}".hlint
    haskell.packages."${compiler}".cabal-install
  ];
}
