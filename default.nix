{ pkgs ? import <nixpkgs> { }, compiler ? "ghc864" }:

with pkgs;

{
  boxpub = with haskell.packages."${compiler}"; callPackage ./release.nix {
    scalpel = scalpel_0_6_0.override {
      scalpel-core = scalpel-core_0_6_0;
    };
  };
}
