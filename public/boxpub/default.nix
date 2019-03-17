{ pkgs ? import <nixpkgs> { }, compiler ? "ghc864" }:

with pkgs;

rec {
  fs = rustPlatform.buildRustPackage rec {
    name = "fs-${version}";
    version = "0.1.0";

    src = ./.;

    cargoSha256 = "0jacm96l1gw9nxwavqi1x4669cg6lzy9hr18zjpwlcyb3qkw9z7f";
  };

  boxpub = with haskell.packages."${compiler}"; callPackage ./release.nix {
    inherit fs;
    scalpel = scalpel_0_6_0.override {
      scalpel-core = scalpel-core_0_6_0;
    };
  };
}
