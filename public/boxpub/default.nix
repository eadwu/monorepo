{ nixpkgs ? import ./nixpkgs.nix { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;

let
  boxpub = (import ./nix { }).boxpub.components.exes.boxpub;
in with ghc; {
  inherit boxpub;
  boxpub-1_x = boxpub.overrideAttrs (oldAttrs: rec {
    name = "${oldAttrs.pname}-${version}";
    version = "1.2.2.0";

    src = fetchgit {
      url = ./.;
      rev = version;
      sha256 = "0qflibwbzav7r19n51vya1zi238jz5xr0zf3pz1hhkl7bapw7cvi";
    };
  });
}
