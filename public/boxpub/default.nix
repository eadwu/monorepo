{ nixpkgs ? import ./nix/nixpkgs.nix { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;

let
  pkgSet = import ./nix { };
  boxpub = pkgSet.boxpub.components.exes.boxpub.overrideAttrs (oldAttrs: {
    src = nixpkgs.gitignoreSource ./.;
  });
in with ghc; {
  inherit boxpub pkgSet;

  boxpub-1_x = boxpub.overrideAttrs (oldAttrs: rec {
    name = "${oldAttrs.pname}-${version}";
    version = "1.2.3.0";

    src = fetchgit {
      url = ./.;
      rev = version;
      sha256 = "07qqrzdvm665p4s1r3mpw617rk9k1vvirzr4k7djmm2py7kcg0lz";
    };
  });
}
