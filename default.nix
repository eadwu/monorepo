{ nixpkgs ? import ./nixpkgs.nix { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;

{
  boxpub = haskell.packages."${compiler}".developPackage {
    root = ./.;
    source-overrides = {
      scalpel = "0.6.0";
      scalpel-core = "0.6.0";
    };
  };

  boxpub-1_x = with haskell.packages."${compiler}"; callCabal2nix "boxpub-1_x" (fetchgit {
    url = ./.;
    rev = "1.1.2.0";
    sha256 = "0cfyrnqff50s2w5s6b4fqaf3hlh94npxyndc776j72wk1d4spc8q";
  }) {
    scalpel = scalpel_0_6_0.override {
      scalpel-core = scalpel-core_0_6_0;
    };
  };
}
