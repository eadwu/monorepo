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
    rev = "1.2.1.0";
    sha256 = "1x6379s2cd8crgibwhybf17j3i47h9lvmp62zh91ginw6661q68i";
  }) {
    scalpel = scalpel_0_6_0.override {
      scalpel-core = scalpel-core_0_6_0;
    };
  };
}
