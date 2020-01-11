{ compiler ? "ghc865" }:

let
  hsPkgs = import ./nix { };
  inherit (hsPkgs) nixpkgs iohaskell;

  boxpub = hsPkgs.boxpub.components.all.overrideAttrs (oldAttrs: {
    src = iohaskell.cleanSourceHaskell { src = ./.; };
  });
in {
  inherit boxpub hsPkgs;

  boxpub-1_x = boxpub.overrideAttrs (oldAttrs: rec {
    name = "${oldAttrs.pname}-${version}";
    version = "1.2.3.0";

    src = nixpkgs.fetchgit {
      url = "https://git.sr.ht/~eadwu/boxpub";
      rev = version;
      sha256 = "07qqrzdvm665p4s1r3mpw617rk9k1vvirzr4k7djmm2py7kcg0lz";
    };
  });
}
