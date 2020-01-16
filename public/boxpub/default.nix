{ compiler ? "ghc865" }:

let
  hsPkgs = import ./nix { };
  inherit (hsPkgs) nixpkgs iohaskell;

  boxpub = hsPkgs.boxpub.components.all.overrideAttrs (oldAttrs: {
    src = iohaskell.cleanSourceHaskell { src = ./.; };
  });
in {
  inherit boxpub hsPkgs;

  boxpub-1_x = (import (nixpkgs.fetchgit {
    url = "https://git.sr.ht/~eadwu/boxpub";
    rev = "2d5a022e95d924e7c27778deef1908b164b1343a";
    sha256 = "0jry51vk96k3lly4xv0548dl27z0wnk2zx6g7f2xv5kh7h44qkjr";
  }) {}).boxpub-1_x;
}
