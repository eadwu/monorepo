{ compiler ? "ghc865" }:

let
  _pkgs = import ./nix { inherit compiler; };
in _pkgs.hsPkgs.boxpub.components.all
