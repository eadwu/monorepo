{ compiler ? "ghc865", ... }@args:

let
  _pkgs = import ./nix ({ inherit compiler; } // args);
in _pkgs.hsPkgs.boxpub.components.all
