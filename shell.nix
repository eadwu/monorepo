{ compiler ? "ghc865" }:

let
  hie = (import (builtins.fetchTarball {
    url = "https://github.com/infinisil/all-hies/tarball/master";
  }) { }).versions."${compiler}";

  inherit (import ./default.nix { }) hsPkgs;
  inherit (hsPkgs) nixpkgs iohaskell;
in with nixpkgs.haskell.packages."${compiler}"; hsPkgs.shellFor {
  exactDeps = true;
  withHoogle = true;
  packages = ps: with ps; [ boxpub ];
  buildInputs = [ ghc cabal-install iohaskell.nix-tools ];
}
