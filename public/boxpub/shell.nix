{ nixpkgs ? import ./nixpkgs.nix { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;
with haskell.packages."${compiler}";

let
  hie = (import (builtins.fetchTarball {
    url = "https://github.com/infinisil/all-hies/tarball/master";
  }) { }).versions."${compiler}";

  boxpub = (import ./default.nix { }).boxpub;
in shellFor {
  withHoogle = true;
  packages = ps: [ hie boxpub ];
  nativeBuildInputs = [ hie cabal-install ];

  shellHook = ''
    # Ensure HIE can find the hoogle database
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/default.hoo";
  '';
}
