{ nixpkgs ? import ./nixpkgs.nix { } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./spec/haskell.json);
in nixpkgs.stdenv.mkDerivation {
  name = "haskell-nix";

  src = nixpkgs.fetchgit {
    inherit (parsedMetaData) rev sha256;
    url = "https://github.com/input-output-hk/haskell.nix";
  };

  patches = [ ./patches/expand-derivation-attrs.patch ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -r . $out
  '';
}
