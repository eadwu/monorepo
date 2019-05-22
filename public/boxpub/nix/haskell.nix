{ bootstrap ? import <nixpkgs> { } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./spec/haskell.json);
  haskell_nix = builtins.fetchGit {
    inherit (parsedMetaData) rev;
    ref = "master";
    url = "https://github.com/input-output-hk/haskell.nix";
  };
in bootstrap.stdenv.mkDerivation {
  name = "haskell_nix";

  src = haskell_nix;

  patches = [
    ./expand-derivation-attrs.patch
  ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -r . $out
  '';
}
