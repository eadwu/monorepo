{ bootstrap ? import <nixpkgs> { } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./spec/haskell.json);
  haskellNix = builtins.fetchGit {
    inherit (parsedMetaData) rev;
    ref = "master";
    url = "https://github.com/input-output-hk/haskell.nix";
  };
in bootstrap.stdenv.mkDerivation {
  name = "haskell-nix";

  src = haskellNix;

  patches = [
    ./expand-derivation-attrs.patch
  ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -r . $out
  '';
}
