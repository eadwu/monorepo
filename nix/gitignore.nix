{ nixpkgs ? import ./nixpkgs.nix { } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./spec/gitignore.json);
  gitignoreNix = nixpkgs.fetchgit {
    inherit (parsedMetaData) rev sha256;
    url = "https://github.com/hercules-ci/gitignore";
  };
in import gitignoreNix { inherit (nixpkgs) lib; }
