{ nixpkgs ? import ./nixpkgs.nix { } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./spec/gitignore.json);
  gitignoreNix = builtins.fetchGit {
    inherit (parsedMetaData) rev;
    ref = "master";
    url = "https://github.com/hercules-ci/gitignore";
  };
in import gitignoreNix { inherit (nixpkgs) lib; }
