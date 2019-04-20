{ bootstrap ? import <nixpkgs> { } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  nixpkgs = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    inherit (parsedMetaData) rev sha256;
  };
in import nixpkgs { }
