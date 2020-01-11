{ bootstrap ? import <nixpkgs> { config = { }; } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./spec/nixpkgs.json);
  nixpkgs = bootstrap.fetchgit {
    inherit (parsedMetaData) rev sha256;
    url = "https://github.com/NixOS/nixpkgs-channels";
  };
in (import nixpkgs {
  config = { };
  overlays = [
    (_: super: { inherit nixpkgs; })
  ];
})
