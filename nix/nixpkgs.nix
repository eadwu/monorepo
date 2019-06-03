{ bootstrap ? import <nixpkgs> { config = { }; } }:

let
  parsedMetaData = builtins.fromJSON (builtins.readFile ./spec/nixpkgs.json);
  nixpkgs = builtins.fetchGit {
    inherit (parsedMetaData) rev;
    ref = "nixos-19.03";
    url = "https://github.com/NixOS/nixpkgs-channels";
  };
in (import nixpkgs {
  config = { };
}) // {
  inherit nixpkgs;
  inherit (import ./gitignore.nix { }) gitignoreSource;
}
