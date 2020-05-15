{
  description = "Nix flake for boxpub";

  inputs.nixpkgs = { type = "github"; owner = "eadwu"; repo = "nixpkgs"; ref = "develop"; };
  inputs.custom = { type = "github"; owner = "eadwu"; repo = "flakes"; };

  outputs = { self, nixpkgs, custom }:
    let
      lib = nixpkgs.lib;
      systems = [ "x86_64-linux" ];
      forAllSystems = f: lib.genAttrs systems (system: f system);
    in
    rec {
      defaultPackage = forAllSystems (system: packages.${system}.boxpub);

      packages = forAllSystems
        (
          system:
          let
            args = {
              inherit system;
              config.allowUnfree = true;
              overlays =
                [
                  (custom.overlays system)
                  (import ./nix/overlays/rust.nix)
                ];
            };
            pkgs = import nixpkgs args;
          in
          {
            boxpub = import ./default.nix { inherit pkgs nixpkgs; };
            ebooklib = import ./ebooklib.nix { inherit pkgs nixpkgs; };
          }
        );
    };
}
