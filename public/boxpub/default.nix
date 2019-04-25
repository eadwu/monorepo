{ nixpkgs ? import ./nixpkgs.nix { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;

let
  ghcPackages = haskell.packages."${compiler}".override {
    overrides = self: super: {
      pandoc = super.pandoc.overrideAttrs (oldAttrs: {
        prePatch = (oldAttrs.prePatch or "") + ''
          # Conform to EPUB 3.2 ttf font specifications
          # See https://w3c.github.io/publ-epub-revision/epub32/spec/epub-spec.html#sec-cmt-supported
          sed -i 's@\(("ttf","\)application/x-font-truetype\(")\)@\1application/font-sfnt\2@' src/Text/Pandoc/MIME.hs
        '';
      });
    };
  };
in with ghcPackages; {
  boxpub = developPackage {
    root = ./.;
    source-overrides = {
      scalpel = "0.6.0";
      scalpel-core = "0.6.0";
    };
  };

  boxpub-1_x = callCabal2nix "boxpub-1_x" (fetchgit {
    url = ./.;
    rev = "1.2.2.0";
    sha256 = "0qflibwbzav7r19n51vya1zi238jz5xr0zf3pz1hhkl7bapw7cvi";
  }) {
    scalpel = scalpel_0_6_0.override {
      scalpel-core = scalpel-core_0_6_0;
    };
  };
}
