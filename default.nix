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
    rev = "1.2.1.0";
    sha256 = "1x6379s2cd8crgibwhybf17j3i47h9lvmp62zh91ginw6661q68i";
  }) {
    scalpel = scalpel_0_6_0.override {
      scalpel-core = scalpel-core_0_6_0;
    };
  };
}
