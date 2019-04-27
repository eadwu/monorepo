{ nixpkgs ? import ./nixpkgs.nix { }, compiler ? "ghc864" }:

with nixpkgs.pkgs;

let
  ghc = haskell.packages."${compiler}".override {
    overrides = self: super: {
      scalpel = self.callHackage "scalpel" "0.6.0" { };
      scalpel-core = self.callHackage "scalpel-core" "0.6.0" { };

      pandoc = super.pandoc.overrideAttrs (oldAttrs: {
        prePatch = (oldAttrs.prePatch or "") + ''
          # Conform to EPUB 3.2 ttf font specifications
          # See https://w3c.github.io/publ-epub-revision/epub32/spec/epub-spec.html#sec-cmt-supported
          sed -i 's@\(("ttf","\)application/x-font-truetype\(")\)@\1application/font-sfnt\2@' src/Text/Pandoc/MIME.hs
        '';
      });
    };
  };
in with ghc; {
  boxpub = developPackage {
    root = ./.;
    modifier = drv: with haskell.lib; generateOptparseApplicativeCompletion
      "boxpub"
      (doCoverage (doHaddock (doBenchmark drv)));

    # Interferes too much in nix-shell, if the environment is needed manually do {{drv}}.env
    returnShellEnv = false;
  };

  boxpub-1_x = haskell.lib.generateOptparseApplicativeCompletion
    "boxpub"
    (callCabal2nix "boxpub-1_x" (fetchgit {
      url = ./.;
      rev = "1.2.2.0";
      sha256 = "0qflibwbzav7r19n51vya1zi238jz5xr0zf3pz1hhkl7bapw7cvi";
    }) { });
}
