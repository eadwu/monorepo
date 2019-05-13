{ pkgs ? import ../nixpkgs.nix { } }:

let
  haskell_nix = pkgs.stdenv.mkDerivation {
    name = "haskell_nix";

    src = builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
    };

    patches = [
      ./expand-derivation-attrs.patch
    ];

    dontBuild = true;

    installPhase = ''
      mkdir -p $out
      cp -r . $out
    '';
  };

  haskell = import haskell_nix { inherit pkgs; };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [ ];
    modules = [
      {
        packages.transformers-compat.flags.five-three = true; # transformers >= 0.5.3
        packages.time-locale-compat.flags.old-locale = false; # time >= 1.5
      }

      {
        # Conform to EPUB 3.2 ttf font specifications
        # See https://w3c.github.io/publ-epub-revision/epub32/spec/epub-spec.html#sec-cmt-supported
        packages.pandoc.preBuild = ''
          sed -i 's@\(("ttf","\)application/x-font-truetype\(")\)@\1application/font-sfnt\2@' src/Text/Pandoc/MIME.hs
        '';

        # Adjust env to correct paths since cabal `nix-style` installation isn't used
        # See https://github.com/haskell/cabal/issues/5543
        packages.boxpub.postInstall = ''
          . ${pkgs.makeWrapper}/nix-support/setup-hook
          ghc_name="ghc-$(ghc --numeric-version)"
          pkg_name="$pname-$version"
          install_suffix="${builtins.currentSystem}-$ghc_name/$pkg_name"

          wrapProgram $out/bin/boxpub \
            --set boxpub_libdir $out/lib/$install_suffix \
            --set boxpub_datadir $out/share/$install_suffix
        '';
      }
    ];
  };
in
  pkgSet.config.hsPkgs
