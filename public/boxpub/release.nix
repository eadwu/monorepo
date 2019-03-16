{ mkDerivation, base, fs, optparse-applicative, scalpel, stdenv }:
mkDerivation {
  pname = "boxpub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ fs ];
  executableHaskellDepends = [ base optparse-applicative scalpel ];
  executableSystemDepends = [ fs ];
  homepage = "https://git.sr.ht/~eadwu/boxpub";
  description = "boxnovel epub generator";
  license = stdenv.lib.licenses.bsd3;
}
