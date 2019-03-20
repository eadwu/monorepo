{ mkDerivation, base, directory, http-client, http-client-tls
, optparse-applicative, scalpel, stdenv
}:
mkDerivation {
  pname = "boxpub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory ];
  executableHaskellDepends = [
    base directory http-client http-client-tls optparse-applicative
    scalpel
  ];
  doHaddock = false;
  homepage = "https://git.sr.ht/~eadwu/boxpub";
  description = "boxnovel epub generator";
  license = stdenv.lib.licenses.bsd3;
}
