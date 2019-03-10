{ mkDerivation, base, fs, http-client, http-client-tls
, http-conduit, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "boxpub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ fs ];
  executableHaskellDepends = [
    base http-client http-client-tls http-conduit optparse-applicative
  ];
  executableSystemDepends = [ fs ];
  homepage = "https://git.sr.ht/~eadwu/boxpub";
  description = "boxnovel epub generator";
  license = stdenv.lib.licenses.bsd3;
}
