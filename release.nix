{ mkDerivation, base, http-client, http-client-tls
, optparse-applicative, pandoc, scalpel, stdenv
}:
mkDerivation {
  pname = "boxpub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base http-client http-client-tls optparse-applicative pandoc
    scalpel
  ];
  homepage = "https://git.sr.ht/~eadwu/boxpub";
  description = "boxnovel epub generator";
  license = stdenv.lib.licenses.bsd3;
}
