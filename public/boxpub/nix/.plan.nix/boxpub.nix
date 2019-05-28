{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "boxpub"; version = "1.2.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "edmund.wu@protonmail.com";
      author = "Edmund Wu";
      homepage = "https://git.sr.ht/~eadwu/boxpub";
      url = "";
      synopsis = "boxnovel epub generator";
      description = "";
      buildType = "Simple";
      };
    components = {
      exes = {
        "boxpub" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.text)
            (hsPkgs.spawn)
            (hsPkgs.pandoc)
            (hsPkgs.scalpel)
            (hsPkgs.filepath)
            (hsPkgs.directory)
            (hsPkgs.temporary)
            (hsPkgs.bytestring)
            (hsPkgs.http-client)
            (hsPkgs.data-default)
            (hsPkgs.http-client-tls)
            (hsPkgs.optparse-applicative)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../.; }
