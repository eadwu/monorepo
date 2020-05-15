{ nixpkgs ? <nixpkgs>
, pkgs ? import nixpkgs {}
, python ? "python38" }:

with pkgs;

let
  pythonPackages = pkgs.${python}.withPackages (ps: with ps; [ Nuitka ]);
in stdenv.mkDerivation rec {
  pname = "ebooklib";
  version = "0.17.1";

  src = fetchFromGitHub {
    owner = "aerkalov";
    repo = "ebooklib";
    rev = "v${version}";
    sha256 = "1575rs67086hvgsvr6b4w5hbprp7m9ayykdclmd8c8x4cqifpx3r";
  };

  nativeBuildInputs = [ pythonPackages ];

  buildPhase = ''
    export HOME=$TMPDIR
    python -m nuitka --output-dir $out ebooklib/__init__.py
  '';

  dontInstall = true;
  dontStrip = true;
  dontShrink = true;
}
