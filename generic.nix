{ lib, diffutils, rustPlatform }:
{ version, ... }@args:

rustPlatform.buildRustPackage (args // rec {
  # https://github.com/NixOS/nixpkgs/issues/87081
  nativeBuildInputs = [ diffutils ]
    ++ (args.nativeBuildInputs or []);

  # wasm2c currently support only default feature flags
  WASM_FEATURES = [];

  postInstall = ''
    mkdir -p $out/include
    for f in $out/bin/*.wasm; do
      FILENAME="$(basename "$f" .wasm | sed 's/_/-/g')"
      wasm2c "$f" ${lib.concatMapStringsSep "\n" (feat: "--enable-${feat}") WASM_FEATURES} \
        --no-debug-names -o "$out/include/$FILENAME.c"
    done
  '';
})
