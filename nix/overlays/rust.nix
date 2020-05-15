self: super:

let
  inherit (super) rustPlatform;

  wasm32-wasi = self.pkgs.fetchzip {
    url = "https://static.rust-lang.org/dist/${rustPlatform.date}/rust-std-nightly-wasm32-wasi.tar.gz";
    sha256 = "080hk175ljvl0jpsssh7mr541ps5kc7w881439vwvnls3cfijrgn";
  };

  wasm32-unknown-unknown = self.pkgs.fetchzip {
    url = "https://static.rust-lang.org/dist/${rustPlatform.date}/rust-std-nightly-wasm32-unknown-unknown.tar.gz";
    sha256 = "0sln05n3qsha89y903slk2a8yyk460rhlwvk98f0vmk9712kgkm1";
  };
in {
  rustPlatform = rustPlatform // rec {
    # need the fixed --sysroot
    buildRustPackage = rustPlatform.buildRustPackage.override { rustc = rust; };

    rust = rustPlatform.rust.overrideAttrs(oldAttrs: {
      buildCommand = (oldAttrs.buildCommand or "") + ''
        cp -r ${wasm32-wasi}/rust-std-wasm32-wasi/lib/rustlib/wasm32-wasi $out/lib/rustlib
        cp -r ${wasm32-unknown-unknown}/rust-std-wasm32-unknown-unknown/lib/rustlib/wasm32-unknown-unknown $out/lib/rustlib
      '';
    });
  };
}
