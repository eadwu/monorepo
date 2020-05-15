{ nixpkgs ? <nixpkgs>
, pkgs ? import nixpkgs {} }:

with pkgs;

let
  common = callPackage (import ./generic.nix) { };

  defaultArgs = {
    pname = "boxpub";
    version = "3.0.0";
    src = lib.cleanSource ./.;
    cargoBuildFlags = [ "-Z unstable-options" "-Z no-index-update" "-Z avoid-dev-deps" ];
  };

  wasi = common (defaultArgs // {
    pname = "${defaultArgs.pname}-wasi";
    target = "wasm32-wasi";
    cargoSha256 = "0nmlcs6qa4fnplijrwgp8v3ff44a345mhp702y7v5mc2nb88zkyy";

    nativeBuildInputs = [ pkgs.wabt ];
  });

  wasm = common (defaultArgs // {
    pname = "${defaultArgs.pname}-wasm";
    target = "wasm32-unknown-unknown";
    cargoSha256 = "175h0icxvvjc058z7vbr9631jdqg8rx1iihqr3kjln8q7xk4066y";

    nativeBuildInputs = [ pkgs.wabt ];
  });
in rustPlatform.buildRustPackage (defaultArgs // {
  cargoSha256 = "11ryvnzdv4dpf2w2x0v9vhr295fg9fs3pggzf8anzff2gj8bxqn7";
  # https://github.com/NixOS/nixpkgs/issues/87081
  nativeBuildInputs = [ pkgs.diffutils ];

  passthru = { inherit wasi wasm; };
})
