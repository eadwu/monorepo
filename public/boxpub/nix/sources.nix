let
  sources = let
    parse = file: builtins.fromJSON (builtins.readFile file);
  in {
    "haskell.nix" = parse ./spec/haskell.json;
    "nixpkgs" = rec {
      rev = "5272327b81ed355bbed5659b8d303cf2979b6953";
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
    };
  };
in builtins.mapAttrs
  (_: spec: builtins.fetchTarball { inherit (spec) url sha256; })
  sources
