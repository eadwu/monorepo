let
  sources = let
    parse = file: builtins.fromJSON (builtins.readFile file);
  in {
    "haskell.nix" = parse ./spec/haskell.json;
  };
in builtins.mapAttrs
  (_: spec: builtins.fetchTarball { inherit (spec) url sha256; })
  sources
