#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch-scripts

nix-prefetch-git --rev refs/heads/nixos-19.03 https://github.com/NixOS/nixpkgs-channels.git > nixpkgs.json
