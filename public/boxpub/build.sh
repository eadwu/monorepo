#! /usr/bin/env nix-shell
#! nix-shell -i bash

function fail() {
  echo "$1" >&2
  exit 1
}

CABAL=$(command -v cabal)
CARGO=$(command -v cargo)

if [ -z "$CABAL" ]; then
  fail "cabal executable not found"
fi

if [ -z "$CARGO" ]; then
  fail "cargo executable not found"
fi

$CARGO build --lib --release
$CABAL new-update
$CABAL new-build
