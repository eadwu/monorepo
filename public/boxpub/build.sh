#! /usr/bin/env bash

function fail() {
  echo "$1" >&2
  exit 1
}

CARGO=$(command -v cargo)

if [ -z "$CARGO" ]; then
  fail "cargo executable not found"
fi

$CABAL new-update
$CABAL new-build
