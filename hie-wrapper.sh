#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash

hie-wrapper --lsp "$@"
