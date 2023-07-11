#! /usr/bin/env nix-shell
#! nix-shell -i bash -p wasmer clang zig patchelf binaryen

# https://github.com/wasmerio/wasmer/issues/3834, beware of hyphens

# # Optimize for size.
# wasm-opt -Os -o output.wasm input.wasm
# # Optimize aggressively for size.
# wasm-opt -Oz -o output.wasm input.wasm
# # Optimize for speed.
# wasm-opt -O -o output.wasm input.wasm
# # Optimize aggressively for speed.
# wasm-opt -O3 -o output.wasm input.wasm

set -xe
INTERPRETER=/nix/store/3n58xw4373jp0ljirf06d8077j15pc4j-glibc-2.37-8/lib/ld-linux-x86-64.so.2

shopt -s expand_aliases
alias wasmer-executable="wasmer create-exe --enable-verifier --enable-all"
alias wasmer-object="wasmer create-obj --enable-verifier --enable-all"

rm -rf build
mkdir build

# https://doc.rust-lang.org/nightly/rustc/platform-support.html
# export RUSTFLAGS=--cfg=web_sys_unstable_apis
cargo build --release
# cargo build --target wasm32-wasi --release
# wasmer-object target/wasm32-wasi/release/webgpu.wasm -o build/webgpu # library
# wasmer-executable target/wasm32-wasi/release/webgpu_bin.wasm -o build/rust

nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz \
  --command wasm32-wasi-ghc tensor/hs.hs -o build/haskell.wasm
wasmer-executable build/haskell.wasm -o build/haskell
rm build/haskell.wasm

# GOOS=wasip1 GOARCH=wasm go build -o build/go.wasm tensor/go.go
nix-shell -p tinygo --run \
  'tinygo build -target=wasi -o build/go.wasm tensor/go.go'
wasmer-executable build/go.wasm -o build/go
rm build/go.wasm

# https://00f.net/2019/04/07/compiling-to-webassembly-with-llvm-and-clang/
# -s Remove all symbol table and relocation information from the executable.
zig cc --target=wasm32-wasi -Os -s -o build/c.wasm tensor/c.c
wasmer-executable build/c.wasm -o build/c
rm build/c.wasm

zig c++ --target=wasm32-wasi -Os -s -o build/cpp.wasm tensor/cpp.cpp
wasmer create-exe build/cpp.wasm -o build/cpp
rm build/cpp.wasm

# https://github.com/wasmerio/wasmer/issues/3436

for f in build/*;
do
  patchelf --set-interpreter "${INTERPRETER}" "$f" || true
  $f || true
done

# Scala, Java, C++
