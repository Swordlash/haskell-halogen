#!/bin/bash

set -ex

echo "Running native"
cabal test

echo "Running ghcjs"
cabal test --project-file=cabal-ghcjs.project

echo "Running wasm"
. "$HOME/.ghc-wasm/env"
workspace_dir=$(pwd)
wasm_ghc=$(command -v wasm32-wasi-ghc-9.14.1.20260731)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg-9.14.1.20260731)
cabal test \
  --project-file=cabal-wasm.project \
  --builddir=dist-newstyle/wasm-test \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --test-wrapper="$workspace_dir/toolchain/wasm-test-wrapper.sh"
