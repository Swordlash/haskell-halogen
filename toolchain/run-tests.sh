#!/bin/bash
# Run the test suites across all three backends.
set -ex

cd "$(dirname "$0")/.."

echo "Running native"
cabal test all

echo "Running ghcjs"
cabal test all --project-file=cabal-ghcjs.project

echo "Running wasm"
. "$HOME/.ghc-wasm/env"
workspace_dir=$(pwd)
wasm_ghc=$(command -v wasm32-wasi-ghc)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg)
cabal test all \
  --project-file=cabal-wasm.project \
  --builddir=dist-newstyle/wasm-test \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --test-wrapper="$workspace_dir/toolchain/wasm-test-wrapper.sh"
