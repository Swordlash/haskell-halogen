#!/bin/sh
# The in-memory DOM and Static tests under standalone WASI, without JSFFI.
set -eu
cd "$(dirname "$0")/.."
. "$HOME/.ghc-wasm/env"
build_dir=dist-newstyle/wasi
rm -f "$build_dir/cache/compiler"
cabal test haskell-halogen-core:test:Halogen-core-test \
  --project-file=cabal-wasi.project --builddir="$build_dir" \
  --with-compiler="$(command -v wasm32-wasi-ghc)" --with-hc-pkg="$(command -v wasm32-wasi-ghc-pkg)" \
  --with-hsc2hs="$(command -v wasm32-wasi-hsc2hs)" \
  --test-wrapper="$(pwd)/toolchain/wasi-test-wrapper.sh" "$@"
mkdir -p "$build_dir/prerender"
sh toolchain/run-wasi.sh halogen-example-vanilla:exe:halogen-example-vanilla-prerender \
  examples/vanilla/web/index.html "$build_dir/prerender"
test -s "$build_dir/prerender/index.html"
