#!/bin/sh
# Run the test suites with the GHC WebAssembly backend, under a node wrapper
# that instantiates each test binary (see wasm-test-wrapper.sh).
#
# Shares dist-newstyle/wasm with build-wasm.sh so the example builds and the
# test run reuse each other's artifacts.
set -eu

cd "$(dirname "$0")/.."
. "$HOME/.ghc-wasm/env"

wasm_ghc=$(command -v wasm32-wasi-ghc)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg)
wasm_hsc2hs=$(command -v wasm32-wasi-hsc2hs)
build_dir=dist-newstyle/wasm

# Cabal may reuse the host ghc-pkg from this cache when switching toolchains.
rm -f "$build_dir/cache/compiler"

exec cabal test all \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  --test-wrapper="$(pwd)/toolchain/wasm-test-wrapper.sh" \
  "$@"
