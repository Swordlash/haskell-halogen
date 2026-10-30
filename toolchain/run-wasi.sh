#!/bin/sh
# Build and run a standalone WASI executable, without a native prerenderer.
# Usage: sh toolchain/run-wasi.sh PACKAGE:exe:NAME [program arguments...]
set -eu
cd "$(dirname "$0")/.."
target=${1:?usage: run-wasi.sh PACKAGE:exe:NAME [program arguments...]}
shift
. "$HOME/.ghc-wasm/env"
build_dir=dist-newstyle/wasi
rm -f "$build_dir/cache/compiler"
cabal build "$target" --project-file=cabal-wasi.project --builddir="$build_dir" \
  --with-compiler="$(command -v wasm32-wasi-ghc)" --with-hc-pkg="$(command -v wasm32-wasi-ghc-pkg)" \
  --with-hsc2hs="$(command -v wasm32-wasi-hsc2hs)"
binary=$(cabal list-bin -v0 "$target" --project-file=cabal-wasi.project --builddir="$build_dir" \
  --with-compiler="$(command -v wasm32-wasi-ghc)" --with-hc-pkg="$(command -v wasm32-wasi-ghc-pkg)" \
  --with-hsc2hs="$(command -v wasm32-wasi-hsc2hs)")
exec wasmtime run --dir . "$binary" "$@"
