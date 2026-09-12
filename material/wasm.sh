#!/bin/sh
set -eu

cd "$(dirname "$0")"
. "$HOME/.ghc-wasm/env"

wasm_ghc=$(command -v wasm32-wasi-ghc)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg)
wasm_hsc2hs=$(command -v wasm32-wasi-hsc2hs)
build_dir=dist-newstyle/wasm
public_dir=$build_dir/public

# Cabal may reuse the host ghc-pkg from this cache when switching toolchains.
rm -f "$build_dir/cache/compiler"

cabal build \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  exe:halogen-material-app "$@"

wasm_binary=$(find "$build_dir/build" -type f -name halogen-material-app.wasm -print -quit)
wasm_libdir=$($wasm_ghc --print-libdir)

mkdir -p "$public_dir"
node "$wasm_libdir/post-link.mjs" --input "$wasm_binary" --output "$public_dir/ghc_wasm_jsffi.js"
cp "$wasm_binary" "$public_dir/halogen-material-app.wasm"
cp dev/index.html "$public_dir/index.html"
cp dev/wasm.js "$public_dir/index.js"
npx webpack-cli --config webpack.config-wasm.js

printf '\nWasm build ready in %s.\n' "$public_dir"
