#!/bin/sh
set -eu

cd "$(dirname "$0")"
. "$HOME/.ghc-wasm/env"

wasm_ghc=$(command -v wasm32-wasi-ghc-9.14.1.20260731)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg-9.14.1.20260731)
wasm_hsc2hs=$(command -v wasm32-wasi-hsc2hs-9.14.1.20260731)
build_dir=dist-newstyle/wasm
public_dir=$build_dir/public

cabal build \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  exe:halogen-core-prototype "$@"

wasm_binary=$(cabal list-bin \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  exe:halogen-core-prototype "$@")
wasm_libdir=$($wasm_ghc --print-libdir)

mkdir -p "$public_dir"
node "$wasm_libdir/post-link.mjs" --input "$wasm_binary" --output "$public_dir/ghc_wasm_jsffi.js"
cp "$wasm_binary" "$public_dir/halogen-core-prototype.wasm"
cp dev/index.html "$public_dir/index.html"
cp dev/wasm.js "$public_dir/index.js"

printf '\nWasm build ready in %s. Run ./serve-wasm.sh to serve it.\n' "$public_dir"
