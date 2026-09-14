#!/bin/sh
set -eu

cd "$(dirname "$0")"
. "$HOME/.ghc-wasm/env"

wasm_ghc=$(command -v wasm32-wasi-ghc)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg)
wasm_hsc2hs=$(command -v wasm32-wasi-hsc2hs)
build_dir=dist-newstyle/wasm-pixi-component
public_dir=$build_dir/public

cabal build \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  exe:halogen-pixi-example "$@"

wasm_binary=$(cabal list-bin \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  exe:halogen-pixi-example "$@")
wasm_libdir=$($wasm_ghc --print-libdir)

mkdir -p "$public_dir"
node "$wasm_libdir/post-link.mjs" --input "$wasm_binary" --output "$public_dir/ghc_wasm_jsffi.js"
cp "$wasm_binary" "$public_dir/halogen-pixi-example.wasm"
cp dev/pixi.html "$public_dir/index.html"
cp dev/pixi-wasm.js "$public_dir/index.js"
cp dev/pixi-tile.svg "$public_dir/pixi-tile.svg"

printf '\nPixi wasm example ready in %s. Run ./serve-wasm-pixi.sh to serve it.\n' "$public_dir"
