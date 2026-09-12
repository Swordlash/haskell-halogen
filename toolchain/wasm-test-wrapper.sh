#!/bin/sh
set -eu

wasm_binary=$1
shift
wasm_ghc=$(command -v wasm32-wasi-ghc-9.14.1.20260731)
wasm_libdir=$($wasm_ghc --print-libdir)
temp_dir=$(mktemp -d)
trap 'rm -rf "$temp_dir"' EXIT HUP INT TERM

node "$wasm_libdir/post-link.mjs" \
  --input "$wasm_binary" \
  --output "$temp_dir/ghc_wasm_jsffi.mjs"
node "$(dirname "$0")/wasm-test-runner.mjs" \
  "$wasm_binary" \
  "$temp_dir/ghc_wasm_jsffi.mjs" \
  "$@"
