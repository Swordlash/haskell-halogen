#!/bin/sh
set -eu

wasm_binary=$1
shift
wasm_ghc=$(command -v wasm32-wasi-ghc)
wasm_libdir=$($wasm_ghc --print-libdir)
temp_dir=$(mktemp -d)
trap 'rm -rf "$temp_dir"' EXIT HUP INT TERM

# A suite built with hspec-halogen is a reactor that exports hs_start, and
# the hspec-halogen executable (test-wasm.sh builds it) runs it in headless
# Chromium. It gets none of the Node test globals below, which would make
# Playwright's own process look like a page.
if node -e '
  const bytes = require("node:fs").readFileSync(process.argv[1]);
  const names = WebAssembly.Module.exports(new WebAssembly.Module(bytes)).map((e) => e.name);
  process.exit(names.includes("hs_start") ? 0 : 1);
' "$wasm_binary"; then
  "${HSPEC_HALOGEN:?build it first: HSPEC_HALOGEN=\$(sh toolchain/build-hspec-halogen.sh)}" test "$wasm_binary" "$@"
  exit
fi

node "$wasm_libdir/post-link.mjs" \
  --input "$wasm_binary" \
  --output "$temp_dir/ghc_wasm_jsffi.mjs"

browser_test_dir=$temp_dir
. "$(dirname "$0")/browser-test-env.sh"

node "$(dirname "$0")/wasm-test-runner.mjs" \
  "$wasm_binary" \
  "$temp_dir/ghc_wasm_jsffi.mjs" \
  "$@"
