#!/bin/bash
# Run the test suites across all three backends.
set -ex

cd "$(dirname "$0")/.."

echo "Running native"
cabal test all

echo "Running ghcjs"
sh toolchain/test-ghcjs.sh

echo "Running wasm"
sh toolchain/test-wasm.sh
