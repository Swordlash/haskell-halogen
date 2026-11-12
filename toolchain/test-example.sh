#!/bin/sh
# Test a built WebAssembly example from Haskell: haskell-halogen-playwright's
# example suite, built with the host GHC, against
# dist-newstyle/wasm/public/vanilla (build it with
# `sh toolchain/build-wasm.sh vanilla`, or `npm run build-wasm-all`), or the
# directory HALOGEN_EXAMPLE_DIR names. Here the build is required: without it
# the suite fails rather than reporting its examples pending.
#
#   npm run test-example [-- cabal test args...]
set -eu

cd "$(dirname "$0")/.."
# A native program, so built without the wasm toolchain's CC and friends,
# which CI's wasm jobs export for every later command.
unset CC CXX LD AR NM RANLIB STRIP OBJCOPY OBJDUMP SIZE STRINGS

HALOGEN_EXAMPLE_REQUIRED=1
export HALOGEN_EXAMPLE_REQUIRED
# The suite runs in playwright/, so a relative directory is made absolute.
case "${HALOGEN_EXAMPLE_DIR:-}" in
  "" | /*) ;;
  *) HALOGEN_EXAMPLE_DIR=$(pwd)/$HALOGEN_EXAMPLE_DIR; export HALOGEN_EXAMPLE_DIR ;;
esac

exec cabal test haskell-halogen-playwright:test:Halogen-playwright-example-test \
  --builddir=dist-newstyle/native --test-show-details=direct "$@"
