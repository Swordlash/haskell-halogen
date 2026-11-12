#!/bin/sh
# Test a built WebAssembly example from Haskell, with the host GHC: a native
# suite that drives the example in Chromium through haskell-halogen-playwright.
#
#   sh toolchain/test-example.sh vanilla [cabal test args...]   # npm run test-example
#   sh toolchain/test-example.sh worker [cabal test args...]    # npm run test-worker
#
# vanilla is haskell-halogen-playwright's own example suite; worker is the
# suite in examples/worker/test. Each runs against
# dist-newstyle/wasm/public/<example> (build it with
# `sh toolchain/build-wasm.sh <example>`, or `npm run build-wasm-all`), or the
# directory HALOGEN_EXAMPLE_DIR names. Here the build is required: without it
# the suite fails rather than reporting its examples pending.
set -eu

cd "$(dirname "$0")/.."
# A native program, so built without the wasm toolchain's CC and friends,
# which CI's wasm jobs export for every later command.
unset CC CXX LD AR NM RANLIB STRIP OBJCOPY OBJDUMP SIZE STRINGS

example=${1:?usage: test-example.sh <vanilla|worker> [cabal test args...]}
shift
case "$example" in
  vanilla) suite=haskell-halogen-playwright:test:Halogen-playwright-example-test ;;
  worker) suite=halogen-example-worker:test:halogen-example-worker-test ;;
  *)
    printf 'No suite for the example %s: vanilla or worker\n' "$example" >&2
    exit 1
    ;;
esac

HALOGEN_EXAMPLE_REQUIRED=1
export HALOGEN_EXAMPLE_REQUIRED
# The suite runs in its package's directory, so the directory is given to it
# absolute.
case "${HALOGEN_EXAMPLE_DIR:-}" in
  "") HALOGEN_EXAMPLE_DIR=$(pwd)/dist-newstyle/wasm/public/$example ;;
  /*) ;;
  *) HALOGEN_EXAMPLE_DIR=$(pwd)/$HALOGEN_EXAMPLE_DIR ;;
esac
export HALOGEN_EXAMPLE_DIR

exec cabal test "$suite" --builddir=dist-newstyle/native --test-show-details=direct "$@"
