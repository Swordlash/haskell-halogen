#!/bin/sh
# Test a built WebAssembly example from Haskell, with the host GHC: a native
# suite that drives the example in Chromium through haskell-halogen-playwright.
#
#   sh toolchain/test-example.sh vanilla [cabal test args...]     # npm run test-example
#   sh toolchain/test-example.sh worker [cabal test args...]      # npm run test-worker
#   sh toolchain/test-example.sh playground [cabal test args...]  # npm run test-playground
#   sh toolchain/test-example.sh learn [cabal test args...]       # npm run test-learn
#
# vanilla is haskell-halogen-playwright's own example suite; worker is the
# suite in examples/worker/test. Each runs against
# dist-newstyle/wasm/public/<example> (build it with
# `sh toolchain/build-wasm.sh <example>`, or `npm run build-wasm-all`), or the
# directory HALOGEN_EXAMPLE_DIR names. playground is the suite in
# examples/learn/playground-test, which drives learn/'s page on the rootfs:
# dist-newstyle/wasm/learn (build it with `npm run build-rootfs`), or the
# directory LEARN_ROOTFS_DIR names. learn is the suite in
# examples/learn/browser-test, which drives the tutorial as Pages deploys it:
# dist-newstyle/wasm/public/learn with the rootfs in it (build the rootfs,
# then `sh toolchain/build-wasm.sh learn`), or the directory
# HALOGEN_EXAMPLE_DIR names, served under HALOGEN_BASE_PATH (the base path
# the pages were built for), with the rootfs LEARN_ROOTFS_DIR names in place
# of the tree's own when it is set. Here the build is required: without it
# the suite fails rather than reporting its examples pending.
set -eu

cd "$(dirname "$0")/.."
# A native program, so built without the wasm toolchain's CC and friends,
# which CI's wasm jobs export for every later command.
unset CC CXX LD AR NM RANLIB STRIP OBJCOPY OBJDUMP SIZE STRINGS

example=${1:?usage: test-example.sh <vanilla|worker|playground|learn> [cabal test args...]}
shift
case "$example" in
  vanilla) suite=haskell-halogen-playwright:test:Halogen-playwright-example-test ;;
  worker) suite=halogen-example-worker:test:halogen-example-worker-test ;;
  playground) suite=halogen-example-learn:test:halogen-example-learn-playground-test ;;
  learn) suite=halogen-example-learn:test:halogen-example-learn-browser-test ;;
  *)
    printf 'No suite for the example %s: vanilla, worker, playground or learn\n' "$example" >&2
    exit 1
    ;;
esac

HALOGEN_EXAMPLE_REQUIRED=1
export HALOGEN_EXAMPLE_REQUIRED
# The suite runs in its package's directory, so the directory is given to it
# absolute.
absolute() {
  case "$1" in
    /*) printf '%s' "$1" ;;
    *) printf '%s/%s' "$(pwd)" "$1" ;;
  esac
}
if [ "$example" = playground ]; then
  LEARN_ROOTFS_DIR=$(absolute "${LEARN_ROOTFS_DIR:-dist-newstyle/wasm/learn}")
  export LEARN_ROOTFS_DIR
else
  # The tutorial's own rootfs is the one in its tree unless another is named.
  if [ -n "${LEARN_ROOTFS_DIR:-}" ]; then
    LEARN_ROOTFS_DIR=$(absolute "$LEARN_ROOTFS_DIR")
    export LEARN_ROOTFS_DIR
  fi
  HALOGEN_EXAMPLE_DIR=$(absolute "${HALOGEN_EXAMPLE_DIR:-dist-newstyle/wasm/public/$example}")
  export HALOGEN_EXAMPLE_DIR
fi

exec cabal test "$suite" --builddir=dist-newstyle/native --test-show-details=direct "$@"
