#!/bin/sh
# Build the hspec-halogen executable with the host GHC and print its path.
#
#   HSPEC_HALOGEN=$(sh toolchain/build-hspec-halogen.sh)
#
# It is a native program -- cabal's test wrapper for browser suites and the
# opener for dev-test.sh -- so it is built without the wasm toolchain's CC and
# friends, which ~/.ghc-wasm/env and CI export for every later command.
set -eu

cd "$(dirname "$0")/.."
unset CC CXX LD AR NM RANLIB STRIP OBJCOPY OBJDUMP SIZE STRINGS

cabal build -v0 hspec-halogen:exe:hspec-halogen --builddir=dist-newstyle/native >&2
cabal list-bin -v0 hspec-halogen:exe:hspec-halogen --builddir=dist-newstyle/native
