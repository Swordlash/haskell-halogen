#!/bin/sh
# Run the test suites with the GHC JavaScript backend.
#
# Shares dist-newstyle/javascript with the webpack build (haskell-loader.mjs
# uses the same build directory), so this reuses those artifacts rather than
# compiling everything a second time.
set -eu

cd "$(dirname "$0")/.."

exec cabal test all \
  --project-file=cabal-ghcjs.project \
  --builddir=dist-newstyle/javascript \
  "$@"
