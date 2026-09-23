#!/bin/sh
# Run the test suites with the GHC JavaScript backend.
#
# Shares dist-newstyle/javascript with the webpack build (haskell-loader.mjs
# uses the same build directory), so this reuses those artifacts rather than
# compiling everything a second time.
set -eu

cd "$(dirname "$0")/.."
. toolchain/ghcjs-env.sh

build_dir=dist-newstyle/javascript

# Cabal may reuse the host ghc-pkg from this cache when switching toolchains.
rm -f "$build_dir/cache/compiler"

exec cabal test all \
  --project-file=cabal-ghcjs.project \
  --builddir="$build_dir" \
  --with-compiler="$ghcjs_ghc" \
  --with-hc-pkg="$ghcjs_ghc_pkg" \
  --test-wrapper="$(pwd)/toolchain/ghcjs-test-wrapper.sh" \
  "$@"
