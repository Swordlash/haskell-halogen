#!/bin/sh
# Run the test suites with the GHC JavaScript backend.
#
# Shares dist-newstyle/javascript with toolchain/build-ghcjs.sh, so this reuses
# those artifacts rather than compiling everything a second time.
set -eu

cd "$(dirname "$0")/.."

# ghcjs-test-wrapper.sh hands browser suites to the hspec-halogen executable,
# a native program.
HSPEC_HALOGEN=${HSPEC_HALOGEN:-$(sh toolchain/build-hspec-halogen.sh)}
export HSPEC_HALOGEN

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
