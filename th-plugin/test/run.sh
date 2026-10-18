#!/usr/bin/env bash
# The plugin on a compiler: a module with every kind of broken quote has
# each error at its place, and a module with good quotes compiles. Run from
# anywhere after `cabal build all`; CABAL_BUILDDIR names cabal's build
# directory when it is not the default.
set -euo pipefail
cd "$(dirname "$0")/../.."

ghc() {
  cabal exec ${CABAL_BUILDDIR:+--builddir="$CABAL_BUILDDIR"} -v0 -- ghc -fno-code -fplugin=Halogen.TH.Plugin \
    -package haskell-halogen-th-plugin -package haskell-halogen-th -package haskell-halogen-core -package clay -package text "$@"
}

out=$(ghc th-plugin/test/Bad.hs 2>&1 || true)
for want in \
  "Bad.hs:13:30: error:" "hh: parse error on input \`)'" \
  "Bad.hs:18:34: error:" "clay: parse error on input \`)'" \
  "Bad.hs:21:25: error:" "hhs: parse error (possibly incorrect indentation or mismatched brackets)" \
  "Bad.hs:25:7: error:" \
  "Bad.hs:28:30: error:" "hh: lexical error at character 'q'" \
  "Bad.hs:31:12: error:" "hh: {{ is never closed by }}"; do
  if ! grep -qF -- "$want" <<<"$out"; then
    echo "missing in the output: $want" >&2
    echo "$out" >&2
    exit 1
  fi
done
echo "Bad.hs: six errors, each at its place"

if ! ghc th-plugin/test/Good.hs; then
  echo "Good.hs does not compile with the plugin" >&2
  exit 1
fi
echo "Good.hs: compiles"
