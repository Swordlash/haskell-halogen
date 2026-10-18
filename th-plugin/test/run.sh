#!/usr/bin/env bash
# The plugin on a compiler: a module with every kind of broken quote has
# each error at its place, a module with good quotes compiles, and a module
# whose quotes do not type check has each type error at its hole. Run from
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
  "Bad.hs:13:30: error:" \
  "Bad.hs:18:34: error:" \
  "Bad.hs:21:25: error:" \
  "Bad.hs:25:7: error:" \
  "Bad.hs:28:30: error:" "lexical error at character 'q'" \
  "Bad.hs:31:12: error:" "hh: {{ is never closed by }}"; do
  if ! grep -qF -- "$want" <<<"$out"; then
    echo "missing in the output: $want" >&2
    echo "$out" >&2
    exit 1
  fi
done
if [ "$(grep -c 'parse error' <<<"$out")" != 4 ]; then
  echo "four parse errors expected in Bad.hs" >&2
  echo "$out" >&2
  exit 1
fi
echo "Bad.hs: six errors, each at its place"

if ! ghc th-plugin/test/Good.hs; then
  echo "Good.hs does not compile with the plugin" >&2
  exit 1
fi
echo "Good.hs: compiles"

out=$(ghc th-plugin/test/Typed.hs 2>&1 || true)
for want in \
  "Typed.hs:14:24: error:" "In the first argument of" \
  "Typed.hs:19:23: error:" "In the pattern: (x, y)"; do
  if ! grep -qF -- "$want" <<<"$out"; then
    echo "missing in the output: $want" >&2
    echo "$out" >&2
    exit 1
  fi
done
echo "Typed.hs: the type errors are at their holes"

out=$(ghc th-plugin/test/Instance.hs 2>&1 || true)
if ! grep -qF -- "Instance.hs:12:28: error:" <<<"$out" || ! grep -qF -- "No instance for" <<<"$out"; then
  echo "the missing instance is not reported at the hole" >&2
  echo "$out" >&2
  exit 1
fi
echo "Instance.hs: the missing instance is at its hole"
