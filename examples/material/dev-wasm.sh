#!/bin/sh
# Browser-GHCi dev loop for the material example, with ghciwatch reloads.
set -eu

cd "$(dirname "$0")/../.."
. "$HOME/.ghc-wasm/env"
export PATH="$HOME/.local/bin:$PATH"

self=examples/material/dev-wasm.sh

case "${PORT:-8080}" in
  ''|*[!0-9]*) printf 'PORT must be a number.\n' >&2; exit 1 ;;
esac

case "${1:-}" in
  --open-browser)
    browser_url="http://127.0.0.1:${PORT:-8080}/assets/index.html"
    attempts=0
    until curl --fail --silent --max-time 1 --output /dev/null "$browser_url"; do
      attempts=$((attempts + 1))
      if [ "$attempts" -ge 300 ]; then
        printf 'Browser server is not ready; open %s when GHCi prints its URL.\n' "$browser_url" >&2
        exit 1
      fi
      sleep 1
    done
    exec sh toolchain/open-browser.sh "$browser_url"
    ;;
  --repl)
    exec cabal repl --project-file=cabal-wasm.project \
      --with-compiler="$(command -v wasm32-wasi-ghc)" \
      --with-hc-pkg="$(command -v wasm32-wasi-ghc-pkg)" \
      --with-hsc2hs="$(command -v wasm32-wasi-hsc2hs)" \
      --builddir=dist-newstyle/wasm-dev --disable-multi-repl --enable-shared \
      -finteractive exe:halogen-example-material \
      --repl-options="-fghci-browser -fghci-browser-port=${PORT:-8080} -fghci-browser-assets-dir=dist-newstyle/wasm-dev/public"
    ;;
  '') ;;
  *) printf 'Usage: %s [--repl|--open-browser]\n' "$self" >&2; exit 1 ;;
esac

if ! command -v ghciwatch >/dev/null 2>&1; then
  printf 'Install ghciwatch: https://mercurytechnologies.github.io/ghciwatch/\nFor manual reloads, run: npm run dev-wasm -- --repl\n' >&2
  exit 1
fi

mkdir -p dist-newstyle/wasm-dev/public
WASM_PUBLIC_DIR=dist-newstyle/wasm-dev/public \
  npx webpack-cli --config examples/material/webpack.config.js
cp examples/material/web/index-ghci.html dist-newstyle/wasm-dev/public/index.html

exec ghciwatch \
  --command "sh $self --repl" \
  --before-startup-shell "async:sh $self --open-browser" \
  --watch material/src --watch examples/material \
  --watch material/haskell-halogen-material.cabal --watch cabal-wasm.project \
  --restart-glob cabal-wasm.project \
  --after-startup-ghci ':main' --after-reload-ghci ':main' --debounce 100ms --poll 500ms
