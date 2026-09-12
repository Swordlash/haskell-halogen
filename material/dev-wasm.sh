#!/bin/sh
set -eu

cd "$(dirname "$0")"
. "$HOME/.ghc-wasm/env"
export PATH="$HOME/.local/bin:$PATH"

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
    if command -v open >/dev/null 2>&1; then
      exec open "$browser_url"
    elif command -v xdg-open >/dev/null 2>&1; then
      exec xdg-open "$browser_url"
    fi
    printf 'Open %s in a browser.\n' "$browser_url"
    ;;
  --repl)
    exec cabal repl --project-file=cabal-wasm.project \
      --with-compiler="$(command -v wasm32-wasi-ghc)" \
      --with-hc-pkg="$(command -v wasm32-wasi-ghc-pkg)" \
      --with-hsc2hs="$(command -v wasm32-wasi-hsc2hs)" \
      --builddir=dist-newstyle/wasm-dev --disable-multi-repl --enable-shared \
      -finteractive exe:halogen-material-app \
      --repl-options="-fghci-browser -fghci-browser-port=${PORT:-8080} -fghci-browser-assets-dir=dist-newstyle/wasm-dev/public"
    ;;
  '') ;;
  *) printf 'Usage: %s [--repl|--open-browser]\n' "$0" >&2; exit 1 ;;
esac

if ! command -v ghciwatch >/dev/null 2>&1; then
  printf 'Install ghciwatch: https://mercurytechnologies.github.io/ghciwatch/\nFor manual reloads, run: npm run dev-wasm -- --repl\n' >&2
  exit 1
fi

mkdir -p dist-newstyle/wasm-dev/public
WASM_BUILD_DIR=dist-newstyle/wasm-dev npx webpack-cli --config webpack.config-wasm.js
cp dev/wasm-ghci.html dist-newstyle/wasm-dev/public/index.html

exec ghciwatch \
  --command 'sh ./dev-wasm.sh --repl' \
  --before-startup-shell 'async:sh ./dev-wasm.sh --open-browser' \
  --watch src --watch app --watch halogen-material.cabal --watch cabal-wasm.project \
  --restart-glob cabal-wasm.project \
  --after-startup-ghci ':main' --after-reload-ghci ':main' --debounce 100ms --poll 500ms
