#!/bin/bash
# Build one example with the GHC JavaScript backend and serve it.
#
#   sh toolchain/serve-ghcjs.sh vanilla
set -ex

cd "$(dirname "$0")/.."
. toolchain/ghcjs-env.sh

example=${1:-vanilla}
package=${PACKAGE:-halogen-example-$example}

sh run_fourmolu.sh

cabal build -fforce-recomp \
  --project-file=cabal-ghcjs.project \
  --with-compiler="$ghcjs_ghc" \
  --with-hc-pkg="$ghcjs_ghc_pkg" \
  "exe:$package"

all_js="$(find dist-newstyle/build/javascript-ghcjs -type f -path "*/$package.jsexe/all.js" -print -quit)"
test -n "$all_js"

public_dir="dist-newstyle/ghcjs/public/$example"
mkdir -p "$public_dir"
cp -R "examples/$example/web/." "$public_dir/"
cp "$all_js" "$public_dir/index.js"

server_port=${PORT:-8080}
server_url="http://127.0.0.1:${server_port}"

if [ "${NO_OPEN:-0}" != 1 ]; then
  (sleep 1; sh toolchain/open-browser.sh "$server_url") &
fi

exec npx http-server "$public_dir" -p "$server_port"
