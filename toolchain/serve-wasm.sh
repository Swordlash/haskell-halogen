#!/bin/sh
# Build one example and serve it on http://127.0.0.1:$PORT (default 8080).
# Set NO_OPEN=1 to skip opening a browser.
#
#   sh toolchain/serve-wasm.sh pixi
set -eu

cd "$(dirname "$0")/.."

example=${1:?usage: serve-wasm.sh <example> [cabal args...]}
shift

sh toolchain/build-wasm.sh "$example" "$@"

server_port=${PORT:-8080}
server_url="http://127.0.0.1:${server_port}"

if [ "${NO_OPEN:-0}" != 1 ]; then
  (sleep 1; sh toolchain/open-browser.sh "$server_url") &
fi

cd "dist-newstyle/wasm/public/$example"
exec npx http-server . -p "$server_port"
