#!/bin/sh
set -eu

cd "$(dirname "$0")"
./wasm-pixi.sh "$@"

server_port=${PORT:-8080}
server_url="http://127.0.0.1:${server_port}"

if [ "${NO_OPEN:-0}" != 1 ]; then
  (sleep 1; sh toolchain/open-browser.sh "$server_url") &
fi

cd dist-newstyle/wasm-pixi-component/public
exec npx http-server . -p "$server_port"
