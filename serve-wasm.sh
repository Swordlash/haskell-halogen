#!/bin/sh
set -eu

cd "$(dirname "$0")"
./wasm.sh "$@"
cd dist-newstyle/wasm/public
exec npx http-server . -p "${PORT:-8080}"
