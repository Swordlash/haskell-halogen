#!/bin/sh
# Three.js for the browser test runner, bundled from node_modules into one ES
# module (three.module.js imports three.core.js), served as three.mjs: the
# runner loads every .js file here as a classic script, and an .mjs it only
# serves, for the renderer to import (Test.Page.moduleUrl).
set -eu
root=$(cd "$(dirname "$0")/../../.." && pwd)
exec "$root/node_modules/.bin/esbuild" "$root/node_modules/three/build/three.module.js" \
  --bundle --format=esm --minify --log-level=warning --outfile="$1/three.mjs"
