#!/bin/sh
# Bundle the Material Components JavaScript and CSS that the wasm build of this
# example loads next to app.wasm: material.js from the material package's
# jsbits (which publishes its functions on globalThis for the JSFFI) and
# material.css from style.scss.
#
#   sh examples/material/bundle.sh <output-dir>
#
# toolchain/build-wasm.sh runs this for the deployed build, and dev-wasm.sh for
# the browser-GHCi page.
set -eu

cd "$(dirname "$0")/../.."

out_dir=${1:?usage: bundle.sh <output-dir>}
mkdir -p "$out_dir"

npx esbuild material/jsbits/material.js \
  --bundle --minify --log-level=warning \
  --outfile="$out_dir/material.js"

# --quiet-deps hides the deprecation warnings raised inside @material's own
# stylesheets, which are not ours to fix.
npx sass --load-path=node_modules --style=compressed --no-source-map --quiet-deps \
  examples/material/style.scss "$out_dir/material.css"
