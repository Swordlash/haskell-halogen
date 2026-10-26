#!/bin/sh
# Put the WASI shim beside the page as wasi.js, one module bundled from
# node_modules' @bjorn3/browser_wasi_shim: the page's index.js imports it,
# and so does every worker Halogen.Worker starts, from the address the page
# gives it (globalThis.halogenWorker.wasi). Served from the page's own site,
# the example loads nothing from anywhere else, which toolchain/test-worker.mjs
# checks.
#
#   sh examples/worker/bundle.sh <output-dir>
set -eu

cd "$(dirname "$0")/../.."

out_dir=${1:?usage: bundle.sh <output-dir>}
mkdir -p "$out_dir"

npx esbuild node_modules/@bjorn3/browser_wasi_shim/dist/index.js \
  --bundle --minify --format=esm --target=es2022 --log-level=warning \
  --outfile="$out_dir/wasi.js"
