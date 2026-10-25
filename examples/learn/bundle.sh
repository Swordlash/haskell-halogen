#!/bin/sh
# Bundle the tutorial's JavaScript into the learn.js its page loads before
# app.wasm: the compiler's loader and the CodeMirror editor
# (examples/learn/js/), with CodeMirror's packages from node_modules. The
# editor's styles come with it, as CodeMirror injects them; the page's own
# are web/learn.css, copied as it is.
#
#   sh examples/learn/bundle.sh <output-dir>
#
# toolchain/build-wasm.sh runs this for the deployed build, and toolchain/dev.sh
# for the browser-GHCi page.
#
# The compiler itself (the rootfs, from toolchain/build-learn-rootfs.sh) is
# not part of the build: the page looks for it in rootfs/ under its base.
set -eu

cd "$(dirname "$0")/../.."

out_dir=${1:?usage: bundle.sh <output-dir>}
mkdir -p "$out_dir"

# The loader imports browser_wasi_shim and the rootfs' dyld.mjs at run time,
# from addresses only known then, so esbuild leaves those imports alone.
npx esbuild examples/learn/js/learn.js \
  --bundle --minify --format=esm --target=es2022 --log-level=warning \
  --outfile="$out_dir/learn.js"
