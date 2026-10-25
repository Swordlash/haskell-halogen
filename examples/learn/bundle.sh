#!/bin/sh
# Bundle the tutorial's JavaScript into the learn.js its page loads before
# app.wasm: the compiler's loader and the CodeMirror editor
# (examples/learn/js/), with CodeMirror's packages from node_modules. The
# editor's styles come with it, as CodeMirror injects them; the page's own
# are web/learn.css, copied as it is. Then put the compiler beside it.
#
#   sh examples/learn/bundle.sh <output-dir>
#
# toolchain/build-wasm.sh runs this for the deployed build, and toolchain/dev.sh
# for the browser-GHCi page.
#
# The compiler is the rootfs toolchain/build-learn-rootfs.sh builds (npm run
# build-rootfs), which this does not build: it copies the directory that
# script wrote, LEARN_ROOTFS_DIR (default dist-newstyle/wasm/learn), to
# rootfs/ in the output, where the page looks for it, without learn/'s own
# page (its index.html and index.js). A copy rather than a link, because the
# output directory is what Pages deploys. Without a rootfs it says so and
# goes on: the page then says the compiler did not load, and everything else
# works.
set -eu

cd "$(dirname "$0")/../.."

out_dir=${1:?usage: bundle.sh <output-dir>}
rootfs=${LEARN_ROOTFS_DIR:-dist-newstyle/wasm/learn}
mkdir -p "$out_dir"

# The loader imports browser_wasi_shim and the rootfs' dyld.mjs at run time,
# from addresses only known then, so esbuild leaves those imports alone.
npx esbuild examples/learn/js/learn.js \
  --bundle --minify --format=esm --target=es2022 --log-level=warning \
  --outfile="$out_dir/learn.js"

rm -rf "$out_dir/rootfs"
if [ -f "$rootfs/rootfs.json" ]; then
  mkdir "$out_dir/rootfs"
  for file in "$rootfs"/*; do
    case ${file##*/} in
      index.html | index.js) ;;
      *) cp -R "$file" "$out_dir/rootfs/" ;;
    esac
  done
  printf 'Copied the rootfs (version %s) from %s to %s/rootfs.\n' \
    "$(sed -n 's/^  "version": "\(.*\)",$/\1/p' "$rootfs/rootfs.json")" "$rootfs" "$out_dir"
else
  printf 'No rootfs in %s (npm run build-rootfs builds it): the page will say the compiler did not load.\n' "$rootfs" >&2
fi
