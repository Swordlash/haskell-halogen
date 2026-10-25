#!/bin/sh
# Build every example in examples/, each into dist-newstyle/wasm/public/<name>/.
#
#   sh toolchain/build-wasm-all.sh [cabal args...]   # npm run build-wasm-all [-- -j2]
#
# The GitHub Pages deploy uploads only public/learn: the tutorial, with the
# compiler it runs in the page (the rootfs) in public/learn/rootfs/, and a
# page per route written for HALOGEN_BASE_PATH (the path the site is served
# under; the deploy sets it). The other examples are built so that each still
# links and prerenders on its own, which nothing else checks for wasm (the test
# suites build only themselves), and so the size report below covers them all;
# each is served on its own with npm run serve-wasm.
#
# The rootfs is toolchain/build-learn-rootfs.sh's output directory, which
# examples/learn/bundle.sh copies in. When there is none, it is built first;
# one already there is used as it is, so rebuild it (npm run build-rootfs)
# after changing a library a lesson uses, or the GHC pin. CI keys its cache of
# that directory by everything it is made from (build-learn-rootfs.sh --key).
set -eu

cd "$(dirname "$0")/.."

public_dir=dist-newstyle/wasm/public
rootfs_dir=${LEARN_ROOTFS_DIR:-dist-newstyle/wasm/learn}

# Start clean so a renamed or removed example cannot leave stale files behind
# in what the Pages deploy uploads.
rm -rf "$public_dir"
mkdir -p "$public_dir"

if [ ! -f "$rootfs_dir/rootfs.json" ]; then
  printf '\n=== the rootfs ===\n'
  sh toolchain/build-learn-rootfs.sh "$rootfs_dir" "$@"
fi

examples=$(find examples -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort)

for example in $examples; do
  printf '\n=== %s ===\n' "$example"
  sh toolchain/build-wasm.sh "$example" "$@"
done

if [ ! -f "$public_dir/learn/rootfs/rootfs.json" ]; then
  printf '%s/learn has no rootfs/rootfs.json: the tutorial would deploy without its compiler.\n' "$public_dir" >&2
  exit 1
fi

# GitHub Pages compresses on the fly, so only report the sizes here rather
# than writing precompressed copies into the upload. The rootfs' archives are
# compressed already.
node toolchain/compress.mjs --no-write \
  $(find "$public_dir" -mindepth 2 -path "$public_dir/learn/rootfs" -prune -o -type f \( -name '*.wasm' -o -name '*.js' -o -name '*.css' \) -print | sort)

printf '\nAll examples built in %s; Pages deploys %s/learn (%s).\n' \
  "$public_dir" "$public_dir" "$(du -sh "$public_dir/learn" | cut -f1)"
