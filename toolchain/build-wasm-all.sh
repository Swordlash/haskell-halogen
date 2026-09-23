#!/bin/sh
# Build every example in examples/, each into dist-newstyle/wasm/public/<name>/.
#
# The GitHub Pages deploy uploads only public/all: the gallery that mounts every
# other example in one page, from one binary. The rest are built so that each
# still links on its own, and so the size report below covers them all.
set -eu

cd "$(dirname "$0")/.."

public_dir=dist-newstyle/wasm/public

# Start clean so a renamed or removed example cannot leave stale files behind
# in what the Pages deploy uploads.
rm -rf "$public_dir"
mkdir -p "$public_dir"

examples=$(find examples -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort)

for example in $examples; do
  printf '\n=== %s ===\n' "$example"
  sh toolchain/build-wasm.sh "$example"
done

# GitHub Pages compresses on the fly, so only report the sizes here rather
# than writing precompressed copies into the upload.
node toolchain/compress.mjs --no-write \
  $(find "$public_dir" -mindepth 2 -type f \( -name '*.wasm' -o -name '*.js' -o -name '*.css' \) | sort)

printf '\nAll examples built in %s; the gallery is in %s/all.\n' "$public_dir" "$public_dir"
