#!/bin/sh
# Build every package with the GHC JavaScript backend, then bundle the material
# example into dist/ as a production build: minified main.js and main.css, their
# brotli-compressed copies, and an index.html that loads them.
#
#   sh toolchain/build-ghcjs.sh [--dev] [cabal args...]
#
# --dev skips minification and compression, for a quicker look in a browser.
set -eu

cd "$(dirname "$0")/.."
. toolchain/ghcjs-env.sh

dev=0
if [ "${1:-}" = --dev ]; then
  dev=1
  shift
fi

build_dir=dist-newstyle/javascript
out_dir=dist

# Cabal may reuse the host ghc-pkg from this cache when switching toolchains.
rm -f "$build_dir/cache/compiler"

set -- \
  --project-file=cabal-ghcjs.project \
  --builddir="$build_dir" \
  --with-compiler="$ghcjs_ghc" \
  --with-hc-pkg="$ghcjs_ghc_pkg" \
  "$@"

# Everything, not just the example, so CI type-checks and links every package
# and example against the JavaScript backend.
cabal build all "$@"
all_js="$(cabal list-bin "$@" exe:halogen-example-material).jsexe/all.js"

rm -rf "$out_dir"
mkdir -p "$out_dir"

if [ "$dev" = 1 ]; then
  minify=
else
  minify=--minify
fi

# The RTS require()s these Node modules only behind h$isNode() checks, so they
# are left as externals rather than resolved. The one direct eval() in the RTS
# looks up FFI imports by name in a debugging check; esbuild warns about it
# because minification renames those names, but it is not on the path a
# browser takes.
npx esbuild "$all_js" \
  --bundle $minify --log-level=warning \
  --log-override:direct-eval=silent \
  --external:os --external:fs --external:child_process --external:path \
  --external:ghcjs-profiling \
  --outfile="$out_dir/main.js"

npx sass --load-path=node_modules --style=compressed --no-source-map --quiet-deps \
  examples/material/style.scss "$out_dir/main.css"

cat > "$out_dir/index.html" <<'EOF'
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Halogen Material Components</title>
    <link rel="stylesheet" href="./main.css">
    <script defer src="./main.js"></script>
  </head>
  <body></body>
</html>
EOF

if [ "$dev" = 0 ]; then
  node toolchain/compress.mjs "$out_dir/main.js" "$out_dir/main.css"
fi

printf '\nBuilt the material example in %s.\n' "$out_dir"
