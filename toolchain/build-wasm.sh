#!/bin/sh
# Build one example in examples/ to a browser-ready wasm bundle.
#
#   sh toolchain/build-wasm.sh pixi [extra cabal args...]
#
# Output lands in dist-newstyle/wasm/public/<example>/, containing app.wasm
# (size-optimised with wasm-opt), ghc_wasm_jsffi.js and everything under
# examples/<example>/web/.
#
# An example whose cabal file has an executable named <package>-prerender
# gets its index.html written by it: the executable is built with the host
# GHC (into dist-newstyle/native) and run as
#
#   <package>-prerender examples/<example>/web/index.html <output dir>
#
# to render the example natively into the page (see Halogen.Static), so it
# shows before app.wasm has loaded. HALOGEN_BASE_PATH, the path the site is
# served under (default /), is passed on to it in the environment.
set -eu

cd "$(dirname "$0")/.."
. "$HOME/.ghc-wasm/env"

example=${1:?usage: build-wasm.sh <example> [cabal args...]}
shift

if [ ! -d "examples/$example" ]; then
  printf 'No such example: examples/%s\n' "$example" >&2
  exit 1
fi

package=${PACKAGE:-halogen-example-$example}
build_dir=dist-newstyle/wasm
public_dir=$build_dir/public/$example

wasm_ghc=$(command -v wasm32-wasi-ghc)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg)
wasm_hsc2hs=$(command -v wasm32-wasi-hsc2hs)

# Cabal may reuse the host ghc-pkg from this cache when switching toolchains.
rm -f "$build_dir/cache/compiler"

set -- \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  "exe:$package" "$@"

cabal build "$@"
wasm_binary=$(cabal list-bin "$@")
wasm_libdir=$($wasm_ghc --print-libdir)

mkdir -p "$public_dir"
node "$wasm_libdir/post-link.mjs" --input "$wasm_binary" --output "$public_dir/ghc_wasm_jsffi.js"
# The linker output carries a name section and debug info worth about a third
# of the file; stripping those and optimising for size roughly halves the
# binary and takes about a fifth off what is served gzipped. wasm-opt comes with
# ghc-wasm-meta's binaryen and leaves the imports and exports the JSFFI glue
# binds to untouched.
wasm-opt -Oz --strip-debug --strip-producers "$wasm_binary" -o "$public_dir/app.wasm"
cp -R "examples/$example/web/." "$public_dir/"

if [ -f "examples/$example/bundle.sh" ]; then
  sh "examples/$example/bundle.sh" "$public_dir"
fi

prerender=$package-prerender
if grep -Eqs "^executable[[:space:]]+$prerender[[:space:]]*\$" "examples/$example"/*.cabal; then
  # A native program, so built without the wasm toolchain's CC and friends,
  # which ~/.ghc-wasm/env exported above (as toolchain/build-hspec-halogen.sh
  # does).
  prerender_bin=$(
    unset CC CXX LD AR NM RANLIB STRIP OBJCOPY OBJDUMP SIZE STRINGS
    cabal build -v0 "$package:exe:$prerender" --builddir=dist-newstyle/native >&2
    cabal list-bin -v0 "$package:exe:$prerender" --builddir=dist-newstyle/native
  )
  "$prerender_bin" "examples/$example/web/index.html" "$public_dir"
  printf 'Prerendered %s with %s.\n' "$public_dir/index.html" "$prerender"
fi

printf '\nBuilt %s in %s.\n' "$package" "$public_dir"
