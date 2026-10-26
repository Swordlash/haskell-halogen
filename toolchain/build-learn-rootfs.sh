#!/bin/sh
# Build the in-browser playground in learn/: GHC itself, compiled to wasm as a
# shared library, with the boot libraries and haskell-halogen-core in an
# archive the page unpacks into its in-memory file system.
#
#   sh toolchain/build-learn-rootfs.sh [output dir] [cabal args...]   # npm run build-rootfs
#   sh toolchain/build-learn-rootfs.sh --key [cabal args...]          # what the build is made from
#
# The output directory (default dist-newstyle/wasm/learn) is a static site:
# serve it as it is (npm run test-playground does, in headless Chromium). The
# tutorial (examples/learn) is served with it, but for its index.html and
# index.js, at rootfs/ beside the app (examples/learn/bundle.sh copies it
# there). It holds
#
#   rootfs.tar.zst   everything the compiler reads, extracted to /tmp:
#                      /tmp/libplayground.so  the GHC API wrapper (learn/Playground.hs)
#                      /tmp/clib              the wasi-sysroot's C and C++ .so
#                      /tmp/hslib/lib         GHC's libdir: settings, the global
#                                             package DB, every boot library's .so
#                                             and .dyn_hi, and the base layer's
#   layers/<name>.tar.zst   further layers (LEARN_LAYERS), each with its
#                    package DB, and layers/<name>/ the files a layer's
#                    packages need on the page (material's CSS and JavaScript)
#                    or fetch while a program runs (lazy's demo library,
#                    liblazy-demo.so)
#   rootfs.json      the manifest: what the page hands to dyld and GHC (the
#                    paths in the archive), every archive's parts, sizes,
#                    package DB and assets, and a version, a hash of the
#                    archives (see learn/README.md for each field)
#   dyld.mjs, post-link.mjs, prelude.mjs   GHC's wasm dynamic linker, from the libdir
#   browser_wasi_shim.js   the WASI shim dyld and the page share, from
#                    learn/vendor/browser_wasi_shim/
#   bsdtar.wasm      libarchive's bsdtar, which the page extracts the archives with
#   index.html, index.js                   learn/web/
#
# An archive larger than LEARN_PART_MB is served as parts (<archive>.00,
# .01, ...), which the page fetches and joins before extracting, so that no
# file is over Pages' limit. Packing is reproducible (the members sorted by
# name, with no times or owners in them), so that packing the same built
# libraries again gives the same version, and a reader's cached copy stays
# valid. Compiling a library again may not give the same bytes (GHC's output
# is not deterministic by default), which changes the version; CI keeps the
# output directory in its cache (--key below), so it packs anew only when an
# input changed.
#
# --key prints a hash of everything the output is made from, for CI to key
# its cache of the output directory by: the wasm compiler, the units of the
# build plan in the closure of the packages packed (a store unit by its id,
# which hashes its own inputs; one of ours by its files), the wasi-sysroot's
# libraries, the files this script and the page read from the repository,
# and the settings below. It plans the wasm build (cabal build --dry-run),
# which writes dist-newstyle/wasm/cache/plan.json, and builds nothing.
#
# The base layer is haskell-halogen-jsbits, haskell-halogen-core and every
# library they need that GHC does not ship (BASE_LAYER below), taken from the
# wasm build of this project: our packages from dist-newstyle/wasm, the rest
# from cabal's store. Their .so go into the libdir's one library directory,
# beside the boot libraries', their interfaces into a directory per unit
# there, and they are registered in the global package DB, so a program needs
# only -package haskell-halogen-core.
#
# A further layer is an archive of its own, extracted on top of the base one,
# with a package DB of its own (/tmp/hslib/layers/<name>/package.conf.d, its
# package.cache built here), which the page passes to GHC with -package-db: it
# holds what the layer's packages need that the base layer does not. A layer
# stands on the base layer alone, never on another layer, so that a page can
# load any of them, in any order; a library two layers need belongs in the
# base layer, and the build stops if two layers would both carry one. See
# learn/README.md for why a DB of its own rather than the global one.
#
# The boot libraries are trimmed to what a reader's program and GHC itself
# use (BOOT_TRIM below), and the ghc package, whose interfaces only a program
# importing the GHC API reads, is hidden and its interfaces left out, unless
# LEARN_GHC_API=1. Its library stays: the playground is linked with it.
#
# This follows GHC's own browser playground (its testsuite's
# ghc-api-browser/playground001.sh) and try-miso's build-root.sh.
#
# Environment:
#   ZSTD_CLEVEL    zstd level for the archives (default 19, as for a release;
#                  3 packs in seconds, for trying things out)
#   LEARN_LAYERS   further layers, as name=package,package... separated by
#                  spaces (default: DEFAULT_LAYERS below, every package of
#                  this project a lesson can use; empty for none), for example
#                  LEARN_LAYERS='hooks=haskell-halogen-hooks router=haskell-halogen-router'
#   LEARN_PART_MB  the largest file served, in MB (default 95, under the
#                  100 MB a file on GitHub may have); a larger archive is
#                  split into parts of this size
#   LEARN_GHC_API  when set to 1, keep the ghc package exposed, with its
#                  interfaces (32 MB, 8 MB compressed), for programs that
#                  import the GHC API
#   WASM_SO_OPT    when set, wasm-opt flags the layers' .so are run through,
#                  as GHC's own build does its libraries' (see learn/README.md
#                  for what that gains)
#   LEARN_REPORT   when set to 1, also report each part's compressed size
#                  (compresses each part again, at ZSTD_CLEVEL)
set -eu

cd "$(dirname "$0")/.."

# The toolchain is ghc-wasm-meta's; source its environment only when it is not
# on the PATH already.
if ! command -v wasm32-wasi-ghc >/dev/null 2>&1; then
  . "$HOME/.ghc-wasm/env"
fi

out=dist-newstyle/wasm/learn
key_only=
case ${1-} in
  --key)
    key_only=1
    shift
    ;;
  '' | -*) ;;
  *)
    out=$1
    shift
    ;;
esac
level=${ZSTD_CLEVEL:-19}
part_bytes=$((${LEARN_PART_MB:-95} * 1000000))
# The output directory is deleted and written afresh, so it must be one this
# script wrote (it has a rootfs.json, or the .building mark of a build that
# did not finish), or empty, or not there yet: never, say, learn/ given by
# mistake.
if [ -e "$out" ] && [ ! -f "$out/rootfs.json" ] && [ ! -f "$out/.building" ] &&
  [ -n "$(ls -A "$out" 2>/dev/null || echo file)" ]; then
  printf '%s is not a rootfs directory (it has no rootfs.json), so it is not deleted to build one there.\n' "$out" >&2
  exit 1
fi

# The roots of the base layer; the rest of it is their dependencies' closure.
BASE_LAYER="haskell-halogen-jsbits haskell-halogen-core"
# Every package of this project a lesson can use, a layer each. Each holds
# its package and what that needs beyond the base layer: hooks, router, net,
# elements (haskell-halogen-custom-element), devtools, pixi, three, store and
# worker only themselves;
# material also clock and extra; th also ghc-lib-parser (as big as GHC's own
# library) and the parsers it uses, which is why it is a layer of its own
# that only a lesson about quasi-quotes loads;
# hspec (hspec-halogen, the test harness, which runs a suite in the page with
# synthetic events where it has no Playwright) also hspec, hspec-core, HUnit,
# ansi-terminal and the rest of hspec's closure that the base layer lacks;
# lazy is haskell-halogen-lazy and haskell-halogen-lazy-demo, the types its
# demo library shares with the program loading it (the library itself,
# liblazy-demo.so, is served beside the archive rather than in it: see step 8).
# sound and tauri are not here: a lesson has no sound to play nor a Tauri to
# talk to.
DEFAULT_LAYERS="hooks=haskell-halogen-hooks router=haskell-halogen-router net=haskell-halogen-net elements=haskell-halogen-custom-element devtools=haskell-halogen-devtools material=haskell-halogen-material pixi=haskell-halogen-pixi three=haskell-halogen-three th=haskell-halogen-th hspec=hspec-halogen store=haskell-halogen-store worker=haskell-halogen-worker lazy=haskell-halogen-lazy,haskell-halogen-lazy-demo"
layers=${LEARN_LAYERS-$DEFAULT_LAYERS}
# Boot packages that neither GHC (the closure of ghc, ghci, ghc-boot,
# template-haskell, hpc and ghc-heap) nor any package above depends on, so no
# reader's program can use them either: Cabal is a sixth of the libdir, and
# the rest are haddock's, GHCi's line editor, the toolchain configurator,
# compact regions and the two compatibility shims for bignum. ghc-pkg refuses
# to unregister one that a boot package left depends on, and ghc-pkg check
# runs on the result; but the trimming comes before the base layer and the
# layers are injected, so for their packages the check is learn-inject.py's,
# which stops on a dependency on a trimmed package and names it. (ghc-platform
# looks as unused, but ghc-boot depends on it.)
BOOT_TRIM="Cabal Cabal-syntax haddock-api haddock-library xhtml haskeline ghc-toolchain ghc-compact integer-gmp ghc-bignum"

build_dir=dist-newstyle/wasm
wasm_ghc=$(command -v wasm32-wasi-ghc)
wasm_ghc_pkg=$(command -v wasm32-wasi-ghc-pkg)
wasm_hsc2hs=$(command -v wasm32-wasi-hsc2hs)
wasm_cc=$(command -v wasm32-wasi-clang)
libdir=$("$wasm_ghc" --print-libdir)

# bsdtar.wasm from https://github.com/haskell-wasm/bsdtar-wasm (libarchive's
# bsdtar with zstd, BSD-2-Clause), the build GHC's own playground loads. Pinned
# by its hash so that the page is the same from one build to the next; set
# BSDTAR_SHA256= (empty) to take whatever is there now.
bsdtar_url=https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm
bsdtar_sha256=${BSDTAR_SHA256-e13ebb15ca0971f6629a6313bc043c532dd9be3a0e6bb0b7f8a395de835ad0c0}

step() { printf '\n==> %s\n' "$*"; }
layer_name() { printf '%s' "${1%%=*}"; }
layer_packages() { printf '%s' "${1#*=}" | tr ',' ' '; }

# 1. Our packages, built as the examples are (toolchain/build-wasm.sh), with
# the shared libraries cabal-wasm.project asks for. A no-op when they are up
# to date.
packages=$BASE_LAYER
for layer in $layers; do
  packages="$packages $(layer_packages "$layer")"
done
rm -f "$build_dir/cache/compiler"
build() {
  # shellcheck disable=SC2046
  cabal build \
    --project-file=cabal-wasm.project \
    --builddir="$build_dir" \
    --with-compiler="$wasm_ghc" \
    --with-hc-pkg="$wasm_ghc_pkg" \
    --with-hsc2hs="$wasm_hsc2hs" \
    "$@" $(for p in $packages; do printf '%s:lib:%s ' "$p" "$p"; done)
}
plan=$build_dir/cache/plan.json

if [ -n "$key_only" ]; then
  build --dry-run "$@" >&2
  exec python3 -I toolchain/learn-rootfs-key.py \
    --plan "$plan" --packages "$packages" \
    --setting "ZSTD_CLEVEL=$level" --setting "LEARN_LAYERS=$layers" \
    --setting "LEARN_PART_MB=${LEARN_PART_MB-}" --setting "LEARN_GHC_API=${LEARN_GHC_API-}" \
    --setting "WASM_SO_OPT=${WASM_SO_OPT-}" --setting "BSDTAR_SHA256=$bsdtar_sha256" \
    "$(dirname "$wasm_cc")/../share/wasi-sysroot/lib/wasm32-wasi" \
    toolchain/build-learn-rootfs.sh toolchain/learn-inject.py toolchain/learn-rootfs-key.py learn \
    examples/material/bundle.sh examples/material/style.scss package-lock.json
fi

step "building $packages for wasm"
build "$@"

compiler=$(python3 -I -c 'import json, sys; p = json.load(open(sys.argv[1])); print(p["compiler-id"] + "-" + p["compiler-abi"])' "$plan")
compiler_id=${compiler%-*}
store_db=$(cabal path --store-dir)/$compiler/package.db
inplace_db=$build_dir/packagedb/$compiler_id
for db in "$store_db" "$inplace_db"; do
  [ -d "$db" ] || { printf 'No package DB at %s\n' "$db" >&2; exit 1; }
done

work=$build_dir/learn-build
stage=$work/stage
rm -rf "$work" "$out"
mkdir -p "$stage/tmp" "$work/obj" "$out"
: >"$out/.building"

# 2. The compiler: GHC's API in a shared library exporting `playground`.
step "libplayground.so"
"$wasm_ghc" -v0 -package ghc -shared -dynamic -O2 \
  -no-keep-hi-files -no-keep-o-files -outputdir "$work/obj" \
  -Wall -Wextra -Werror \
  learn/Playground.hs -o "$stage/tmp/libplayground.so"

# 3. /tmp/clib: the C and C++ libraries, from the wasi-sysroot next to the
# toolchain's clang. libsetjmp and the wasi-emulated ones are for programs
# that ask for them, which GHC's libraries do not.
step "/tmp/clib"
cp -RL "$(dirname "$wasm_cc")/../share/wasi-sysroot/lib/wasm32-wasi" "$stage/tmp/clib"
chmod -R u+w "$stage/tmp/clib"
find "$stage/tmp/clib" -type f ! -name '*.so' -delete
find "$stage/tmp/clib" -type d -empty -delete
rm -f "$stage/tmp/clib/libsetjmp.so" "$stage/tmp/clib"/libwasi-emulated-*.so

# 4. /tmp/hslib/lib: the libdir, trimmed to what a dynamic, non-profiled
# compiler reads, and to the boot packages something uses (BOOT_TRIM).
step "/tmp/hslib/lib"
hslib=$stage/tmp/hslib/lib
mkdir -p "$stage/tmp/hslib"
cp -RL "$libdir" "$hslib"
chmod -R u+w "$hslib"
global_db=$hslib/package.conf.d
pkg() { "$wasm_ghc_pkg" --no-user-package-db --global-package-db="$global_db" "$@"; }
find "$hslib" '(' \
  -name '*.hi' \
  -o -name '*.a' \
  -o -name '*.p_hi' \
  -o -name 'libHS*_p.a' \
  -o -name '*.p_dyn_hi' \
  -o -name 'libHS*_p*.so' \
  -o -name 'libHSrts*_debug*.so' \
  -o -name '*.conf.copy' \
  ')' -delete
rm -rf "${hslib:?}/doc" "${hslib:?}/html" "${hslib:?}/latex" "${hslib:?}"/*.mjs "${hslib:?}"/*.js "${hslib:?}"/*.txt
# The one directory with every libHS*.so in it (the boot packages'
# dynamic-library-dirs), with a directory of interfaces per unit beside them.
searchdir=$(find "$hslib" -type f -name 'libHSbase-*.so' -exec dirname {} \; | head -n 1)
[ -n "$searchdir" ] || { printf 'No libHSbase .so in %s\n' "$hslib" >&2; exit 1; }
size() { if [ $# -eq 0 ]; then echo 0; else du -cb "$@" 2>/dev/null | tail -n 1 | cut -f1; fi; }
mb() { awk -v b="$1" 'BEGIN { printf "%8.2f MB", b / 1e6 }'; }
# shellcheck disable=SC2086
trim_ids=$(for p in $BOOT_TRIM; do pkg field "$p" id --simple-output; done)
# shellcheck disable=SC2086
pkg unregister $BOOT_TRIM
for id in $trim_ids; do
  printf '%s\n' "$searchdir/$id" "$searchdir/libHS$id-ghc${compiler_id#ghc-}.so"
done >"$work/trimmed.txt"
# shellcheck disable=SC2046
printf '    unregistered and deleted %s: %s\n' "$BOOT_TRIM" "$(mb "$(size $(cat "$work/trimmed.txt"))")"
while read -r path; do rm -rf "${path:?}"; done <"$work/trimmed.txt"
# The documentation is gone, so the boot packages' .conf no longer name it
# (ghc-pkg check would warn about every one). Unless LEARN_GHC_API=1, ghc is
# hidden and its interfaces left out, and its .conf no longer lists the
# modules it has no interfaces for, so that ghc-pkg check holds and
# -package ghc says there is no such module rather than failing on a missing
# interface. Only its library is used, by libplayground.so, which dyld loads
# by its file name.
ghc_id=$(pkg field ghc id --simple-output)
if [ "${LEARN_GHC_API-}" != 1 ]; then
  printf '    hid ghc and left out its interfaces: %s\n' \
    "$(mb "$(find "$searchdir/$ghc_id" -name '*.dyn_hi' -printf '%s\n' | awk '{ s += $1 } END { print s + 0 }')")"
  find "$searchdir/$ghc_id" -name '*.dyn_hi' -delete
  find "$searchdir/$ghc_id" -mindepth 1 -type d -empty -delete
  hide=$ghc_id
else
  hide=
fi
python3 -I - "$global_db" "$hide" <<'PY'
import glob
import os
import re
import sys

db, hide = sys.argv[1:]


def strip(text, field):
    return re.sub(rf"(?m)^{field}:.*(\n[ \t].*)*\n?", "", text)


for path in glob.glob(os.path.join(db, "*.conf")):
    text = open(path, encoding="utf-8").read()
    text = strip(strip(text, "haddock-interfaces"), "haddock-html")
    if hide and os.path.basename(path) == hide + ".conf":
        text = strip(strip(text, "exposed-modules"), "hidden-modules")
        text = re.sub(r"(?m)^exposed:.*$", "exposed: False", text)
    open(path, "w", encoding="utf-8").write(text)
PY
pkg recache
pkg check

inject() {
  # shellcheck disable=SC2046
  python3 -I toolchain/learn-inject.py --ghc-version "${compiler_id#ghc-}" \
    --source "$store_db" --source "$inplace_db" \
    $(for id in $trim_ids; do printf -- '--trimmed %s ' "$id"; done) "$@"
}

# 5. The base layer, in the global package DB.
step "base layer: $BASE_LAYER"
# shellcheck disable=SC2086
inject --db "$global_db" --so-dir "$searchdir" --hi-dir "$searchdir" \
  --present "$global_db" --listing "$work/base.txt" $BASE_LAYER
pkg recache
pkg check
pkg describe haskell-halogen-core >/dev/null

# 6. Further layers, each in a package DB of its own beside the libdir, on top
# of the global DB alone: what an earlier layer has, this one may not need.
taken=""
for layer in $layers; do
  name=$(layer_name "$layer")
  root=$stage/tmp/hslib/layers/$name
  step "layer $name: $(layer_packages "$layer")"
  # shellcheck disable=SC2046,SC2086
  inject --db "$root/package.conf.d" --so-dir "$root/lib" --hi-dir "$root" \
    --present "$global_db" $taken --listing "$work/$name.txt" $(layer_packages "$layer")
  pkg --package-db="$root/package.conf.d" recache
  pkg --package-db="$root/package.conf.d" check
  taken="$taken --taken $root/package.conf.d"
done

# 7. Optionally, wasm-opt the layers' .so, as GHC's own build does its
# libraries' with WASM_SO_OPT. Only the layers': the boot libraries are as the
# toolchain ships them, and libHSghc is too large for wasm-opt in 15 GB.
if [ -n "${WASM_SO_OPT-}" ]; then
  step "wasm-opt $WASM_SO_OPT"
  sed -n 's/^so //p' "$work"/*.txt | while read -r so; do
    # shellcheck disable=SC2086
    wasm-opt $WASM_SO_OPT "$so" -o "$so.opt"
    mv "$so.opt" "$so"
  done
fi

# 8. The page and what it loads beside the archives.
step "page"
cp learn/web/index.html learn/web/index.js "$out/"
cp "$libdir/dyld.mjs" "$libdir/post-link.mjs" "$libdir/prelude.mjs" "$out/"
# dyld gives every library it instantiates an `env` that is a copy of every
# function every library loaded before it exports: some hundred thousand
# properties once GHC is loaded, copied again for each of the base layer's
# sixty libraries, which made the first program to use haskell-halogen-core
# take three times as long. A library only needs the functions it imports,
# which dyld's loop over its imports can look up, so have it do that. Should
# a later dyld have changed this code, the page works the same unpatched, only
# slower.
python3 -I - "$out/dyld.mjs" <<'PY'
import sys

path = sys.argv[1]
text = open(path, encoding="utf-8").read()
spread = """          __stack_pointer: this.#sp,
          ...this.exportFuncs,
        },"""
loop = """      for (const { module, name, kind } of WebAssembly.Module.imports(mod)) {
"""
if text.count(spread) == 1 and text.count(loop) == 1:
    text = text.replace(spread, spread.replace("          ...this.exportFuncs,\n", ""))
    text = text.replace(
        loop,
        loop
        + """        // Patched by toolchain/build-learn-rootfs.sh: only what is imported.
        if (module === "env" && !(name in import_obj.env) && this.exportFuncs[name]) {
          import_obj.env[name] = this.exportFuncs[name];
        }
""",
    )
    open(path, "w", encoding="utf-8").write(text)
    print("    patched dyld.mjs to look up only the functions each library imports")
else:
    print("    dyld.mjs has changed: left as it is (see the comment in build-learn-rootfs.sh)")
PY
# dyld imports browser_wasi_shim from esm.sh, and the page must use the same
# copy, as the WASI instance dyld makes checks the file system the page hands
# it against its own copy's classes. So dyld and the page both import the
# copy in learn/vendor/ (its header says where it comes from), which the
# rootfs ships beside dyld.mjs, and the tutorial does not depend on esm.sh
# being up. Unlike the patch above, this one must apply: a dyld left
# importing esm.sh would have a copy of its own, so the build stops instead.
cp learn/vendor/browser_wasi_shim/browser_wasi_shim.js "$out/"
python3 -I - "$out/dyld.mjs" <<'PY'
import sys

path = sys.argv[1]
text = open(path, encoding="utf-8").read()
remote = 'import("https://esm.sh/gh/haskell-wasm/browser_wasi_shim")'
if text.count(remote) != 1:
    sys.exit(f"{path} no longer imports browser_wasi_shim as {remote}: see build-learn-rootfs.sh")
open(path, "w", encoding="utf-8").write(text.replace(remote, 'import("./browser_wasi_shim.js")'))
print("    pointed dyld.mjs at browser_wasi_shim.js beside it")
PY
bsdtar_cache=$build_dir/bsdtar.wasm
if [ ! -f "$bsdtar_cache" ]; then
  curl -sSfL "$bsdtar_url" -o "$bsdtar_cache.part"
  mv "$bsdtar_cache.part" "$bsdtar_cache"
fi
if [ -n "$bsdtar_sha256" ]; then
  actual=$(sha256sum "$bsdtar_cache" | cut -d' ' -f1)
  if [ "$actual" != "$bsdtar_sha256" ]; then
    printf '%s has sha256 %s, not the pinned %s.\nDelete it to fetch it again, or set BSDTAR_SHA256 to accept it.\n' \
      "$bsdtar_cache" "$actual" "$bsdtar_sha256" >&2
    exit 1
  fi
fi
cp "$bsdtar_cache" "$out/bsdtar.wasm"

# What a layer's packages need on the page besides their libraries: their
# JavaScript and CSS (layers/<name>/), and what they load from elsewhere at
# run time. Material's components call the functions material/jsbits/material.js
# puts on globalThis, which imports the @material npm packages and so is not
# embedded in the library through browserJS as other packages' jsbits are:
# the page loads it, bundled as examples/material/bundle.sh bundles it for the
# example, with the components' CSS. Pixi loads PixiJS from a CDN itself, and
# Three Three.js.
# Lazy's demo library is what its lesson loads with Halogen.Lazy.Dyld: a
# shared library of its own, fetched by the program while it runs and linked
# into it by dyld, so it is a file served beside the layer's archive and not
# in it. Nothing else needs anything: every other package's JavaScript is in
# its library.
in_rootfs() { printf '/%s' "${1#"$stage/"}"; }
manifest=$work/manifest.txt
: >"$manifest"
for layer in $layers; do
  name=$(layer_name "$layer")
  {
    printf 'layer %s\n' "$name"
    printf 'packages %s\n' "$(layer_packages "$layer")"
    printf 'db %s\n' "$(in_rootfs "$stage/tmp/hslib/layers/$name/package.conf.d")"
    printf 'listing %s\n' "$work/$name.txt"
    printf 'tree %s\n' "$work/layers/$name"
  } >>"$manifest"
  for p in $(layer_packages "$layer"); do
    case $p in
      haskell-halogen-material)
        step "layer $name: material.js and material.css"
        if [ ! -x node_modules/.bin/esbuild ] || [ ! -x node_modules/.bin/sass ]; then
          printf 'No esbuild or sass in node_modules: run npm ci first.\n' >&2
          exit 1
        fi
        sh examples/material/bundle.sh "$out/layers/$name"
        printf 'asset script layers/%s/material.js\n' "$name" >>"$manifest"
        printf 'asset stylesheet layers/%s/material.css\n' "$name" >>"$manifest"
        font=$(sed -n 's/.*src: url(\(https:[^)]*\)).*/\1/p' examples/material/style.scss | head -n 1)
        if [ -n "$font" ]; then
          printf 'external %s the Material Icons font, which material.css names\n' "$font" >>"$manifest"
        fi
        ;;
      haskell-halogen-pixi)
        cdn=$(sed -n 's/.*moduleUrl = "\(https:[^"]*\)".*/\1/p' pixi/src/Halogen/Canvas/Pixi.hs | head -n 1)
        if [ -z "$cdn" ]; then
          printf 'No default moduleUrl in pixi/src/Halogen/Canvas/Pixi.hs\n' >&2
          exit 1
        fi
        printf 'external %s PixiJS, which Halogen.Canvas.Pixi.defaultConfig imports when a canvas mounts\n' "$cdn" >>"$manifest"
        ;;
      haskell-halogen-three)
        cdn=$(sed -n 's/.*moduleUrl = "\(https:[^"]*\)".*/\1/p' three/src/Halogen/Three.hs | head -n 1)
        if [ -z "$cdn" ]; then
          printf 'No default moduleUrl in three/src/Halogen/Three.hs\n' >&2
          exit 1
        fi
        printf 'external %s Three.js, which Halogen.Three.defaultConfig imports when a scene mounts\n' "$cdn" >>"$manifest"
        ;;
      haskell-halogen-lazy-demo)
        # lazy/demo/feature, compiled as libplayground.so is (step 2), against
        # the very libraries the layers carry: its NEEDED entries name them by
        # file name, and dyld finds them linked already when a program that
        # uses this layer loads it. Its own unit id keeps its symbols from
        # meeting the program's (both would be main's otherwise). The
        # packages are named, not their ids: the plan has one of each.
        step "layer $name: liblazy-demo.so"
        mkdir -p "$out/layers/$name" "$work/lazy-demo"
        "$wasm_ghc" -v0 -shared -dynamic -O2 -this-unit-id halogen-lazy-demo-feature \
          -no-keep-hi-files -no-keep-o-files -outputdir "$work/lazy-demo" \
          -package-env - -package-db "$store_db" -package-db "$inplace_db" -hide-all-packages \
          -package base -package mtl -package row-types -package text \
          -package haskell-halogen-core -package haskell-halogen-lazy -package haskell-halogen-lazy-demo \
          -Wall -Wextra -Werror -ilazy/demo/feature \
          lazy/demo/feature/Entry.hs lazy/demo/feature/Feature.hs -o "$out/layers/$name/liblazy-demo.so"
        printf 'library layers/%s/liblazy-demo.so the demo library its lesson loads with Halogen.Lazy.Dyld\n' "$name" >>"$manifest"
        ;;
    esac
  done
done

# 9. The archives: the layers', then the base one without them, each split
# into parts if it is larger than a file served may be.
step "archives (zstd -$level)"
pack() {
  ZSTD_CLEVEL=$level ZSTD_NBTHREADS=0 tar -C "$stage" \
    --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner \
    --zstd -cf "$@"
}
split_large() {
  if [ "$(size "$1")" -gt "$part_bytes" ]; then
    split -d -a 2 -b "$part_bytes" "$1" "$1."
    rm "$1"
    printf '    split %s into %s parts\n' "${1#"$out/"}" "$(find "$(dirname "$1")" -name "$(basename "$1").??" | wc -l)"
  fi
}
for layer in $layers; do
  name=$(layer_name "$layer")
  mkdir -p "$out/layers"
  pack "$out/layers/$name.tar.zst" "tmp/hslib/layers/$name"
  split_large "$out/layers/$name.tar.zst"
done
if [ -n "$layers" ]; then
  mv "$stage/tmp/hslib/layers" "$work/layers"
fi
pack "$out/rootfs.tar.zst" tmp
split_large "$out/rootfs.tar.zst"

# The manifest, rootfs.json (learn/README.md describes every field).
python3 -I - "$out" "$manifest" "$work/base.txt" "$stage/tmp" "$compiler_id" "$(in_rootfs "$searchdir")" "$BASE_LAYER" <<'PY'
import glob
import hashlib
import json
import os
import sys

out, manifest, base_listing, tree, ghc, searchdir, base_packages = sys.argv[1:]


def tree_size(path):
    return sum(os.path.getsize(os.path.join(d, f)) for d, _, files in os.walk(path) for f in files)


def archive(path):
    whole = os.path.join(out, path)
    files = [whole] if os.path.exists(whole) else sorted(glob.glob(whole + ".[0-9][0-9]"))
    if not files:
        sys.exit(f"no {path} in {out}")
    parts = [{"path": os.path.relpath(f, out), "size": os.path.getsize(f)} for f in files]
    return {"archive": path, "parts": parts, "compressedSize": sum(p["size"] for p in parts)}


def unit_ids(listing):
    return [line.split()[1] for line in open(listing, encoding="utf-8") if line.startswith("unit ")]


rootfs = {
    **archive("rootfs.tar.zst"),
    "size": tree_size(tree),
    "packages": base_packages.split(),
    "units": unit_ids(base_listing),
}
layers, layer = [], None
for line in open(manifest, encoding="utf-8"):
    key, _, value = line.rstrip("\n").partition(" ")
    if key == "layer":
        layer = {"name": value, "assets": [], "external": [], "libraries": []}
        layers.append(layer)
    elif key == "packages":
        layer["packages"] = value.split()
    elif key == "db":
        layer["packageDb"] = value
    elif key == "listing":
        layer["units"] = unit_ids(value)
    elif key == "tree":
        layer["size"] = tree_size(value)
    elif key == "asset":
        kind, path = value.split(" ", 1)
        layer["assets"].append({"kind": kind, "path": path, "size": os.path.getsize(os.path.join(out, path))})
    elif key == "external":
        url, what = value.split(" ", 1)
        layer["external"].append({"url": url, "what": what})
    elif key == "library":
        path, what = value.split(" ", 1)
        layer["libraries"].append({"path": path, "size": os.path.getsize(os.path.join(out, path)), "what": what})
for layer in layers:
    layer.update(archive(f"layers/{layer['name']}.tar.zst"))


# The version: a hash of every part of every archive, with its name, so that
# it changes whenever any archive's content does, and only then (packing is
# reproducible). A page keeps the archives it downloaded under it.
def version():
    h = hashlib.sha256()
    for entry in [rootfs, *layers]:
        for part in entry["parts"]:
            h.update(part["path"].encode() + b"\0")
            with open(os.path.join(out, part["path"]), "rb") as f:
                for chunk in iter(lambda: f.read(1 << 20), b""):
                    h.update(chunk)
    return h.hexdigest()[:16]


# The fields the first page read come first (name, archive, packageDb).
order = ["name", "archive", "packageDb", "packages", "units", "parts", "size", "compressedSize", "assets", "external", "libraries"]
config = {
    "ghc": ghc,
    "version": version(),
    # The base archive's, as in rootfs below, for a page that reads only the
    # top level.
    "size": rootfs["size"],
    "compressedSize": rootfs["compressedSize"],
    "libdir": "/tmp/hslib/lib",
    "searchDirs": ["/tmp/clib", searchdir],
    "mainSoPath": "/tmp/libplayground.so",
    "rootfs": rootfs,
    "layers": [{k: layer[k] for k in order} for layer in layers],
}
with open(os.path.join(out, "rootfs.json"), "w", encoding="utf-8") as f:
    json.dump(config, f, indent=2)
    f.write("\n")
PY

# Sizes, uncompressed, by part.
part() {
  # part <label> <files...>
  label=$1
  shift
  line="  $(printf '%-28s' "$label") $(mb "$(size "$@")")"
  # With no files (ghc's interfaces, when they are left out) there is nothing
  # to compress, and cat would read the script's stdin.
  if [ "${LEARN_REPORT-}" = 1 ] && [ $# -gt 0 ]; then
    line="$line  $(mb "$(cat "$@" | zstd -q "-$level" -T0 -c | wc -c)") compressed"
  fi
  printf '%s\n' "$line"
}
base_so=$(sed -n 's/^so //p' "$work/base.txt")
base_hi=$(sed -n 's/^unit //p' "$work/base.txt" | while read -r u; do find "$searchdir/$u" -name '*.dyn_hi'; done)
boot_so=$(find "$searchdir" -maxdepth 1 -name 'libHS*.so' | grep -vxF "$base_so")
boot_hi=$(find "$searchdir" -name '*.dyn_hi' | grep -vxF "$base_hi")
step "sizes"
# shellcheck disable=SC2046,SC2086
{
  part "libplayground.so" "$stage/tmp/libplayground.so"
  part "clib (.so)" $(find "$stage/tmp/clib" -type f)
  part "boot libraries (.so)" $boot_so
  part "  of which libHSghc" $(printf '%s\n' "$boot_so" | grep '/libHSghc-[0-9]')
  part "boot libraries (.dyn_hi)" $boot_hi
  part "  of which ghc's" $(find "$searchdir"/ghc-[0-9]* -name '*.dyn_hi')
  part "base layer (.so)" $base_so
  part "base layer (.dyn_hi)" $base_hi
  part "rest of libdir" $(find "$hslib" -type f ! -name '*.so' ! -name '*.dyn_hi')
}
printf '  %-28s %s\n' "rootfs, uncompressed" "$(mb "$(size "$stage/tmp")")"
python3 -I - "$out/rootfs.json" <<'PY'
import json
import sys

config = json.load(open(sys.argv[1], encoding="utf-8"))


def mb(n):
    return f"{n / 1e6:8.2f} MB"


r = config["rootfs"]
print(f"  {'rootfs.tar.zst':28} {mb(r['compressedSize'])} in {len(r['parts'])} file(s)")
print(f"  {'version':28} {config['version']}")
print(f"  {'layer':10} {'units':>5} {'uncompressed':>12} {'compressed':>12} {'files':>5}  assets and libraries")
for layer in config["layers"]:
    assets = ", ".join(a["path"] for a in layer["assets"] + layer["libraries"]) or "-"
    external = f"; loads {', '.join(e['url'] for e in layer['external'])}" if layer["external"] else ""
    print(
        f"  {layer['name']:10} {len(layer['units']):5} {mb(layer['size'])} {mb(layer['compressedSize'])}"
        f" {len(layer['parts']):5}  {assets}{external}"
    )
layers = sum(
    layer["compressedSize"] + sum(a["size"] for a in layer["assets"] + layer["libraries"]) for layer in config["layers"]
)
print(f"  {'every archive and asset':28} {mb(r['compressedSize'] + layers)}")
PY
largest=$(find "$out" -type f -printf '%s %p\n' | sort -n | tail -n 1)
printf '  largest file served: %s (%s)\n' "${largest#* }" "$(mb "${largest%% *}")"

rm -rf "$work" "$out/.building"
printf '\nBuilt the rootfs and the playground in %s; serve it with any static server, or npm run test-playground.\n' "$out"
