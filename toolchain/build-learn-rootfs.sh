#!/bin/sh
# Build the in-browser playground in learn/: GHC itself, compiled to wasm as a
# shared library, with the boot libraries and haskell-halogen-core in an
# archive the page unpacks into its in-memory file system.
#
#   sh toolchain/build-learn-rootfs.sh [output dir] [cabal args...]   # npm run build-learn
#
# The output directory (default dist-newstyle/wasm/learn) is a static site:
# serve it as it is (npm run test-learn does, in headless Chromium). It holds
#
#   rootfs.tar.zst   everything the compiler reads, extracted to /tmp:
#                      /tmp/libplayground.so  the GHC API wrapper (learn/Playground.hs)
#                      /tmp/clib              the wasi-sysroot's C and C++ .so
#                      /tmp/hslib/lib         GHC's libdir: settings, the global
#                                             package DB, every boot library's .so
#                                             and .dyn_hi, and the base layer's
#   layers/<name>.tar.zst   further layers, if LEARN_LAYERS asks for any
#   rootfs.json      what the page hands to dyld and GHC: the paths in the
#                    archive and the layers' package DBs
#   dyld.mjs, post-link.mjs, prelude.mjs   GHC's wasm dynamic linker, from the libdir
#   bsdtar.wasm      libarchive's bsdtar, which the page extracts the archives with
#   index.html, index.js                   learn/web/
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
# holds what the layer's packages need that the layers below it do not. See
# learn/README.md for why a DB of its own rather than the global one.
#
# This follows GHC's own browser playground (its testsuite's
# ghc-api-browser/playground001.sh) and try-miso's build-root.sh.
#
# Environment:
#   ZSTD_CLEVEL    zstd level for the archives (default 19, as for a release;
#                  3 packs in seconds, for trying things out)
#   LEARN_LAYERS   further layers, as name=package,package... separated by
#                  spaces, each on top of those before it (default
#                  'hooks=haskell-halogen-hooks'; empty for none), for example
#                  LEARN_LAYERS='hooks=haskell-halogen-hooks router=haskell-halogen-router'
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
case ${1-} in
  '' | -*) ;;
  *)
    out=$1
    shift
    ;;
esac
level=${ZSTD_CLEVEL:-19}
# One further layer by default, so that the page (and test-learn) show a layer
# working: haskell-halogen-hooks.
layers=${LEARN_LAYERS-hooks=haskell-halogen-hooks}

# The roots of the base layer; the rest of it is their dependencies' closure.
BASE_LAYER="haskell-halogen-jsbits haskell-halogen-core"

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
step "building $packages for wasm"
rm -f "$build_dir/cache/compiler"
# shellcheck disable=SC2046
cabal build \
  --project-file=cabal-wasm.project \
  --builddir="$build_dir" \
  --with-compiler="$wasm_ghc" \
  --with-hc-pkg="$wasm_ghc_pkg" \
  --with-hsc2hs="$wasm_hsc2hs" \
  "$@" $(for p in $packages; do printf '%s:lib:%s ' "$p" "$p"; done)

plan=$build_dir/cache/plan.json
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
# compiler reads. Cabal and Cabal-syntax are a sixth of it, and no program
# here needs them.
step "/tmp/hslib/lib"
hslib=$stage/tmp/hslib/lib
mkdir -p "$stage/tmp/hslib"
cp -RL "$libdir" "$hslib"
chmod -R u+w "$hslib"
global_db=$hslib/package.conf.d
pkg() { "$wasm_ghc_pkg" --no-user-package-db --global-package-db="$global_db" "$@"; }
pkg unregister Cabal Cabal-syntax
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
rm -rf "$hslib/doc" "$hslib/html" "$hslib/latex" "$hslib"/*.mjs "$hslib"/*.js "$hslib"/*.txt
# The one directory with every libHS*.so in it (the boot packages'
# dynamic-library-dirs), with a directory of interfaces per unit beside them.
searchdir=$(find "$hslib" -type f -name 'libHSbase-*.so' -exec dirname {} \; | head -n 1)
[ -n "$searchdir" ] || { printf 'No libHSbase .so in %s\n' "$hslib" >&2; exit 1; }
# What is left of Cabal and Cabal-syntax once unregistered: their .so and
# their interfaces.
rm -rf "$searchdir"/*Cabal*

inject() {
  python3 -I toolchain/learn-inject.py --ghc-version "${compiler_id#ghc-}" \
    --source "$store_db" --source "$inplace_db" "$@"
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
# of the global DB and the layers before it.
present="--present $global_db"
stack=""
for layer in $layers; do
  name=$(layer_name "$layer")
  root=$stage/tmp/hslib/layers/$name
  step "layer $name: $(layer_packages "$layer")"
  # shellcheck disable=SC2046,SC2086
  inject --db "$root/package.conf.d" --so-dir "$root/lib" --hi-dir "$root" \
    $present --listing "$work/$name.txt" $(layer_packages "$layer")
  stack="$stack --package-db=$root/package.conf.d"
  # shellcheck disable=SC2086
  pkg $stack recache
  # shellcheck disable=SC2086
  pkg $stack check
  present="$present --present $root/package.conf.d"
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

in_rootfs() { printf '/%s' "${1#"$stage/"}"; }
layers_json=""
for layer in $layers; do
  name=$(layer_name "$layer")
  layers_json="$layers_json${layers_json:+,}
    { \"name\": \"$name\", \"archive\": \"layers/$name.tar.zst\", \"packageDb\": \"$(in_rootfs "$stage/tmp/hslib/layers/$name/package.conf.d")\" }"
done
cat >"$out/rootfs.json" <<EOF
{
  "ghc": "$compiler_id",
  "libdir": "/tmp/hslib/lib",
  "searchDirs": ["/tmp/clib", "$(in_rootfs "$searchdir")"],
  "mainSoPath": "/tmp/libplayground.so",
  "layers": [$layers_json${layers_json:+
  }]
}
EOF

# 9. The archives: the layers', then the base one without them.
step "archives (zstd -$level)"
pack() { ZSTD_CLEVEL=$level ZSTD_NBTHREADS=0 tar -C "$stage" --zstd -cf "$@"; }
for layer in $layers; do
  name=$(layer_name "$layer")
  mkdir -p "$out/layers"
  pack "$out/layers/$name.tar.zst" "tmp/hslib/layers/$name"
done
if [ -n "$layers" ]; then
  mv "$stage/tmp/hslib/layers" "$work/layers"
fi
pack "$out/rootfs.tar.zst" tmp

# Sizes, uncompressed, by part.
size() { du -cb "$@" 2>/dev/null | tail -n 1 | cut -f1; }
mb() { awk -v b="$1" 'BEGIN { printf "%8.2f MB", b / 1e6 }'; }
part() {
  # part <label> <files...>
  label=$1
  shift
  line="  $(printf '%-28s' "$label") $(mb "$(size "$@")")"
  if [ "${LEARN_REPORT-}" = 1 ]; then
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
printf '  %-28s %s\n' "rootfs.tar.zst" "$(mb "$(size "$out/rootfs.tar.zst")")"
for layer in $layers; do
  name=$(layer_name "$layer")
  printf '  %-28s %s, %s uncompressed\n' "layers/$name.tar.zst" \
    "$(mb "$(size "$out/layers/$name.tar.zst")")" "$(mb "$(size "$work/layers/$name")")"
done
largest=$(find "$out" -type f -printf '%s %p\n' | sort -n | tail -n 1)
printf '  largest file served: %s (%s)\n' "${largest#* }" "$(mb "${largest%% *}")"

rm -rf "$work"
printf '\nBuilt the playground in %s; serve it with any static server, or npm run test-learn.\n' "$out"
