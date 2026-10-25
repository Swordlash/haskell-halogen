#!/bin/sh
# Browser-GHCi loop for an example: the example runs in a browser tab, and
# ghciwatch reloads it and reruns it (:main) on every save.
#
#   sh toolchain/dev.sh vanilla                 # npm run dev -- vanilla
#   sh toolchain/dev.sh --repl vanilla          # plain GHCi, for typing :main yourself
#   sh toolchain/dev.sh --open-browser vanilla  # then open its page from another terminal
#
# The example's executable must have an `interactive` flag, as every example's
# has: it compiles the example's own modules into the executable, which is the
# target GHCi reloads, and drops the wasm reactor options, which GHCi cannot
# load. Its main ends the last :main's tree (and the tutorial its router)
# before mounting again, with Halogen.IO.Util's endPreviousRun and
# keepForNextRun, because the page stays open from one run to the next.
#
# The page is the example's own web/index.html, with the script that loads
# index.js (which fetches the built app.wasm) replaced by browser GHCi's
# script, and the rest of web/ and whatever bundle.sh makes beside it. GHCi
# serves it at http://127.0.0.1:$PORT/assets/index.html (PORT defaults to
# 8080). GHCi's interpreter runs in the page, and GHCi starts loading the
# example only once a page has connected; reloading the page ends the session
# until GHCi is restarted, and every restart opens a new page.
#
# examples/learn, the tutorial, routes by path. The page's <base href> is
# /assets/, which its router takes for its base, so its links go to
# /assets/<lesson>/; /assets/index.html is no route of its, so it starts at its
# fallback, the introduction. Following its links works, but there is no page
# at their addresses to reload (which would end the session anyway). Its
# bundle.sh copies the rootfs (npm run build-rootfs) to rootfs/ beside the
# page, so the compiler loads there as it does when deployed.
set -eu

cd "$(dirname "$0")/.."
. "$HOME/.ghc-wasm/env"
export PATH="$HOME/.local/bin:$PATH"
# Browser GHCi's server needs the ws package, which ghc-wasm-meta's Node comes
# with; its env file points NODE_PATH there only when NODE_PATH is unset.
export NODE_PATH="$HOME/.ghc-wasm/nodejs/lib/node_modules${NODE_PATH:+:$NODE_PATH}"

self=toolchain/dev.sh
mode=watch
case "${1:-}" in
  --repl|--open-browser) mode=${1#--}; shift ;;
esac

example=${1:?usage: dev.sh [--repl|--open-browser] <example>}
example_dir=examples/$example
package=halogen-example-$example
port=${PORT:-8080}
url="http://127.0.0.1:$port/assets/index.html"
# Absolute, because GHCi resolves the assets directory from the example's
# directory, which is where cabal starts it.
public_dir=$(pwd)/dist-newstyle/wasm-dev/examples/$example

case "$port" in
  ''|*[!0-9]*) printf 'PORT must be a number.\n' >&2; exit 1 ;;
esac

if [ ! -f "$example_dir/$package.cabal" ]; then
  printf 'No such example: %s has no %s.cabal.\n' "$example_dir" "$package" >&2
  exit 1
fi
if ! grep -Eq '^flag[[:space:]]+interactive[[:space:]]*$' "$example_dir/$package.cabal"; then
  printf '%s has no interactive flag; see examples/vanilla for one.\n' "$example_dir/$package.cabal" >&2
  exit 1
fi

# The page GHCi boots, made from the one the built example is served with so
# that its markup, styles and #app are the same.
prepare_page() {
  rm -rf "$public_dir"
  mkdir -p "$public_dir"
  cp -R "$example_dir/web/." "$public_dir/"
  rm -f "$public_dir/index.js"
  if [ -f "$example_dir/bundle.sh" ]; then
    sh "$example_dir/bundle.sh" "$public_dir"
  fi
  # What index.js imports only for its effect (material's material.js, which
  # publishes the functions its JSFFI calls on globalThis) is loaded in its
  # place, before GHCi's script, as index.js loads it before app.wasm. Such
  # an import is a line `import "./x.js";` or `import './x.js'`, with or
  # without the semicolon; one that names what it imports (`import x from`)
  # is index.js's own business, which GHCi's script does in its place.
  imports=$(awk '/^import[ \t]+["\047]\.\/[^"\047]+["\047][ \t]*;?[ \t]*$/ { match($0, /\.\/[^"\047]+/); print substr($0, RSTART, RLENGTH) }' "$example_dir/web/index.js")
  # A page without a <base href> of its own gets one naming where GHCi
  # serves it, which is where the page's relative addresses resolve to
  # already, and which a router takes as its base (Halogen.Router.pageBase).
  # Whether it has one is decided before the page is rewritten, because the
  # browser goes by the first <base>, and ours would come before the page's.
  if grep -Eqi '<base[[:space:]>]' "$example_dir/web/index.html"; then has_base=1; else has_base=0; fi
  if ! IMPORTS=$imports awk -v has_base="$has_base" '
    !has_base && !based && match($0, /<head([ \t][^>]*)?>/) {
      indent = $0
      sub(/[^ \t].*$/, "", indent)
      $0 = substr($0, 1, RSTART + RLENGTH - 1) "\n" indent "  <base href=\"/assets/\">" substr($0, RSTART + RLENGTH)
      based = 1
    }
    !replaced && match($0, /<script[^>]*src="(\.\/)?index\.js"[^>]*>[ \t]*<\/script>/) && substr($0, RSTART, RLENGTH) ~ /type="module"/ {
      indent = $0
      sub(/[^ \t].*$/, "", indent)
      scripts = ""
      n = split(ENVIRON["IMPORTS"], imported, "\n")
      for (i = 1; i <= n; i++)
        if (imported[i] != "")
          scripts = scripts "<script type=\"module\" src=\"" imported[i] "\"></script>\n" indent
      $0 = substr($0, 1, RSTART - 1) scripts "<script type=\"module\" src=\"../main.js\"></script>" substr($0, RSTART + RLENGTH)
      replaced = 1
    }
    { print }
    END { exit replaced ? 0 : 1 }
  ' "$example_dir/web/index.html" > "$public_dir/index.html"; then
    printf '%s has no <script type="module" src="./index.js"></script> to load browser GHCi in place of.\n' "$example_dir/web/index.html" >&2
    exit 1
  fi
}

case "$mode" in
  open-browser)
    # ghciwatch runs an opener on every start of GHCi, and one whose GHCi
    # failed before serving the page (a build error) would otherwise still be
    # polling when a later start serves it, and open a tab of its own beside
    # the later opener's. So only the latest opener polls: it ends the one
    # before it, if that pid is still an opener's (a pid can be reused), and
    # leaves its own beside the page for the next one.
    mkdir -p "$(dirname "$public_dir")"
    pid_file=$public_dir.opener.pid
    if previous=$(cat "$pid_file" 2>/dev/null) && ps -p "$previous" -o args= 2>/dev/null | grep -q -- '--open-browser'; then
      kill "$previous" 2>/dev/null || true
    fi
    echo $$ > "$pid_file"
    # Ten minutes covers GHCi's start once the libraries are built. A first
    # build can take longer, and then the page is left to open by hand: GHCi
    # prints its address when it starts.
    attempts=0
    until curl --fail --silent --max-time 1 --output /dev/null "$url"; do
      attempts=$((attempts + 1))
      if [ "$attempts" -ge 600 ]; then
        printf 'Browser GHCi is not serving %s yet; open it once it does.\n' "$url" >&2
        rm -f "$pid_file"
        exit 1
      fi
      sleep 1
    done
    rm -f "$pid_file"
    exec sh toolchain/open-browser.sh "$url"
    ;;
  repl)
    # Every start writes the page afresh, so a restart picks up a change to
    # it or to what bundle.sh bundles.
    prepare_page
    # Cabal may reuse the host ghc-pkg from this cache when switching toolchains.
    rm -f dist-newstyle/wasm-dev/cache/compiler
    printf 'Browser GHCi will serve %s at %s; GHCi starts once a page has connected.\n' "$example" "$url" >&2
    # -Wwarn undoes cabal.project's -Werror for the example GHCi loads: an
    # unused import halfway through an edit shouldn't stop it from rerunning.
    # GHCi runs :main as System.Environment.withArgs [] (Control.Monad.void
    # main), which needs base exposed, and an example that depends on
    # protolude rather than on base does not expose it.
    exec cabal repl --project-file=cabal-wasm.project \
      --with-compiler="$(command -v wasm32-wasi-ghc)" \
      --with-hc-pkg="$(command -v wasm32-wasi-ghc-pkg)" \
      --with-hsc2hs="$(command -v wasm32-wasi-hsc2hs)" \
      --builddir=dist-newstyle/wasm-dev --disable-multi-repl --enable-shared \
      --constraint="$package +interactive" \
      "$package:exe:$package" \
      --repl-options="-Wwarn -package base -fghci-browser -fghci-browser-port=$port -fghci-browser-assets-dir=$public_dir"
    ;;
esac

if ! command -v ghciwatch >/dev/null 2>&1; then
  printf 'Install ghciwatch: https://mercurytechnologies.github.io/ghciwatch/\nFor manual runs, use: sh %s --repl %s\n' "$self" "$example" >&2
  exit 1
fi

# The example's modules reload in place. The libraries it is built on are
# compiled by cabal before GHCi starts, so a change to one, or to a cabal
# file, needs GHCi restarted to rebuild it; so does a change to the page,
# which is written when GHCi starts and which the browser has loaded already.

# The arguments, one a line, that restart GHCi for what in an example's
# directory goes into the page rather than into GHCi: its web/, and its
# bundle.sh and what that bundles: the stylesheets (material's) and js/ (the
# tutorial's). An example this one depends on needs the same, since its
# bundle.sh may run the other's.
example_restart_globs() {
  printf '%s\n' --restart-glob "$1/web/**" --restart-glob "$1/bundle.sh" --restart-glob "$1/*.scss" --restart-glob "$1/js/**"
}

set -- --watch "$example_dir"
lines=$(example_restart_globs "$example_dir")
for library in core jsbits hooks router net custom-element tauri material pixi sound th th-plugin; do
  for dir in src jsbits; do
    if [ -d "$library/$dir" ]; then
      set -- "$@" --watch "$library/$dir" --restart-glob "$library/$dir/**"
    fi
  done
done
# The examples this one depends on as libraries, if any, are rebuilt by
# cabal too, and may bundle into its page.
for other in examples/*/; do
  other=${other%/}
  name=${other#examples/}
  if [ "$other" != "$example_dir" ] && grep -Eq "(^|[[:space:],:])halogen-example-$name([[:space:],]|\$)" "$example_dir/$package.cabal"; then
    set -- "$@" --watch "$other" --restart-glob "$other/src/**"
    lines="$lines
$(example_restart_globs "$other")"
  fi
done
while IFS= read -r line; do
  set -- "$@" "$line"
done <<GLOBS
$lines
GLOBS
for cabal_file in */*.cabal examples/*/*.cabal; do
  set -- "$@" --watch "$cabal_file"
done

# A restart starts a new GHCi, which serves a new page: the opener runs on
# every startup and opens it.
exec ghciwatch \
  --command "sh $self --repl $example" \
  --before-startup-shell "async:sh $self --open-browser $example" \
  "$@" \
  --watch cabal.project --watch cabal-wasm.project \
  --restart-glob "**/*.cabal" --restart-glob cabal.project --restart-glob cabal-wasm.project \
  --after-startup-ghci ':main' --after-reload-ghci ':main' --debounce 100ms --poll 500ms
