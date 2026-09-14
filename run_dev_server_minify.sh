#!/bin/bash

set -ex

sh run_fourmolu.sh

cabal build -fforce-recomp --project-file=cabal-ghcjs.project all
export IN1="$(find dist-newstyle/build/javascript-ghcjs/ghc-9.12.2 -type f -path '*/halogen-core-prototype.jsexe/all.js' -print -quit)"
test -n "$IN1"
export EXE="${IN1%/all.js}"
export IN2=$EXE"/all.externs.js"
export OUT1=$EXE"/all.min.js"

export OUT2="dev/index.js"

#npx google-closure-compiler -O ADVANCED --language_in UNSTABLE --warning_level QUIET --isolation_mode IIFE --assume_function_wrapper --emit_use_strict --js_output_file $OUT1 $IN1 $IN2
cp $IN1 $OUT2
# terser $IN --compress toplevel=true --mangle toplevel=true --output $OUT
server_port=${PORT:-8080}
server_url="http://127.0.0.1:${server_port}"

if [ "${NO_OPEN:-0}" != 1 ]; then
  (sleep 1; sh toolchain/open-browser.sh "$server_url") &
fi

exec npx http-server dev/ -p "$server_port" # or parcel dev/index.html or webpack
