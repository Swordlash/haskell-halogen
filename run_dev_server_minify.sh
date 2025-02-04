#!/bin/bash

set -ex

sh run_fourmolu.sh

cabal build -fforce-recomp --project-file=cabal-ghcjs.project all
export EXE=$(cabal exec -v0 --project-file=cabal-ghcjs.project -- which halogen-core-prototype)".jsexe"
export IN1=$EXE"/all.js"
export IN2=$EXE"/all.externs.js"
export OUT1=$EXE"/all.min.js"

export OUT2="dev/index.js"

#npx google-closure-compiler -O ADVANCED --language_in UNSTABLE --warning_level QUIET --isolation_mode IIFE --assume_function_wrapper --emit_use_strict --js_output_file $OUT1 $IN1 $IN2
cp $IN1 $OUT2
# terser $IN --compress toplevel=true --mangle toplevel=true --output $OUT
npx http-server dev/ # or parcel dev/index.html or webpack