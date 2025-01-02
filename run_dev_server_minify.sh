#!/bin/bash

set -ex

export EXE=$(cabal exec -v0 --project-file=cabal-ghcjs.project -- which halogen-core-prototype)".jsexe"
export IN1=$EXE"/all.js"
export IN2=$EXE"/all.externs.js"
export OUT1=$EXE"/all.min.js"

export OUT2="dev/index.js"

cabal build -v0 -fforce-recomp --project-file=cabal-ghcjs.project all
google-closure-compiler -O ADVANCED --js_output_file $OUT1 $IN1 $IN2
cp $OUT1 $OUT2
# terser $IN --compress toplevel=true --mangle toplevel=true --output $OUT
http-server dev/ # or parcel dev/index.html or webpack