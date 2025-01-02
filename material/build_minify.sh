#!/bin/bash

set -ex

./run_fourmolu.sh
cabal build -v0 -fforce-recomp --project-file=cabal-ghcjs.project all

export EXE=$(cabal exec -v0 --project-file=cabal-ghcjs.project -- which halogen-material-app)".jsexe"
export IN1=$EXE"/all.js"
export IN2=$EXE"/all.externs.js"
export IN3="src/Halogen/Material/halogen.externs.js"
export OUT1=$EXE"/all.min.js"
export OUT2="dev/index.js"

npx google-closure-compiler -O ADVANCED --js_output_file $OUT1 $IN1 $IN2 $IN3

#cp $IN1 $OUT1
cp $OUT1 $OUT2

npx webpack-cli --mode production --config webpack.config.js
