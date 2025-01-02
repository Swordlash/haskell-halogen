#!/bin/bash

set -ex

./run_fourmolu.sh
cabal build -v0 -fforce-recomp --project-file=cabal-ghcjs.project all

export EXE=$(cabal exec -v0 --project-file=cabal-ghcjs.project -- which halogen-material-app)".jsexe"
export IN1=$EXE"/all.js"
export IN2=$EXE"/all.externs.js"
export OUT1=$EXE"/all.min.js"
export OUT2="dev/index.js"

# npx google-closure-compiler -O ADVANCED --js_output_file $OUT1 $IN1 $IN2
# cp $IN1 $OUT1
cp $IN1 $OUT2

npx parcel build dev/index.html --dist-dir dist

# this fails on undeclared variable for some reason (from parcel, not ghc output)
# export IN=$(ls dist/*.js)
# export OUT=dist/temp.js
# npx google-closure-compiler -O ADVANCED --js $IN --js_output_file dist/temp.js

#cp $IN $IN.bak
#cp $IN $OUT
#rm $IN