#!/bin/bash

set -ex

./run_fourmolu.sh
cabal build -v0 -fforce-recomp --project-file=cabal-ghcjs.project all

export EXE=$(cabal exec -v0 --project-file=cabal-ghcjs.project -- which halogen-material-app)".jsexe"
export IN=$EXE"/all.js"
export OUT="dev/index.js"

cp $IN $OUT

npx webpack-cli --mode production --config webpack.config.js
