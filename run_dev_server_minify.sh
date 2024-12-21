#!/bin/bash

set -ex

export IN="./dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/haskell-halogen-core-0.1.0.0/x/halogen-core-prototype/build/halogen-core-prototype/halogen-core-prototype.jsexe/all.js"
export OUT="dev/index.js"

cabal build -v0 --project-file=cabal-ghcjs-9.12.project all
# google-closure-compiler -O ADVANCED $IN --js_output_file $OUT
terser $IN --compress toplevel=true --mangle toplevel=true --output $OUT
http-server dev/