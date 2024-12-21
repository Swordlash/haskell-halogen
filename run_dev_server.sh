#!/bin/bash

set -ex

cabal build -v0 --project-file=cabal-ghcjs.project all
cp "./dist-newstyle/build/javascript-ghcjs/ghc-9.10.0.20240413/haskell-halogen-core-0.1.0.0/x/halogen-core-prototype/build/halogen-core-prototype/halogen-core-prototype.jsexe/all.js" "dev/index.js"
http-server dev/