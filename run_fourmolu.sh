#!/bin/sh
set -eu
cd "$(dirname "$0")"
# -XGHC2021: the tutorial's lessons (examples/learn/lessons) are in no cabal
# file for fourmolu to read their language from; every package's is GHC2021.
fourmolu -i -o -XGHC2021 jsbits/src jsbits/test core/src core/test core/bench hooks/src hooks/test store/src store/test router/src router/test pixi/src three/src three/test sound/src sound/test hspec-halogen/src hspec-halogen/app th/src th/test material/test net/src net/test custom-element/src custom-element/test devtools/src devtools/test worker/src worker/test lazy/src lazy/test lazy/demo/src lazy/demo/feature tauri/src tauri/test examples/*
