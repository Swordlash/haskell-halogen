#!/bin/sh
set -eu
cd "$(dirname "$0")"
fourmolu -i jsbits/src jsbits/test core/src core/test core/bench hooks/src hooks/test router/src router/test pixi/src sound/src sound/test hspec-halogen/src hspec-halogen/app th/src th/test material/test net/src net/test tauri/src tauri/test examples/*
