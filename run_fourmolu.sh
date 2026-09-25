#!/bin/sh
set -eu
cd "$(dirname "$0")"
fourmolu -i core/src core/test hooks/src hooks/test pixi/src sound/src sound/test hspec-halogen/src hspec-halogen/app material/test examples/*
