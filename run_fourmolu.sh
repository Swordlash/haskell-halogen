#!/bin/sh
set -eu
cd "$(dirname "$0")"
fourmolu -i core/src core/test hooks/src hooks/test pixi/src hspec-halogen/src material/test examples/*
