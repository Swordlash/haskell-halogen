#!/bin/sh
set -eu
cd "$(dirname "$0")"
fourmolu -i core/src core/test pixi/src examples
