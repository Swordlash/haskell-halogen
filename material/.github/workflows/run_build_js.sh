#!/bin/bash
set -ex

echo "Build with GHC-JS"
cabal build --project-file=cabal-ghcjs.project all