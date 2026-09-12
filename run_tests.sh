#!/bin/bash

set -ex

echo "Running native"
cabal test

echo "Running ghcjs"
cabal test --project-file=cabal-ghcjs.project