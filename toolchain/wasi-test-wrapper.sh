#!/bin/sh
set -eu
exec wasmtime run --dir . "$@"
