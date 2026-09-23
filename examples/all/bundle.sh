#!/bin/sh
# The gallery mounts the material example, so it needs the same Material
# Components JavaScript and CSS next to its app.wasm.
#
#   sh examples/all/bundle.sh <output-dir>
set -eu

exec sh "$(dirname "$0")/../material/bundle.sh" "$@"
