#!/bin/sh
# The material components' JavaScript and CSS, which the browser test runner
# loads before the suite: the same bundle the example app is served with.
set -eu
exec sh "$(dirname "$0")/../../../examples/material/bundle.sh" "$1"
