#!/bin/sh
set -eu
browser_test_dir=$(mktemp -d)
trap 'rm -rf "$browser_test_dir"' EXIT HUP INT TERM
. "$(dirname "$0")/browser-test-env.sh"
"$@"
