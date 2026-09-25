#!/bin/sh
set -eu

# A suite built with hspec-halogen runs in headless Chromium, which the
# hspec-halogen executable (test-ghcjs.sh builds it) takes it to. Such a
# program carries the name of the page global its arguments arrive in. It
# gets none of the Node test globals below, which would make Playwright's own
# process look like a page.
if grep -q __halogenTestArgs "$1"; then
  exec "${HSPEC_HALOGEN:?build it first: HSPEC_HALOGEN=\$(sh toolchain/build-hspec-halogen.sh)}" test "$@"
fi

browser_test_dir=$(mktemp -d)
trap 'rm -rf "$browser_test_dir"' EXIT HUP INT TERM
. "$(dirname "$0")/browser-test-env.sh"
"$@"
