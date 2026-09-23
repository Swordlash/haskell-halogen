# Sourced by the test wrappers, which own and remove browser_test_dir. Each
# suite gets its own storage file even when Cabal runs suites concurrently.
browser_test_tools=$(CDPATH= cd "$(dirname "$0")" && pwd)
NODE_OPTIONS="${NODE_OPTIONS:+$NODE_OPTIONS }--experimental-webstorage --localstorage-file=\"$browser_test_dir/local\" --require=\"$browser_test_tools/browser-test-globals.cjs\""
export NODE_OPTIONS
