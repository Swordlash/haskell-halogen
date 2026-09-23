#!/bin/sh
# A failing wasm process must fail the test command, on every Node version.
set -eu
runner_test_dir=$(mktemp -d)
trap 'rm -rf "$runner_test_dir"' EXIT HUP INT TERM
cat > "$runner_test_dir/Main.hs" <<'EOF'
module Main where
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
main :: IO ()
main = do
  args <- getArgs
  exitWith $ if args == ["fail"] then ExitFailure 7 else ExitSuccess
EOF
wasm32-wasi-ghc -outputdir "$runner_test_dir" -o "$runner_test_dir/check.wasm" "$runner_test_dir/Main.hs"
sh "$(dirname "$0")/wasm-test-wrapper.sh" "$runner_test_dir/check.wasm"
if sh "$(dirname "$0")/wasm-test-wrapper.sh" "$runner_test_dir/check.wasm" fail; then
  echo "wasm test runner discarded a failing exit status" >&2
  exit 1
else
  status=$?
  test "$status" -eq 7
fi
