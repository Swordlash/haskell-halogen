#!/bin/sh
set -eu

url=$1

case "$(uname -s)" in
  Darwin)
    open "$url"
    ;;
  Linux)
    if command -v xdg-open >/dev/null 2>&1; then
      xdg-open "$url"
    fi
    ;;
esac
