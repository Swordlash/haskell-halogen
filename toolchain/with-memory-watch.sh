#!/bin/sh
# Run a command under a time limit, saying how much memory the machine has
# left while it runs and, when it fails or is stopped, what was using it and
# what the kernel killed.
#
#   sh toolchain/with-memory-watch.sh <minutes> <command> [args...]
#
# For CI's JavaScript job, whose build twice stopped at DOM.HTML.Indexed (a
# module whose one splice the JavaScript backend runs in node) with
# "terminated" printed twice and nothing after it, and whose test step has
# overrun the same way, each time holding its runner until it was cancelled
# and leaving no log to say why. A command that is still going after the limit
# is killed with everything it started, so that the step fails while the
# runner can still upload its log, and what is printed here says whether
# memory ran out and whose.
#
# MEMORY_WATCH_INTERVAL is the seconds between two lines (60).
set -u

minutes=${1:?usage: with-memory-watch.sh <minutes> <command> [args...]}
shift
interval=${MEMORY_WATCH_INTERVAL:-60}

# One line: the time, memory used and available, swap used, and the process
# holding the most.
memory_line() {
  free -m | awk -v at="$(date -u +%H:%M:%S)" '
    NR == 2 { used = $3; available = $7 }
    NR == 3 { swap = $3 }
    END { printf "memory %s: used %s MB, available %s MB, swap used %s MB", at, used, available, swap }'
  ps -eo rss=,comm= --sort=-rss | awk 'NR == 1 { printf "; largest: %s (%d MB)\n", $2, $1 / 1024 }'
}

(
  while sleep "$interval"; do
    memory_line
  done
) &
watch=$!

status=0
timeout --kill-after=30 "${minutes}m" "$@" || status=$?

kill "$watch" 2>/dev/null
wait "$watch" 2>/dev/null

if [ "$status" -ne 0 ]; then
  case "$status" in
    124 | 137) printf 'Stopped: still running after %s minutes.\n' "$minutes" ;;
    *) printf 'Failed with status %s.\n' "$status" ;;
  esac
  memory_line
  echo 'The processes holding the most memory:'
  ps -eo rss=,args= --sort=-rss | head -5 | awk '{ printf "  %d MB  ", $1 / 1024; $1 = ""; print substr($0, 2, 150) }'
  # What the kernel killed for want of memory, if anything. Reading its log
  # needs root, which CI's runner gives without asking.
  killed=$( (sudo -n dmesg 2>/dev/null || dmesg 2>/dev/null) | grep -i -E 'out of memory|oom-kill|killed process' | tail -10)
  if [ -n "$killed" ]; then
    echo 'The kernel killed, for want of memory:'
    printf '%s\n' "$killed" | sed 's/^/  /'
  else
    echo 'The kernel reports no process killed for want of memory.'
  fi
fi

exit "$status"
