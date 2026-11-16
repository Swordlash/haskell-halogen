#!/bin/sh
# Run a command under a time limit and watch the machine's memory while it
# runs. Stop the command when the machine starts to live in swap. When the
# command fails or is stopped, say what was using the memory and what the
# kernel killed.
#
#   sh toolchain/with-memory-watch.sh <minutes> <command> [args...]
#
# This is for CI's JavaScript job. Its compiler can take most of a 16 GB
# runner, and several compilers run at once. When they need more than the
# machine has, the runner swaps, stops answering, and the job ends some fifty
# minutes later with no log. So this script stops the command first, while
# the runner can still upload its log.
#
# The command is stopped in two cases:
#
# * It is still running after <minutes>.
# * Swap in use stays at or above MEMORY_WATCH_SWAP megabytes (5000) for
#   MEMORY_WATCH_GRACE seconds (60). A compiler whose heap is in swap by
#   gigabytes does not finish in any useful time.
#
# MEMORY_WATCH_INTERVAL is the seconds between two memory lines (60).
# MEMORY_WATCH_TICK is the seconds between two looks at the swap (10).
set -u

minutes=${1:?usage: with-memory-watch.sh <minutes> <command> [args...]}
shift
interval=${MEMORY_WATCH_INTERVAL:-60}
tick=${MEMORY_WATCH_TICK:-10}
swap_limit=${MEMORY_WATCH_SWAP:-5000}
grace=${MEMORY_WATCH_GRACE:-60}

# One line: the time, memory used and available, swap used, and the process
# holding the most.
memory_line() {
  free -m | awk -v at="$(date -u +%H:%M:%S)" '
    NR == 2 { used = $3; available = $7 }
    NR == 3 { swap = $3 }
    END { printf "memory %s: used %s MB, available %s MB, swap used %s MB", at, used, available, swap }'
  ps -eo rss=,comm= --sort=-rss | awk 'NR == 1 { printf "; largest: %s (%d MB)\n", $2, $1 / 1024 }'
}

# The five processes holding the most memory, with their arguments.
largest_processes() {
  echo 'The processes holding the most memory:'
  ps -eo rss=,args= --sort=-rss | head -5 | awk '{ printf "  %d MB  ", $1 / 1024; $1 = ""; print substr($0, 2, 150) }'
}

# Megabytes of swap in use.
swap_used() {
  awk '/^SwapTotal:/ { total = $2 } /^SwapFree:/ { free = $2 } END { print int((total - free) / 1024) }' /proc/meminfo
}

# Written by the watch when it stops the command for want of memory.
starved=$(mktemp)
rm -f "$starved"

# timeout puts the command in a process group of its own and signals the
# whole group, so everything the command started goes with it.
timeout --kill-after=30 "${minutes}m" "$@" &
command=$!

(
  since=0
  deep=0
  while sleep "$tick"; do
    since=$((since + tick))
    if [ "$since" -ge "$interval" ]; then
      memory_line
      since=0
    fi
    if [ "$(swap_used)" -ge "$swap_limit" ]; then
      deep=$((deep + tick))
    else
      deep=0
    fi
    if [ "$deep" -ge "$grace" ]; then
      # Say who holds the memory before they are gone.
      printf 'Stopping: swap in use has been at least %s MB for %s seconds.\n' "$swap_limit" "$grace"
      memory_line
      largest_processes
      : > "$starved"
      kill -TERM "$command" 2>/dev/null
      exit 0
    fi
  done
) &
watch=$!

status=0
wait "$command" || status=$?

kill "$watch" 2>/dev/null
wait "$watch" 2>/dev/null

if [ "$status" -ne 0 ]; then
  if [ -e "$starved" ]; then
    echo 'Stopped: the machine was running out of memory.'
  else
    case "$status" in
      124 | 137) printf 'Stopped: still running after %s minutes.\n' "$minutes" ;;
      *) printf 'Failed with status %s.\n' "$status" ;;
    esac
    memory_line
    largest_processes
  fi
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
rm -f "$starved"

exit "$status"
