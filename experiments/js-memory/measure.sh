#!/bin/sh
# Print what the compiler's own statistics say for each library of this
# experiment: the most memory it had in use, and the most it kept alive.
# For per-module, also the closure types that held the most at the last
# sample of its heap profile.
set -u
cd "$(dirname "$0")"
for stats in *.rts; do
  [ -f "$stats" ] || { echo "no statistics were written"; break; }
  printf '%-22s %s; %s\n' "${stats%.rts}" \
    "$(grep 'total memory in use' "$stats" | sed 's/^ *//')" \
    "$(grep 'maximum residency' "$stats" | sed 's/^ *//')"
done
for profile in *.hp; do
  [ -f "$profile" ] || continue
  echo "heap profile $profile: the largest closure types at the last sample"
  awk '
    /^BEGIN_SAMPLE/ { delete bytes; inside = 1; next }
    /^END_SAMPLE/ { inside = 0; next }
    inside { bytes[$1] = $2 }
    END { for (type in bytes) printf "%14d  %s\n", bytes[type], type }
  ' "$profile" | sort -rn | head -12
done
