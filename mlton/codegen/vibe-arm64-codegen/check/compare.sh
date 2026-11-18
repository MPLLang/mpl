#!/usr/bin/env bash

# Compare two output directories of gen.sh: the same programs must have
# compiled, to the same assembly.  See README.
#
# usage: compare.sh <output dir> <output dir>

set -u

if [ $# -ne 2 ]; then
  echo >&2 "usage: $(basename "$0") <output dir> <output dir>"
  exit 1
fi

a=$(cd "$1" && pwd)
b=$(cd "$2" && pwd)

list () {
  (cd "$1" && find . -type f \( -name '*.s' -o -name '*.log' \) | sort)
}

if [ "$(list "$a")" != "$(list "$b")" ]; then
  echo "the directories don't have the same files:"
  diff <(list "$a") <(list "$b") | head -20
  exit 1
fi

# what compiled, and with what messages
logs () {
  (cd "$1" && find . -name '*.log' | sort | xargs cat)
}
if [ "$(logs "$a")" != "$(logs "$b")" ]; then
  echo "the logs differ"
  exit 1
fi

n=0
bad=0
for f in $(cd "$a" && find . -name '*.s' | sort); do
  n=$((n + 1))
  # The first line of a file is a comment with the version of the compiler.
  if ! cmp -s <(tail -n +2 "$a/$f") <(tail -n +2 "$b/$f"); then
    bad=$((bad + 1))
    [ $bad -le 20 ] && echo "differ: $f"
  fi
done

echo "$n assembly files, $bad differ"
[ $n -gt 0 ] && [ $bad = 0 ]
