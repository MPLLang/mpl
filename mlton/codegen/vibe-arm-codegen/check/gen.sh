#!/usr/bin/env bash

# Write the assembly that the vibe-arm codegen outputs for a corpus of
# programs, under each of its configurations.  See README.
#
# usage: gen.sh <build tree> <output dir> [<parallel-ml-bench dir>]
#
# environment:
#   JOBS   how many compilations to run at once (default 8)
#   ONLY   only the programs of one set: reg, ex, pmb or rare

set -u

if [ $# -lt 2 ]; then
  echo >&2 "usage: $(basename "$0") <build tree> <output dir> [<parallel-ml-bench dir>]"
  exit 1
fi

here=$(cd "$(dirname "$0")" && pwd)
src=$(cd "$here/../../../.." && pwd)
tree=$(cd "$1" && pwd)
mkdir -p "$2"
out=$(cd "$2" && pwd)
pmb=${3:-}

if [ ! -x "$tree/bin/mpl" ]; then
  echo >&2 "no bin/mpl in $tree"
  exit 1
fi

# A target that is the host, except that its OS is Linux: enough to output
# (but not to assemble) what the codegen outputs for Linux.
targets="$tree/lib/mlton/targets"
if [ ! -d "$targets/fakelinux" ]; then
  cp -R "$targets/self" "$targets/fakelinux"
  echo linux > "$targets/fakelinux/os"
fi

all="plain sym linux"
most="plain"
rest="sym linux"
jobs="$out/jobs.txt"
: > "$jobs"

# add <set> <name> <source> <configs>
add () {
  for c in $4; do
    echo "$1|$2|$3|$c" >> "$jobs"
  done
}

# The regression suite and the benchmarks under both register allocators,
# and every eighth of them under the other configurations; the examples
# and rare.sml under every configuration.
k=0
for f in "$src"/regression/*.sml; do
  k=$((k + 1))
  add reg "$(basename "$f" .sml)" "$f" "$most"
  [ $((k % 8)) = 0 ] && add reg "$(basename "$f" .sml)" "$f" "$rest"
done
for d in "$src"/examples/src/*/; do
  add ex "$(basename "$d")" "${d}sources.mlb" "$all"
done
if [ -n "$pmb" ]; then
  k=0
  for d in "$pmb"/mpl/bench/*/; do
    n=$(basename "$d")
    [ -f "$d$n.mlb" ] || continue
    k=$((k + 1))
    add pmb "$n" "$d$n.mlb" "$most"
    [ $((k % 8)) = 0 ] && add pmb "$n" "$d$n.mlb" "$rest"
  done
fi
add rare rare "$here/rare.sml" "$all"

run () {
  IFS='|' read -r set name file cfg <<< "$1"
  flags=(-codegen vibe-arm)
  case $cfg in
    plain) ;;
    sym) flags+=(-vibe-arm-symbols true) ;;
    linux) flags+=(-target fakelinux) ;;
  esac
  case $set in
    reg) ;;
    ex) flags+=(-default-type int64 -default-type word64) ;;
    pmb) flags+=(-default-type int64 -default-type word64
                 -mlb-path-var 'COMPAT mpl') ;;
    rare) flags+=(-default-type int64 -default-type word64
                  -default-ann 'allowPrim true' -default-ann 'allowFFI true') ;;
  esac
  d="$out/$cfg/$set"
  mkdir -p "$d"
  # -stop g: output the assembly (and C) files, and don't assemble them
  "$tree/bin/mpl" "${flags[@]}" -stop g -output "$d/$name" "$file" \
    > "$d/$name.log" 2>&1
  echo "exit $?" >> "$d/$name.log"
}
export -f run
export tree out

grep "^${ONLY:-}" "$jobs" | tr '\n' '\0' \
  | xargs -0 -P "${JOBS:-8}" -I{} bash -c 'run "$@"' _ {}

echo "$(grep -c "^${ONLY:-}" "$jobs") compilations," \
     "$(find "$out" -name '*.log' -exec grep -L '^exit 0$' {} + | wc -l | tr -d ' ') failed," \
     "$(find "$out" -name '*.s' | wc -l | tr -d ' ') assembly files in $out"
