#!/bin/sh
# Usage: patscc_map.sh <file.dats> [extra patscc args...]
DATS=""
ARGS=""
for a in "$@"; do
  case "$a" in
    *.dats) DATS="$a"; ARGS="$ARGS $a" ;;
    *) ARGS="$ARGS $a" ;;
  esac
done
if [ -z "$DATS" ]; then
  echo "usage: patscc_map.sh <file.dats> ..."
  exit 2
fi
MAP="${DATS}.map"
BASE=$(basename "$DATS")
ERR=$(mktemp)
OUT=$(mktemp)
# shellcheck disable=SC2086
${PATSHOME}/bin/patscc $ARGS >"$OUT" 2>"$ERR"
EC=$?
if [ -f "$MAP" ]; then
  SRC=$(awk '/^src /{print $2; exit}' "$MAP")
  while IFS= read -r line || [ -n "$line" ]; do
    # ATS format: path: pos(line=N, offs=M)
    L=$(echo "$line" | sed -n 's/.*(line=\([0-9][0-9]*\).*/\1/p')
    if [ -n "$L" ]; then
      PU8=$(awk -v L="$L" '$1+0==L+0 {print $2; exit}' "$MAP")
      if [ -z "$PU8" ]; then
        PU8=$(awk -v L="$L" '$1+0<=L+0 {p=$2} END{print p}' "$MAP")
      fi
      echo "$line"
      if [ -n "$PU8" ] && [ -n "$SRC" ]; then
        echo "  [source map] ${BASE}:${L} -> ${SRC}:${PU8}"
      fi
    else
      echo "$line"
    fi
  done < "$ERR"
else
  cat "$ERR"
fi
cat "$OUT"
rm -f "$OUT" "$ERR"
exit $EC
