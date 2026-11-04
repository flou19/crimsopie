#!/bin/sh
DATS="$1"
OUT="/tmp/crimsopie_out"
MAP="${DATS}.map"
ERR="/tmp/crimsopie_patscc_err.txt"
export PATSHOME="${PATSHOME:-/usr/lib/ats2-postiats-0.4.2}"
"$PATSHOME/bin/patscc" -DATS_MEMALLOC_LIBC -o "$OUT" "$DATS" -lm > /tmp/crimsopie_patscc_out.txt 2>"$ERR"
EC=$?
if [ "$EC" -eq 0 ]; then
  echo "patscc ok"
  exit 0
fi
if [ -f "$MAP" ]; then
  SRC=$(awk '/^src /{print $2; exit}' "$MAP")
  while IFS= read -r line || [ -n "$line" ]; do
    L=$(echo "$line" | sed -n 's/.*(line=\([0-9][0-9]*\).*/\1/p')
    if [ -n "$L" ] && [ -n "$SRC" ]; then
      PU8=$(awk -v L="$L" '$1+0==L+0 {print $2; exit}' "$MAP")
      if [ -z "$PU8" ]; then
        PU8=$(awk -v L="$L" '$1+0<=L+0 {p=$2} END{print p}' "$MAP")
      fi
      if [ -n "$PU8" ]; then
        MSG=$(echo "$line" | sed 's/.*error([^)]*):/error:/' | sed 's/.*error(3):/error:/')
        echo "${SRC}:${PU8}: ${MSG}"
      fi
    fi
  done < "$ERR"
else
  cat "$ERR"
fi
echo "patscc failed"
exit "$EC"
