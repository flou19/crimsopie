#!/bin/sh
DATS="$1"
LINE="$2"
MAP="${DATS}.map"
if [ ! -f "$MAP" ]; then
  echo "no map: $MAP"
  exit 1
fi
SRC=$(awk '/^src /{print $2; exit}' "$MAP")
PU8=$(awk -v L="$LINE" '$1==L {print $2; exit}' "$MAP")
if [ -z "$PU8" ]; then
  PU8=$(awk -v L="$LINE" '$1<=L {p=$2} END{print p}' "$MAP")
fi
if [ -n "$PU8" ] && [ -n "$SRC" ]; then
  echo "${SRC}:${PU8}: mapped from ${DATS}:${LINE}"
else
  echo "no mapping for ${DATS}:${LINE}"
  exit 1
fi
