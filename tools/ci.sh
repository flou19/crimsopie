#!/bin/bash
set -e
make clean
make
make test
make benchmark || true
make release
echo "CI PASS"
