#!/bin/bash
# Quality checks for markdown-table-wrap — run before commits

set -e

cd "$(dirname "$0")/.."

make check
make clean
