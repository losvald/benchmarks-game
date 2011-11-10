#!/bin/bash
if [ $# -lt 1 ]; then
    echo "Usage: $0 file ..." >&2
    exit 1
fi

grep -vE '^[[:space:]]*#' $@ | sed -r -e 's/#.*//g' -e 's/[[:space:]]+$//g'
