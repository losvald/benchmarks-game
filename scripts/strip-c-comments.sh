#!/bin/bash
script=`readlink -f $0`
dir=`dirname $script`

grep -vE '^[[:space:]]+#' $@ | eval "$dir/_strip-c-comments.py $@" | sed 's/[[:space:]]+//g'
