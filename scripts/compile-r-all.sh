#!/bin/bash
script=`readlink -f $0`
dir=`dirname $script`

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 R FILE ..." >&2
    exit 1
fi

cmd="$dir/compile.sh $1"
find "$dir" -regex '\(^\|.*/\)[^_]+.R$' |  eval "xargs $cmd"
