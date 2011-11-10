#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: c-source [args ...]" >&2
    exit 1
fi

cmd=$1
shift 1
[[ $cmd != "/"* ]] && cmd="./$cmd"
echo "$cmd $@" >&2
eval "$cmd" $@
