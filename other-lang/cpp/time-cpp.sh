#!/bin/sh
echo "$@"
if [[ $# -lt 1 ]]; then
    echo "Missing run command" >&2
fi
cmd=$1
shift 1
[[ $cmd != "/" ]] && cmd="./$cmd"
echo "$cmd $@"
( time ( eval "$cmd $@" > /dev/null 2>&1 ) 2>&1 ) | tail -n -3
