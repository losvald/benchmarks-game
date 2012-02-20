#!/bin/sh
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 COMMAND [ARG ...]" >&2
    exit 1
fi

script=`readlink -f $0`
dir=`dirname $script`

cmd="$dir/run-c.sh"

( /opt/r/tracer/mytime "$cmd" $@ >/dev/null ) 2>&1 | sed -n '1p;3p;4p;5p'
#( ulimit -t 1; eval "$cmd" $@ ) >/dev/null 2>&1
#( /usr/bin/time -p "$cmd" $@ >/dev/null ) 2>&1
