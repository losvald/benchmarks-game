#!/bin/sh
script=`readlink -f $0`
dir=`dirname $script`

cmd="$dir/run-r.sh"
( ulimit -t 1; eval "$cmd $@" ) >/dev/null 2>&1
/usr/bin/time -p "$cmd" $@ >/dev/null
