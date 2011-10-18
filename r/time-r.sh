#!/bin/sh
script=`readlink -f $0`
dir=`dirname $script`

cmd="$dir/run.sh $@"
echo "$cmd"
( time ( eval "$cmd" > /dev/null 2>&1 ) 2>&1 ) | tail -n 3
