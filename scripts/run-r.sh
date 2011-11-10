#!/bin/bash

if [ $# -lt 2 ]; then
    echo "Usage: r-path r-source [args ...]" >&2
    exit 1
fi

r="$1"
rsrc="$2"
shift 2

if [ -z `which "$r" 2> /dev/null` ]; then
    echo "Unknown R: $r $@" >&2
    exit 1
fi

cmd="$r --slave -f $rsrc --args $@"
echo "$cmd" >&2
eval "$cmd"
