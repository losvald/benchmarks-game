#!/bin/bash

script=`readlink -f $0`
dir=`dirname $script`

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 R-version TXT ..." >&2
  exit 1
fi

r=$1
shift 1

if [ -z `which "$r" 2> /dev/null` ]; then
    echo "Unknown R: $r $@" >&2
    exit 1
fi

for f in $@; do
    f_dir=`dirname "$f"`
    cat $f | while read line; do
	if [[ -z "$line" ]]; then
	    continue
	elif [[ "$line" == \#* ]]; then
	    continue
	fi
	cmd="$dir/time-r.sh $r $f_dir/$line"
	eval "$cmd"
    done
done
