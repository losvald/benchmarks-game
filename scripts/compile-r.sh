#!/bin/sh

source ~losvald/scripts/bash-common.sh

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 R R_SOURCE_FILE ..." >&2
    exit 1
fi

r=$1
shift 1

for f in $@; do
    if [ -z "$f" ]; then
	continue
    fi
    ext=`get_ext "$f"`
    if [ $ext == 'txt' ]; then
	cmp_list=`sed -r 's/.*\/(.*)_.*/\1.R/' $f | paste -sd ' '`
    else
	cmp_list="$f"
    fi
#    echo "compiler::setCompilerOptions(optimize=3);"
    for cmp in $cmp_list; do
	echo "compiler::cmpfile('$cmp');"
    done | eval "$r --slave --no-save"
done
