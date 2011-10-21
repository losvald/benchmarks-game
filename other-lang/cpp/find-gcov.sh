#!/bin/sh
if [[ $# -lt 1 ]]; then
    echo "$0 program args [...]" >&2
fi
SCRIPT=`readlink -f $0`
DIR=`dirname $SCRIPT`

prog="$1"
shift 1

PROF_DIR="$DIR/$prog-prof"
echo "$PROF_DIR/$prog.cpp.gcov"
