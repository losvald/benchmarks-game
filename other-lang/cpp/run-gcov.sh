#!/bin/sh
if [[ $# -lt 1 ]]; then
    echo "$0 program args [...]" >&2
fi
SCRIPT=`readlink -f $0`
DIR=`dirname $SCRIPT`
prog="$1"
shift 1

PROF_DIR="$DIR/$prog-prof"
src="$prog.cpp"
if [[ ! -e "$DIR/$src" ]]; then
    echo "Source file $src does not exist" >&2
    exit 1
fi

OLDDIR=`pwd`
cd "$PROF_DIR"

# Run gconv version of the program
prog_gcov="$prog-gcov"
out_filepath="$PROF_DIR/$prog.out"
echo "$PROF_DIR/$prog_gcov $@"
rm -f "$out_filepath"
eval "$PROF_DIR/$prog_gcov $@ > $out_filepath"
mv "$DIR/$prog.gcda" "$PROF_DIR"

# Run gconv on source
[[ -L "$src" ]] || ln -s "$DIR/$src" "$src"
gcov "$src" > /dev/null
find . -regex '.*\.gcov' | grep -v "$src.gcov" | xargs rm -f

cd $OLDDIR
