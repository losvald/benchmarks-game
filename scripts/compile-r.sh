#!/bin/sh

# compile-r.sh
#
# Copyright (C) 2011 Leo Osvald (losvald@purdue.edu)
#
# This file is part of R Shootout.
#
# R Shootout is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# R Shootout is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with R Shootout. If not, see <http://www.gnu.org/licenses/>.

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
