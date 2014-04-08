#!/bin/sh

# find-gcov.sh
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


if [[ $# -lt 1 ]]; then
    echo "$0 program args [...]" >&2
fi
SCRIPT=`readlink -f $0`
DIR=`dirname $SCRIPT`

prog="$1"
shift 1

PROF_DIR="$DIR/$prog-prof"
echo "$PROF_DIR/$prog.cpp.gcov"
