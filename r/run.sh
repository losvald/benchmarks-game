#!/bin/bash
#tail -n +3 "$0" |
if [ $# -lt 1 ]; then
    echo "Usage: r-source [args ...]" >&2
    exit 1
fi
rsrc="$1"
shift 1
R --slave -f "$rsrc" --args $@
