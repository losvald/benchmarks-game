#!/bin/bash
if [ $# -lt 1 ]; then
    echo "Usage: FILE ..." >&2
    exit 1
fi

for f in $@; do
    echo \
'/* ------------------------------------------------------------------ */
/* The Computer Language Shootout                                     */
/* http://shootout.alioth.debian.org/                                 */
/*                                                                    */
/* Contributed by Leo Osvald                                          */
/* ------------------------------------------------------------------ */
#!/bin/bash
tail +3 "$0" | R --slave --args $@; exit $?' > "$f"
done
