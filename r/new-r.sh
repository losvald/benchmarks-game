#!/bin/bash
if [ $# -lt 1 ]; then
    echo "Usage: FILE ..." >&2
    exit 1
fi

for f in $@; do
    echo \
"$f <- function(args) {
    # TODO main
}

if (!exists(\"i_am_wrapper\"))
    $f""(commandArgs(trailingOnly=TRUE))" > "$f.R"
done
