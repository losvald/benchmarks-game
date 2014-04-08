# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# regexdna.R
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

pattern1 <- c(
    "agggtaaa|tttaccct",
    "[cgt]gggtaaa|tttaccc[acg]",
    "a[act]ggtaaa|tttacc[agt]t",
    "ag[act]gtaaa|tttac[agt]ct",
    "agg[act]taaa|ttta[agt]cct",
    "aggg[acg]aaa|ttt[cgt]ccct",
    "agggt[cgt]aa|tt[acg]accct",
    "agggta[cgt]a|t[acg]taccct",
    "agggtaa[cgt]|[acg]ttaccct")

pattern2 <- matrix(c(
        c("B", "(c|g|t)"),
        c("D", "(a|g|t)"),
        c("H", "(a|c|t)"),
        c("K", "(g|t)"),
        c("M", "(a|c)"),
        c("N", "(a|c|g|t)"),
        c("R", "(a|g)"),
        c("S", "(c|g)"),
        c("V", "(a|c|g)"),
        c("W", "(a|t)"), 
        c("Y", "(c|t)")
), ncol=2, byrow=TRUE)

match_count <- function(ms) {
    l <- length(ms[[1]])
    fst <- ms[[1]][[1]]
    return(if (l > 1) l else if (fst != -1L) fst else 0)
}

regexdna <- function(args) {
    in_filename = args[[1]]
    f <- file(in_filename, "r")
    str <- paste(c(readLines(f), ""), collapse="\n")
    close(f)

    len1 <- nchar(str)
    str <- gsub(">.*\n|\n", "", str, perl=TRUE, useBytes=TRUE)
    len2 <- nchar(str)

    for (pat in pattern1)
        cat(pat, match_count(gregexpr(pat, str, useBytes=TRUE)), "\n")

    for (i in 1:nrow(pattern2))
        str <- gsub(pattern2[[i, 1]], pattern2[[i, 2]], str, perl=TRUE, 
                    useBytes=TRUE)

    cat("", len1, len2, nchar(str), sep="\n")
}

if (!exists("i_am_wrapper"))
    regexdna(commandArgs(trailingOnly=TRUE))
