# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# binarytrees-2.R
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

tree <- function(item, depth) {
    if (depth == 0L)
        return(c(item, NA, NA))
    return(list(item,
        tree(2L * item - 1L, depth - 1L),
        tree(2L * item, depth - 1L)))
}

check <- function(tree)
    if(is.na(tree[[2]][[1]])) tree[[1]] else tree[[1]] + check(tree[[2]]) - check(tree[[3]])

binarytrees_2 <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 10L

    min_depth <- 4
    max_depth <- max(min_depth + 2, n)
    stretch_depth <- max_depth + 1

    cat(sep="", "stretch tree of depth ", stretch_depth, "\t check: ",
        check(tree(0, stretch_depth)), "\n")

    long_lived_tree <- tree(0, max_depth)

    for (depth in seq(min_depth, max_depth, 2)) {
        iterations <- as.integer(2^(max_depth - depth + min_depth))
        check_sum <- sum(sapply(
                1:iterations, 
                function(i) check(tree(i, depth)) + check(tree(-i, depth))))
        cat(sep="", iterations * 2L, "\t trees of depth ", depth, "\t check: ",
            check_sum, "\n")
    }

    cat(sep="", "long lived tree of depth ", max_depth, "\t check: ", 
        check(long_lived_tree), "\n")
}

if (!exists("i_am_wrapper"))
    binarytrees_2(commandArgs(trailingOnly=TRUE))
