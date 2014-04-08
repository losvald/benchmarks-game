# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# mandelbrot-naive.R
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

lim <- 2
iter <- 50

mandelbrot_naive <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 200L
    cat("P4\n")
    cat(n, n, "\n")
    bin_con <- pipe("cat", "wb")
    for (y in 0:(n-1)) {
        bits <- 0L
        x <- 0L
        while (x < n) {
            c <- 2 * x / n - 1.5 + 1i * (2 * y / n - 1)
            z <- 0+0i
            i <- 0L
            while (i < iter && abs(z) <= lim) {
                z <- z * z + c
                i <- i + 1L
            }
            bits <- 2L * bits + as.integer(abs(z) <= lim)
            if ((x <- x + 1L) %% 8L == 0) {
                writeBin(as.raw(bits), bin_con)
                bits <- 0L
            }
        }
        xmod <- x %% 8L
        if (xmod)
            writeBin(as.raw(bits * as.integer(2^(8L - xmod))), bin_con)
        flush(bin_con)
    }
}

if (!exists("i_am_wrapper"))
    mandelbrot_naive(commandArgs(trailingOnly=TRUE))
