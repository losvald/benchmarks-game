# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# mandelbrot-noout-naive.R
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

mandelbrot_noout_naive <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 200L
    n_mod8 = n %% 8L
    pads <- if (n_mod8) rep.int(0, 8L - n_mod8) else integer(0)
    p <- rep(as.integer(rep.int(2, 8) ^ (7:0)), length.out=n)

    cat("P4\n")
    cat(n, n, "\n")
    C <- matrix(0, n, n)
    for (y in 0:(n-1)) {
        C[, y] <- 2 * 0:(n-1) / n - 1.5 + 1i * (2 * y / n - 1)
    }

    m <- n
    Z <- 0                   # initialize Z to zero
    X <- array(0, c(m,m,20)) # initialize output 3D array
    for (k in 1:20) {        # loop with 20 iterations
        Z <- Z^2+C             # the central difference equation
          X[,,k] <- exp(-abs(Z)) # capture results
    }
}

if (!exists("i_am_wrapper"))
  mandelbrot_noout_naive(commandArgs(trailingOnly=TRUE))
