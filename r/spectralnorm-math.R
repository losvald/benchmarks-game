# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# spectralnorm-math.R
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

spectralnorm_math <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 100L
    options(digits=10)

    eval_A <- function(i, j) 1 / ((i + j - 2) * (i + j - 1) / 2 + i)

    m <- outer(seq(n), seq(n), FUN=eval_A)
    cat(sqrt(max(eigen(t(m) %*% m)$val)), "\n")
}

if (!exists("i_am_wrapper"))
    spectralnorm_math(commandArgs(trailingOnly=TRUE))
