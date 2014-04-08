# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# spectralnorm-alt4.R
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

spectralnorm_alt4 <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 100L
    options(digits=10)

    eval_A <- function(i, j)
        return(if (eval_A_cache[[i, j]] != 0) eval_A_cache[[i, j]] else
            eval_A_cache[[i, j]] <<- 1 / ((i + j - 2) * (i + j - 1) / 2 + i))
    eval_A_times_u <- function(u) {
        #    eval_A_mat <- outer(seq(n), seq(n), FUN=eval_A)
        eval_A_mat <- matrix(0, n, n)
        for (i in 1:n)
            for (j in 1:n)
                eval_A_mat[[i, j]] <- eval_A(i, j)
        return(u %*% eval_A_mat)
    }
    eval_At_times_u <- function(u) {
        # eval_A_mat <- t(outer(seq(n), seq(n), FUN=eval_A))
        eval_A_mat <- matrix(0, n, n)
        for (i in 1:n)
            for (j in 1:n)
                eval_A_mat[[i, j]] <- eval_A(i, j)
        return(u %*% t(eval_A_mat))
    }
    eval_AtA_times_u <- function(u)
    eval_At_times_u(eval_A_times_u(u))

    eval_A_cache <- matrix(0, n, n)
    u <- rep(1, n)
    v <- rep(0, n)
    for (itr in seq(10)) {
        v <- eval_AtA_times_u(u)
        u <- eval_AtA_times_u(v)
    }

    cat(sqrt(sum(u * v) / sum(v * v)), "\n")
}

if (!exists("i_am_wrapper"))
    spectralnorm_alt4(commandArgs(trailingOnly=TRUE))
