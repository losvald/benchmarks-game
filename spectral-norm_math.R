# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

spectral_norm_math <- function(n) {
    eval_A <- function(i, j) 1 / ((i + j - 2) * (i + j - 1) / 2 + i)

    m <- outer(seq(n), seq(n), FUN=eval_A)
    cat(sqrt(max(eigen(t(m) %*% m)$val)), "\n")
}

options(digits=10)
args <- commandArgs(trailingOnly=TRUE)
if (length(args))
    spectral_norm_math(as.integer(args)[[1]])
