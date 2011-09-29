# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

lim <- 2
iter <- 50

mandelbrot <- function(args) {
    n = ifelse(length(args), as.integer(args[[1]]), 200L)
    cat("P4\n")
    cat(n, n, "\n")
    bin_con <- pipe("cat", "wb")
    for (y in 0:(n-1)) {
        c <- 2 * 0:(n-1) / n - 1.5 + 1i * (2 * y / n - 1)
        z <- rep(0+0i, n)
        # for (i in 1:iter) # slower
        #     z <- z * z + c
        i <- 0L
        while (i < iter) {
            z <- z * z + c
            i <- i + 1L
        }
        bits <- as.integer(abs(z) <= lim)
        #print(bits)
        p <- rep(rep.int(2, 8) ^ (7:0), length.out=n)
        bytes <- as.raw(colSums(matrix(c(bits * p, rep(0, 8 - n %% 8)), 8)))
        writeBin(bytes, bin_con)
        flush(bin_con)
    }
}

mandelbrot(commandArgs(trailingOnly=TRUE))
