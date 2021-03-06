# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

codes <- c(
    "A", "C", "G", "T", "U", "M", "R", "W", "S", "Y", "K", "V", "H", "D", "B",
    "N")
complements <- c(
    "T", "G", "C", "A", "A", "K", "Y", "W", "S", "R", "M", "B", "D", "H", "V",
    "N")
comp_map <- NULL
comp_map[codes] <- complements
comp_map[tolower(codes)] <- complements

substr_fast <- function (x, start, stop) {
      if (!is.character(x))
          x <- as.character(x)
     .Internal(substr_fast(x, as.integer(start), as.integer(stop)))
}

reversecomplement <- function(args) {
    f <- file(if (args[[1]] != "0") args[[1]] else "stdin", "r")
    while (length(s <- readLines(f, n=1, warn=FALSE))) {
	range <- seq(1, nchar(s))
        codes <- substr_fast(s, range, range)
        if (codes[[1]] == '>')
            cat(s, "\n", sep="")
        else {
            cat(paste(comp_map[codes], collapse=""), "\n", sep="")
        }
    }
    close(f)
}

if (!exists("i_am_wrapper"))
    reversecomplement(commandArgs(trailingOnly=TRUE))
