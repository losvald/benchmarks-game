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
complement <- function(c)
    return(comp_map[[c]])

reversecomplement <- function(args) {
    in_filename = args[[1]]
    f <- file(in_filename, "r")
    while (length(s <- readLines(f, n=1, warn=FALSE))) {
        if (substr(s, 1, 1) == '>')
            cat(s, "\n", sep="")
        else {
	    s_len <- seq(1, nchar(s))
            codes <- substring(s, s_len, s_len)
            cat(paste(sapply(codes, complement), collapse=""), "\n",
                sep="")
        }
    }
    close(f)
}

if (!exists("i_am_wrapper"))
    reversecomplement(commandArgs(trailingOnly=TRUE))
