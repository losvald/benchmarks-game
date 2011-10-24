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

reversecomplement <- function(args) {
    f <- file(args[[1]], "r")
    while (length(s <- readLines(f, n=1, warn=FALSE))) {
        if (substr(s, 1, 1) == '>')
            cat(s, "\n", sep="")
        else {
	    s_len <- nchar(s)
	    sq <- 1:s_len
	    codes <- substring(s, sq, sq)
	    for (j in 1:s_len)
	        codes[[j]] <- comp_map[[codes[[j]]]]
            cat(paste(codes, collapse=""), "\n", sep="")
        }
    }
    close(f)
}

if (!exists("i_am_wrapper"))
    reversecomplement(commandArgs(trailingOnly=TRUE))
