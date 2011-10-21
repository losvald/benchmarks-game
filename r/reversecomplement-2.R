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
    f <- file(args[[1]], "r")
    lines <- readLines(f)
    for (i in 1:length(lines)) {
        if (substr(lines[[i]], 1, 1) == '>')
            cat(lines[[i]], "\n", sep="")
        else {
	    sq <- seq(1L, nchar(lines[[i]]))
            codes <- substring(lines[[i]], sq, sq)
            cat(paste(sapply(codes, complement), collapse=""), "\n",
                sep="")
        }
    }
    close(f)
}

if (!exists("i_am_wrapper"))
    reversecomplement(commandArgs(trailingOnly=TRUE))
