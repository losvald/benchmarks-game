# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

gen_freq <- function(seq, frame) {
    frame <- frame - 1L
    ns <- nchar(seq) - frame
    h <- new.env(emptyenv(), hash=TRUE)
    for (i in 1:ns) {
        subseq = substr_fast(seq, i, i + frame)
	if (exists(subseq, h, inherits=FALSE))
	    cnt <- get(subseq, h, inherits=FALSE)
	else
	    cnt <- 0L
	assign(subseq, cnt + 1L, h)
    }
    return(sapply(ls(h), function(k) get(k, h, inherits=FALSE)))
}


substr_fast <- function (x, start, stop) {
      if (!is.character(x))
          x <- as.character(x)
     .Internal(substr_fast(x, as.integer(start), as.integer(stop)))
}

sort_seq <- function(seq, len) {
    fs <- gen_freq(seq, len)
    seqs <- names(fs)
    inds <- order(-fs, seqs)
    cat(paste.(seqs[inds], 100 * fs[inds] / sum(fs), collapse="\n", digits=3),
        "\n")
}

find_seq <- function(seq, s) {
    freqs <- gen_freq(seq, nchar(s))
    if (s %in% names(freqs))
        return(freqs[[s]])
    return(0L)
}

knucleotide_hack2 <- function(args) {
    in_filename = args[[1]]
    f <- file(in_filename, "r")
    while (length(line <- readLines(f, n=1, warn=FALSE))) {
        first_char <- substr(line, 1L, 1L)
        if (first_char == '>' || first_char == ';')
            if (substr(line, 2L, 3L) == 'TH')
                break
    }

    n <- 0L
    cap <- 8L
    str_buf <- character(cap)
    while (length(line <- scan(f, what="", nmax=1, quiet=TRUE))) {
        first_char <- substr(line, 1L, 1L)
        if (first_char == '>' || first_char == ';')
            break
        n <- n + 1L
        # ensure O(N) resizing (instead of O(N^2))
        str_buf[[cap <- if (cap < n) 2L * cap else cap]] <- ""
        str_buf[[n]] <- line
    }
    length(str_buf) <- n
    close(f)
    seq <- paste(str_buf, collapse="")

    sort_seq(seq, 2)
    for (s in c("GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"))
        cat(find_seq(seq, tolower(s)), sep="\t", s, "\n")
}

paste. <- function (..., digits=16, sep=" ", collapse=NULL) {
    args <- list(...)
    if (length(args) == 0)
        if (length(collapse) == 0) character(0)
        else ""
    else {
        for(i in seq(along=args))
            if(is.numeric(args[[i]])) 
                args[[i]] <- as.character(round(args[[i]], digits))
            else args[[i]] <- as.character(args[[i]])
        .Internal(paste(args, sep, collapse))
    }
}

if (!exists("i_am_wrapper"))
    knucleotide_hack2(commandArgs(trailingOnly=TRUE))
