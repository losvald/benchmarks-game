# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

gen_freq <- function(seq, frame) {
    seq_len <- nchar(seq)
    ns <- seq_len - frame + 1L
    h <- new.env(emptyenv(), hash=TRUE)
    subseqs <- substr_fast(seq, 1:ns, frame:seq_len)
    for (i in 1:ns) {
	if (exists(subseqs[[i]], h, inherits=FALSE))
	    cnt <- get(subseqs[[i]], h, inherits=FALSE)
	else
	    cnt <- 0L
	assign(subseqs[[i]], cnt + 1L, h)
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

knucleotide_hack <- function(args) {
    f <- file(if (args[[1]] != "0") args[[1]] else "stdin", "r")
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

    for (frame in 1:2)
        sort_seq(seq, frame)
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
    knucleotide_hack(commandArgs(trailingOnly=TRUE))
