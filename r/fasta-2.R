# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# fasta-2.R
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

width <- 60L
myrandom_last <- 42L
myrandom <- function(m) {
    myrandom_last <<- (myrandom_last * 3877L + 29573L) %% 139968L
    return(m * myrandom_last / 139968)
}

alu <- paste(
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
    "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
    "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
    "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
    "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
    "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
    "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA",
    sep="", collapse="")

iub <- matrix(c(
    c(0.27, 'a'),
    c(0.12, 'c'),
    c(0.12, 'g'),
    c(0.27, 't'),
    c(0.02, 'B'),
    c(0.02, 'D'),
    c(0.02, 'H'),
    c(0.02, 'K'),
    c(0.02, 'M'),
    c(0.02, 'N'),
    c(0.02, 'R'),
    c(0.02, 'S'),
    c(0.02, 'V'),
    c(0.02, 'W'),
    c(0.02, 'Y')
), 2)

homosapiens <- matrix(c(
    c(0.3029549426680, 'a'),
    c(0.1979883004921, 'c'),
    c(0.1975473066391, 'g'),
    c(0.3015094502008, 't')
), 2)

repeat_fasta <- function(s, count) {
    chars <- strsplit(s, split="")[[1]]
    len <- nchar(s)
    s2 <- c(chars, chars[1:width])
    pos <- 1L
    while (count) {
	line <- min(width, count)
        next_pos <- pos + line
        cat(s2[pos:(next_pos - 1)], "\n", sep="")
        pos <- next_pos
        if (pos > len) pos <- pos - len
	count <- count - line
    }
}

random_fasta <- function(genelist, count) {
    psum = cumsum(genelist[1,])
    n = length(psum)
    while (count) {
	line = min(width, count) 
        rs <- double(line)
        for (i in 1:line)
          rs[[i]] <- myrandom(1)

	# Linear search (vectorized)
	inds <- 1:line
       	lo <- rep.int(1L, line)
	while (length(inds <- which(psum[lo] < rs)))
	    lo[inds] <- lo[inds] + 1L

	cat(genelist[2, lo], "\n", sep='')
	count <- count - line
    }
}

fasta_2 <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 1000L
    cat(">ONE Homo sapiens alu\n")
    repeat_fasta(alu, 2 * n)
    cat(">TWO IUB ambiguity codes\n")
    random_fasta(iub, 3L * n)
    cat(">THREE Homo sapiens frequency\n")
    random_fasta(homosapiens, 5L * n)
}

if (!exists("i_am_wrapper"))
    fasta_2(commandArgs(trailingOnly=TRUE))
