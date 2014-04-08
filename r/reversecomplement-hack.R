# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# reversecomplement-hack.R
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
    in_filename = args[[1]]
    f <- file(in_filename, "r")
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
