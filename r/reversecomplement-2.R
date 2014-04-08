# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

# reversecomplement-2.R
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

reversecomplement_2 <- function(args) {
    f <- file(args[[1]], "r")
    lines <- readLines(f)
    for (i in 1:length(lines)) {
        codes <- strsplit(lines[[i]], split="")[[1]]
        if (codes[[1]] == '>')
            cat(lines[[i]], "\n", sep="")
        else {
            cat(paste(comp_map[codes], collapse=""), "\n",
                sep="")
        }
    }
    close(f)
}

if (!exists("i_am_wrapper"))
    reversecomplement_2(commandArgs(trailingOnly=TRUE))
