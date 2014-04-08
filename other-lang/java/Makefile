# Makefile
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

all: binarytrees fannkuchredux fasta fastaredux mandelbrot nbody reversecomplement spectralnorm
clean: clean-binarytrees clean-fannkuchredux clean-fasta clean-fastaredux clean-mandelbrot clean-nbody clean-regexdna clean-reversecomplement clean-spectralnorm

binarytrees:
	javac binarytrees.java
clean-binarytrees:
	rm -f binarytrees*.class

fannkuchredux:
	javac fannkuchredux.java
clean-fannkuchredux:
	rm -f fannkuchredux*.class

fasta:
	javac fasta.java
clean-fasta:
	rm -f fasta*.class

fastaredux:
	javac fastaredux.java
clean-fastaredux:
	rm -f fastaredux*.class

mandelbrot:
	javac mandelbrot.java
clean-mandelbrot:
	rm -f mandelbrot*.class

nbody:
	javac nbody.java
clean-nbody:
	rm -f nbody*.class NBodySystem.class Body.class

regexdna:
	javac regexdna.java
clean-regexdna:
	rm -f regexdna*.class ByteWrapper.class

reversecomplement:
	javac reversecomplement.java
clean-reversecomplement:
	rm -f reversecomplement*.class

spectralnorm:
	javac spectralnorm.java
clean-spectralnorm:
	rm -f spectralnorm*.class
