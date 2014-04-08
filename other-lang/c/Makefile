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

all: binarytrees fannkuchredux fasta fastaredux knucleotide mandelbrot nbody regexdna reversecomplement spectralnorm
clean: clean-binarytrees clean-fannkuchredux clean-fasta clean-fastaredux clean-knucleotide clean-mandelbrot clean-nbody clean-regexdna clean-reversecomplement clean-spectralnorm

BINARYTREES=binarytrees
BINARYTREES_SRC=$(BINARYTREES).c
binarytrees: $(BINARYTREES_SRC)
	gcc -o $(BINARYTREES) -O2 -I/usr/include/apr-1 -lapr-1 $(BINARYTREES_SRC)
	gcc -o $(BINARYTREES)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native -fopenmp -D_FILE_OFFSET_BITS=64 -I/usr/include/apr-1 -lapr-1 $(BINARYTREES_SRC)
clean-binarytrees:
	rm -f $(BINARYTREES){,-tweaked}

FANNKUCHREDUX=fannkuchredux
FANNKUCHREDUX_SRC=$(FANNKUCHREDUX).c
fannkuchredux: $(FANNKUCHREDUX_SRC)
	gcc -o $(FANNKUCHREDUX) -O2 $(FANNKUCHREDUX_SRC)
	gcc -o $(FANNKUCHREDUX)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native $(FANNKUCHREDUX_SRC)
clean-fannkuchredux:
	rm -f $(FANNKUCHREDUX){,-tweaked}

FASTA=fasta
FASTA_SRC=$(FASTA).c
fasta: $(FASTA_SRC)
	gcc -o $(FASTA) -O2 $(FASTA_SRC)
	gcc -o $(FASTA)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -mfpmath=sse -msse3 $(FASTA_SRC)
clean-fasta:
	rm -f $(FASTA){,-tweaked}

FASTAREDUX=fastaredux
FASTAREDUX_SRC=$(FASTAREDUX).c
fastaredux: $(FASTAREDUX_SRC)
	gcc -o $(FASTAREDUX) -std=c99 -O2 $(FASTAREDUX_SRC)
	gcc -o $(FASTAREDUX)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -mfpmath=sse -msse3 $(FASTAREDUX_SRC)
clean-fastaredux:
	rm -f $(FASTAREDUX){,-tweaked}

KNUCLEOTIDE=knucleotide
KNUCLEOTIDE_SRC=$(KNUCLEOTIDE).c
KNUCLEOTIDE_INCLUDE = Include/simple_hash3.h
knucleotide: $(KNUCLEOTIDE_SRC)
	gcc -o $(KNUCLEOTIDE) -O3 -Wall -std=c99 -include $(KNUCLEOTIDE_INCLUDE) $(KNUCLEOTIDE_SRC)
	gcc -o $(KNUCLEOTIDE)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -include $(KNUCLEOTIDE_INCLUDE) $(KNUCLEOTIDE_SRC)
clean-knucleotide:
	rm -f $(KNUCLEOTIDE){,-tweaked}

MANDELBROT=mandelbrot
MANDELBROT_SRC=$(MANDELBROT).c
mandelbrot: $(MANDELBROT_SRC)
	gcc -o $(MANDELBROT) -O2 $(MANDELBROT_SRC)
	gcc -o $(MANDELBROT)-tweaked -pipe -O3 -fomit-frame-pointer -march=native -mfpmath=sse -msse2 -fopenmp -mfpmath=sse -msse2 $(MANDELBROT_SRC)
clean-mandelbrot:
	rm -f $(MANDELBROT){,-tweaked}

NBODY=nbody
NBODY_SRC=$(NBODY).c
nbody: $(NBODY_SRC)
	gcc -o $(NBODY)-tweaked $(NBODY_SRC) -pipe -Wall -O3 -fomit-frame-pointer -march=native -lm -mfpmath=sse -msse3
clean-nbody:
	rm -f $(NBODY){,-tweaked}

REGEXDNA=regexdna
REGEXDNA_SRC=$(REGEXDNA).c
regexdna: $(REGEXDNA_SRC)
	gcc -o $(REGEXDNA) $(REGEXDNA_SRC) -O2 -lpcre
	gcc -o $(REGEXDNA)-tweaked $(REGEXDNA_SRC) -pipe -Wall -O3 -fomit-frame-pointer -march=native -lpcre -fopenmp
clean-regexdna:
	rm -f $(REGEXDNA){,-tweaked}

REVERSECOMPLEMENT=reversecomplement
REVERSECOMPLEMENT_SRC=$(REVERSECOMPLEMENT).c
reversecomplement: $(REVERSECOMPLEMENT_SRC)
	gcc -o $(REVERSECOMPLEMENT) -O2 $(REVERSECOMPLEMENT_SRC)
	gcc -o $(REVERSECOMPLEMENT)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 $(REVERSECOMPLEMENT_SRC)
clean-reversecomplement:
	rm -f $(REVERSECOMPLEMENT){,-tweaked}

SPECTRALNORM=spectralnorm
SPECTRALNORM_SRC=$(SPECTRALNORM).c
SPECTRALNORM_ALT=$(SPECTRALNORM)-alt
SPECTRALNORM_ALT_SRC=$(SPECTRALNORM_ALT).c
spectralnorm: $(SPECTRALNORM_SRC) $(SPECTRALNORM_ALT_SRC)
	gcc -o $(SPECTRALNORM) -O2 $(SPECTRALNORM_SRC) -lm
	gcc -o $(SPECTRALNORM)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native -Os -fopenmp -mfpmath=sse -msse2 -lm $(SPECTRALNORM_SRC)
	gcc -o $(SPECTRALNORM_ALT) -O2 $(SPECTRALNORM_ALT_SRC) -lm
	gcc -o $(SPECTRALNORM_ALT)-tweaked -pipe -Wall -O3 -fomit-frame-pointer -march=native -Os -fopenmp -mfpmath=sse -msse2 -lm $(SPECTRALNORM_ALT_SRC)
clean-spectralnorm:
	rm -f $(SPECTRALNORM){,-tweaked} $(SPECTRALNORM_ALT){,-tweaked}
