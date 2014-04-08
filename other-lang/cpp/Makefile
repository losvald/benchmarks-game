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

all: pidigits
clean: clean-pidigits

PIDIGITS=pidigits
PIDIGITS_SRC=$(PIDIGITS).cpp
PIDIGITS_PROF_DIR=$(PIDIGITS)-prof
PIDIGITS_TF = -pipe -O3 -march=native -mfpmath=sse -msse2 -fopenmp -mfpmath=sse -msse2
PIDIGITS_TF2=$(PIDIGITS_TF) -fomit-frame-pointer
pidigits: $(PIDIGITS_SRC)
	g++ -o $(PIDIGITS)-tweaked $(PIDIGITS_TF2) $(PIDIGITS_SRC)

	g++ -o $(PIDIGITS)-gcov --coverage $(PIDIGITS_TF2) $(PIDIGITS_SRC)
	g++ -o $(PIDIGITS)-gprof -pg $(PIDIGITS_TF) $(PIDIGITS_SRC)
	[ -d $(PIDIGITS_PROF_DIR) ] || mkdir $(PIDIGITS_PROF_DIR)
	mv $(PIDIGITS)-gcov $(PIDIGITS_PROF_DIR)/
	mv $(PIDIGITS)-gprof $(PIDIGITS_PROF_DIR)/
	mv $(PIDIGITS).gcno $(PIDIGITS_PROF_DIR)/
clean-pidigits:
	rm -f $(PIDIGITS){,-tweaked}
	rm -f $(PIDIGITS_PROF_DIR)/$(PIDIGITS)-{gcov,gprof}
	rm -f $(PIDIGITS_PROF_DIR)/$(PIDIGITS).{gcda,gcno,out}
