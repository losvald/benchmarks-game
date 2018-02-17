all: pidigits
clean: clean-pidigits

PIDIGITS=pidigits
PIDIGITS_SRC=$(PIDIGITS).cpp
PIDIGITS_PROF_DIR=$(PIDIGITS)-prof
PIDIGITS_TF = -std=c++14 -pipe -O3 -march=native -mfpmath=sse -msse2 -fopenmp -mfpmath=sse -msse2
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
