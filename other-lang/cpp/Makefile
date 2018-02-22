all: pidigits
clean: clean-pidigits

PIDIGITS=pidigits
PIDIGITS_SRC=$(PIDIGITS).cpp
PIDIGITS_PROF_DIR=$(PIDIGITS)-prof
PIDIGITS_TF = -std=c++14 -pipe -O3 -march=native -mfpmath=sse -msse2 -fopenmp -mfpmath=sse -msse2
PIDIGITS_TF2=$(PIDIGITS_TF) -fomit-frame-pointer
pidigits: $(PIDIGITS_SRC)
	g++ -o $(PIDIGITS)-tweaked $(PIDIGITS_TF2) $(PIDIGITS_SRC)
	( ulimit -s 131072; ./pidigits-tweaked 3000 ) | grep -o '^[0-9]\{10\}' | tr -d '\n' | shasum | grep -qm 1 '0f68afe78afb3fad9533245b38fe4a709d235199'
	# ./pidigits-tweaked 1000 | grep -o '^[0-9]\{10\}' | tr -d '\n' | shasum | grep -qm 1 'a03730e0b961b25376bb0623a6569d61af301161'

	# g++ -o $(PIDIGITS)-gcov --coverage $(PIDIGITS_TF2) $(PIDIGITS_SRC)
	# g++ -o $(PIDIGITS)-gprof -pg $(PIDIGITS_TF) $(PIDIGITS_SRC)
	# [ -d $(PIDIGITS_PROF_DIR) ] || mkdir $(PIDIGITS_PROF_DIR)
	# mv $(PIDIGITS)-gcov $(PIDIGITS_PROF_DIR)/
	# mv $(PIDIGITS)-gprof $(PIDIGITS_PROF_DIR)/
	# mv $(PIDIGITS).gcno $(PIDIGITS_PROF_DIR)/
clean-pidigits:
	rm -f $(PIDIGITS){,-tweaked}
	rm -f $(PIDIGITS_PROF_DIR)/$(PIDIGITS)-{gcov,gprof}
	rm -f $(PIDIGITS_PROF_DIR)/$(PIDIGITS).{gcda,gcno,out}
