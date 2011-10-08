all: binarytrees fannkuchredux fasta fasta mandelbrot nbody regexdna reversecomplement spectralnorm
clean: clean-binarytrees clean-fannkuchredux clean-fasta clean-mandelbrot clean-nbody clean-regexdna clean-reversecomplement clean-spectralnorm

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
