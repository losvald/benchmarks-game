# The Computer Language Benchmarks Game

# http://shootout.alioth.debian.org/
# reverse-complement Python 3 #4

#

# contributed by Jacob Lee, Steven Bethard, et al
# 2to3, fixed by Daniele Varrazzo
# modified by Daniel Nanz
# slightly modified by Leo Osvald

import string
import sys

def show(seq, table=string.maketrans(b'ACBDGHKMNSRUTWVYacbdghkmnsrutwvy',
                                     b'TGVHCDMKNSYAAWBRTGVHCDMKNSYAAWBR'),
         write=sys.stdout.write, nl=b'\n'):
    
    [header, s] = seq.split(nl, 1)
    s = s.translate(table, nl)[: : -1]
    
    write(b'>' + header + nl)
    for i in range(0, len(s), 60):
        write(s[i : i + 60] + nl)



def main():
    f = open(sys.argv[1], 'r')
    #sys.stdin = sys.stdin.detach()
    seqs = b''.join([line for line in f]).split(b'>')[1 : ]
    
    for seq in seqs:
        show(seq)        


main()
