# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# regex-dna Python 3 #5
# contributed by Dominique Wahli
# 2to3
# modified by Justin Peel
# slightly modified by Leo Osvald

from sys import stdin,stdout,argv
from re import sub, findall

def main():
    seq = open(argv[1]).read()
    write = stdout.write
    ilen = len(seq)

    seq = sub(b'>.*\n|\n', b'', seq)
    clen = len(seq)

    variants = (
          b'agggtaaa|tttaccct',
          b'[cgt]gggtaaa|tttaccc[acg]',
          b'a[act]ggtaaa|tttacc[agt]t',
          b'ag[act]gtaaa|tttac[agt]ct',
          b'agg[act]taaa|ttta[agt]cct',
          b'aggg[acg]aaa|ttt[cgt]ccct',
          b'agggt[cgt]aa|tt[acg]accct',
          b'agggta[cgt]a|t[acg]taccct',
          b'agggtaa[cgt]|[acg]ttaccct')
    for f in variants:
        write(f + b' ' +bytes(str(len(findall(f, seq)))) + b'\n')

    subst = {
          b'B' : b'(c|g|t)', b'D' : b'(a|g|t)',   b'H' : b'(a|c|t)', b'K' : b'(g|t)',
          b'M' : b'(a|c)',   b'N' : b'(a|c|g|t)', b'R' : b'(a|g)',   b'S' : b'(c|g)',
          b'V' : b'(a|c|g)', b'W' : b'(a|t)',     b'Y' : b'(c|t)'}
    for f, r in subst.items():
        seq = sub(f, r, seq)
    write(b'\n')
    write(bytes(str(ilen)) + b'\n')
    write(bytes(str(clen)) + b'\n')
    write(bytes(str(len(seq))) + b'\n')

main()