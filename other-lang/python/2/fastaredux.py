# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

import sys

width = 60
lookup_size = 4096
lookup_scale = float(lookup_size - 1L)

alu = (
   'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG'
   'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA'
   'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT'
   'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA'
   'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG'
   'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC'
   'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA')

iub = list(zip('acgtBDHKMNRSVWY', [0.27, 0.12, 0.12, 0.27] + [0.02] * 11))

homosapiens = [
    ('a', 0.3029549426680),
    ('c', 0.1979883004921),
    ('g', 0.1975473066391),
    ('t', 0.3015094502008),
]

def gen_random_lookups(ia=3877, ic=29573, im=139968):
    seed = 42
    imf = float(im)
    while True:
        seed = (seed * ia + ic) % im
        yield seed * (lookup_scale / imf)

random_lookups_generator = gen_random_lookups()

def repeat_fasta(src, n):
    r = len(src)
    s = src + src + src[:n % r]
    for j in range(n // width):
        i = j * width % r
        print(s[i:i + width])
    if n % width:
        print(s[-(n % width):])

def random_fasta(genelist, n):
    def make_cumulative(genelist):
        psums, chars = [], []
        prob = 0.
        for char, p in genelist:
            prob += p * lookup_scale
            psums.append(prob)
            chars.append(char)
        return (psums, chars)

    def gen_symbols():
        cprob_lookup, chars = make_cumulative(genelist)
        cprob_lookup[-1] = lookup_size - 1

        j = 0
        lookup = [0] * lookup_size
        for i in range(lookup_size):
            while cprob_lookup[j] < i:
                j += 1
            lookup[i] = j

        gr = random_lookups_generator.next
        while True:
            r = gr()
            ind = lookup[int(r)]
            while cprob_lookup[ind] < r:
                ind += 1
            yield chars[ind]

    r = range(width)
    jn = ''.join
    gs = gen_symbols().next

    for j in range(n // width):
        print(jn([gs() for i in r]))
    if n % width:
        print(jn([gs() for i in range(n % width)]))

def fastaredux(args):
    n = int(args[1] if len(args) > 1 else 1000)

    print('>ONE Homo sapiens alu')
    repeat_fasta(alu, 2 * n)

    print('>TWO IUB ambiguity codes')
    random_fasta(iub, 3 * n)

    print('>THREE Homo sapiens frequency')
    random_fasta(homosapiens, 5 * n)


if __name__ == '__main__':
    fastaredux(sys.argv)
