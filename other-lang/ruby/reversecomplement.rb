# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# reverse-complement Ruby 1.9 #2
#
# Contributed by Peter Bjarke Olsen
# Modified by Doug King
# Modified by Joseph LaFata
# Slightly modified by Leo Osvald

seq=""

def revcomp(seq)
  seq.reverse!.tr!('wsatugcyrkmbdhvnATUGCYRKMBDHVN','WSTAACGRYMKVHDBNTAACGRYMKVHDBN')
  stringlen=seq.length-1
  0.step(stringlen,60) {|x| print seq[x,60] , "\n"}
end

File.open(ARGV[0]).each do |line|
  if line.include? '>'
    if !seq.empty?
      revcomp(seq)
      seq=""
    end
    puts line
  else
    line.chomp!
    seq << line
  end
end
revcomp(seq)
