# The Computer Language Shootout
# http://shootout.alioth.debian.org
# k-nucleotide Ruby 1.9 #2
#
# contributed by jose fco. gonzalez
# modified by Sokolov Yura
# slightly modified by Leo Osvald

seq = String.new

def frecuency( seq,length )
    n, table = seq.length - length + 1, Hash.new(0)
    f, i = nil, nil
    (0 ... length).each do |f|
        (f ... n).step(length) do |i|
            table[seq[i,length]] += 1
        end
    end
    [n,table]

end

def sort_by_freq( seq,length )
    n,table = frecuency( seq,length )
    a, b, v = nil, nil, nil
    table.sort{|a,b| b[1] <=> a[1]}.each do |v|
        puts "%s %.3f" % [v[0].upcase,((v[1]*100).to_f/n)]
    end
    puts
end

def find_seq( seq,s )
    n,table = frecuency( seq,s.length )
    puts "#{table[s].to_s}\t#{s.upcase}"
end

f = File.open(ARGV[0])
line = f.gets while line !~ /^>THREE/
line = f.gets
while (line !~ /^>/) & line do
    seq << line.chomp
    line = f.gets
end

[1,2].each {|i| sort_by_freq( seq,i ) }

%w(ggt ggta ggtatt ggtattttaatt ggtattttaatttatagt).each{|s| find_seq( seq,s) }
