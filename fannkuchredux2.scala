// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/

// contributed by Isaac Gouy
// converted to Scala by Leo Osvald

def fannkuch(n: Int): Int = {
  // all three arrays could be Array[Int@], but Ints are already stack-allocated
  val perm@ = new Array[Int](n);
  val perm1@ = new Array[Int](n);
  val count@ = new Array[Int](n);
  var maxFlipsCount = 0;
  var permCount = 0;
  var checksum = 0;

  var i = 0;
  while (i < n) { perm1(i) = i; i = i + 1 };

  var r = n;
  var first = true;
  while (r != n || first) {
    first = false;

    while (r != 1) { count(r-1) = r; r = r - 1 };

    i = 0;
    while (i < n) { perm(i) = perm1(i); i = i + 1 };
    var flipsCount = 0;
    var k = perm(0) + 0; // workaround to store a copy as 1st-class value
    while (k != 0) {
      val k2@ = (k+1) >> 1;
      i = 0;
      while (i < k2) {
        val temp@ = perm(i); perm(i) = perm(k-i); perm(k-i) = temp + 0;
        i = i + 1
      };
      flipsCount = flipsCount + 1;
      k = perm(0) + 0 // workaround to store a copy as 1st-class value
    };
    checksum = checksum + (if (permCount % 2 == 0) flipsCount else -flipsCount);
    if (flipsCount > maxFlipsCount)
      maxFlipsCount = flipsCount;

    // Use incremental change to generate another permutation
    var brk = false;
    while (r != n && !brk) {
      val perm0@ = perm1(0);
      i = 0;
      while (i < r) {
        val j@ = i + 1;
        perm1(i) = perm1(j);
        i = j + 0 // FIXME work around inability of i to be a 2nd-class var
      };
      perm1(r) = perm0 + 0;
      count(r) = count(r) - 1;
      brk = count(r) > 0;
      r = r + 1
    };
    if (brk) r = r - 1;

    permCount = permCount + 1
  };
  printInt(checksum); printChar('\n');
  maxFlipsCount
};

val result@ = fannkuch(5);
if (result == 7) {
  printChar('O'); printChar('K')
} else {
  printInt(result)
};
printChar('\n')
