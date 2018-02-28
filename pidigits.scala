// Usage: run ../library/bigarrayint.lib ../benchmarks/pidigits.scala

def run(limit: Int, report5: Int => Unit): Unit = {
  var kZero = withBigFromInt[Array[Int]](0, (_: Array[Int]) => _);
  val kOne = new Array[Int](1); kOne(0) = 1;
  val kTwo = new Array[Int](1); kTwo(0) = 2;
  val kThree = new Array[Int](1); kThree(0) = 3; // must fit in big_baseLog bits
  withBigFromInt[Unit](10, (kTen: Array[Int]) => {

    // Note: the rest of the scope is automatically generated from
    //   other-lang/miniscala/src/pidigits.scala
    // using GNU sed
    //   -e 's/BigInt/Array[Int]/g' -e 's/\(with\w*\|cmp\)/&Big/g' -e 's/IntBig/Int/g' -e 's/\(\w\+\).intValue/toIntFromBig(\1)/g'

    var k = 0;
    var ns = 0;

    def rec(i: Int, nDivK: Array[Int], d0: Array[Int], a0: Array[Int]): Unit = {
      if (i < limit) {
        k = k + 1;
        withMulBig[Unit](nDivK, kTwo, (t0: Array[Int]) => {
          withBigFromInt[Unit](k, (kBig: Array[Int]) => {
            withMulBig[Unit](nDivK, kBig, (n: Array[Int]) => {
              withBigFromInt[Unit](2*k + 1, (k2: Array[Int]) => {
                withAddBig[Unit](a0, t0, (a0t0sum: Array[Int]) => {
                  withMulBig[Unit](a0t0sum, k2, (a: Array[Int]) => {
                    withMulBig[Unit](d0, k2, (d: Array[Int]) => {
                      rec2(i, n, d, a)
              }) }) }) })
        }) }) })
      }
    };

    def rec2(i: Int, n: Array[Int], d: Array[Int], a: Array[Int]): Unit = {
      if (cmpBig(a, n) >= 0) {
        withMulBig[Unit](n, kThree, (nMul3: Array[Int]) => {
          withAddBig[Unit](nMul3, a, (three_n_plus_a: Array[Int]) => {
            withDivBig[Unit](three_n_plus_a, d, (t: Array[Int]) => {
              // u = three_n_plus_a % d + n;
              withMulBig[Unit](t, d, (td: Array[Int]) => {
                withSubBig[Unit](three_n_plus_a, td, (u_n: Array[Int]) => {
                  withAddBig[Unit](u_n, n, (u: Array[Int]) => {
                    if (cmpBig(d, u) > 0) {
                      ns = ns * 10 + toIntFromBig(t);
                      val iNext = i + 1;
                      if (iNext % 5 == 0) {
                        report5(ns);
                        ns = 0
                      };
                      withNextNAndABig[Unit](a, n, td,
                        (n10: Array[Int], aNext: Array[Int]) => {
                          rec(iNext, n10, d, aNext)
                      })
                    } else
                      rec(i, n, d, a)
                  }) }) })
            }) }) })
      } else
        rec(i, n, d, a)
    };

    def withNextNAndABig[U](a: Array[Int], n: Array[Int], td: Array[Int],
      f: (Array[Int], Array[Int]) => U
    ): U = {
      withSubBig[U](a, td, (a_td: Array[Int]) => {
        withMulBig[U](a_td, kTen, (aNext: Array[Int]) => {
          withMulBig[U](n, kTen, (n10: Array[Int]) => {
            f(n10, aNext)
      }) }) })
    };

    rec(0, kOne, kOne, kZero)
  })
};

var failCount = 0;
var mul31HalfHash = 0;
var i = 0;
def checkMul31HalfHash(n: Int): Unit = {
  // printInt(n); printChar('\n');
  mul31HalfHash = (mul31HalfHash*31 + n) >> 1;
  i = i + 5;
  val exp = if (i == 10) 289785
  else if (i == 20) 70094125
  else if (i == 40) -705203370
  else if (i == 200) 181146281
  else if (i == 500) 358633796
  else if (i == 1000) -780802796
  else if (i == 10000) -653198279
  else 0;
  if (exp != 0) {
    val act = mul31HalfHash;
    if (exp != act) {
      printString("FAILED for n="); printInt(i); printString(":\n");
      printString("Exp.: "); printInt(exp); printChar('\n');
      printString("Act.: "); printInt(act); printChar('\n');
      failCount = failCount + 1
    }
  }
};

val n = 200;
run(n, checkMul31HalfHash);
if (i == n && failCount == 0) {
  printChar('O'); printChar('K'); printChar('\n')
};
()
