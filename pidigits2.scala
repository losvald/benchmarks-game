// Usage: run ../library/bigarrayint2.lib ../benchmarks/pidigits2.scala

def run@(limit: Int, report5@: Int => Unit): Unit = {
  val kZero@ = new Array[Int](0); // XXX depends on implementation detailsy
  val kOne@ = new Array[Int](1); kOne(0) = 1;
  val kTwo@ = new Array[Int](1); kTwo(0) = 2;
  val kThree@ = new Array[Int](1); kThree(0) = 3; // must fit in big_baseLog bits
  withBigFromInt[Unit](10, {def __1@(kTen@: Array[Int]) = {

    // Note: the rest of the scope is semi-automatically generated from
    //   other-lang/miniscala/src/pidigits.scala
    // using GNU sed
    //   -e 's/BigInt/Array[Int]/g' -e 's/\(with\w*\|cmp\)/&Big/g' -e 's/IntBig/Int/g' -e 's/\(\w\+\).intValue/toIntFromBig(\1)/g'
    // then manually replacing fun literals with named functions & extra scopes
    // then adding annots on ": Array[Int]" and "Array[Int] =>" and vars via regex

    var k@ = 0;
    var ns@ = 0;

    def rec@(i: Int, nDivK@: Array[Int], d0@: Array[Int], a0@: Array[Int]): Unit = {
      if (i < limit) {
        k = k + 1;
        withMulBig[Unit](nDivK, kTwo, {def _1@(t0@: Array[Int]) = {
          withBigFromInt[Unit](k, {def _2@(kBig@: Array[Int]) = {
            withMulBig[Unit](nDivK, kBig, {def _3@(n@: Array[Int]) = {
              withBigFromInt[Unit](2*k + 1, {def _4@(k2@: Array[Int]) = {
                withAddBig[Unit](a0, t0, {def _5@(a0t0sum@: Array[Int]) = {
                  withMulBig[Unit](a0t0sum, k2, {def _6@(a@: Array[Int]) = {
                    withMulBig[Unit](d0, k2, {def _7@(d@: Array[Int]) = {
                      rec2(i, n, d, a)
              };_7}) };_6}) };_5}) };_4})
        };_3}) };_2}) };_1})
      }
    };

    def rec2@(i: Int, n@: Array[Int], d@: Array[Int], a@: Array[Int]): Unit = {
      if (cmpBig(a, n) >= 0) {
        withMulBig[Unit](n, kThree, {def _1@(nMul3@: Array[Int]) = {
          withAddBig[Unit](nMul3, a, {def _2@(three_n_plus_a@: Array[Int]) = {
            withDivBig[Unit](three_n_plus_a, d, {def _3@(t@: Array[Int]) = {
              // u = three_n_plus_a % d + n;
              withMulBig[Unit](t, d, {def _4@(td@: Array[Int]) = {
                withSubBig[Unit](three_n_plus_a, td, {def _5@(u_n@: Array[Int]) = {
                  withAddBig[Unit](u_n, n, {def _6@(u@: Array[Int]) = {
                    if (cmpBig(d, u) > 0) {
                      ns = ns * 10 + toIntFromBig(t);
                      val iNext = i + 1;
                      if (iNext % 5 == 0) {
                        report5(ns);
                        ns = 0
                      };
                      withNextNAndABig[Unit](a, n, td, {
                        def _7@(n10@: Array[Int], aNext@: Array[Int]) = {
                          rec(iNext, n10, d, aNext)
                      };_7})
                    } else
                      rec(i, n, d, a)
                  };_6}) };_5}) };_4})
            };_3}) };_2}) };_1})
      } else
        rec(i, n, d, a)
    };

    def withNextNAndABig@[U](a@: Array[Int], n@: Array[Int], td@: Array[Int],
      f@: (Array@[Int], Array@[Int]) => U
    ): U = {
      withSubBig[U](a, td, {def _1@(a_td@: Array[Int]) = {
        withMulBig[U](a_td, kTen, {def _2@(aNext@: Array[Int]) = {
          withMulBig[U](n, kTen, {def _3@(n10@: Array[Int]) = {
            f(n10, aNext)
      };_3}) };_2}) };_1})
    };

    rec(0, kOne, kOne, kZero)
  };__1})
};

var failCount@ = 0;
var mul31HalfHash@ = 0;
var i@ = 0;
def checkMul31HalfHash@(n: Int): Unit = {
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
