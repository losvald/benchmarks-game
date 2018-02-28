package pidigits

class CPS[M <: lib.big.Big](protected val module: M) {
  import module.{ I => BigInt, _ }
  def BigInt(i: Int) = {
    var ret: BigInt = uninit
    fromInt(i, { ret = _ })
    ret
  }

  def apply(limit: Int, report5: Int => Unit) = {
    val (kZero, kOne, kTen) = (BigInt(0), BigInt(1), BigInt(10))
    val (kTwo, kThree) = (BigInt(2), BigInt(3))

    var k = 0;
    var ns = 0;

    def rec(i: Int, nDivK: BigInt, d0: BigInt, a0: BigInt): Unit = {
      if (i < limit) {
        k = k + 1;
        withMul[Unit](nDivK, kTwo, (t0: BigInt) => {
          withBigFromInt[Unit](k, (kBig: BigInt) => {
            withMul[Unit](nDivK, kBig, (n: BigInt) => {
              withBigFromInt[Unit](2*k + 1, (k2: BigInt) => {
                withAdd[Unit](a0, t0, (a0t0sum: BigInt) => {
                  withMul[Unit](a0t0sum, k2, (a: BigInt) => {
                    withMul[Unit](d0, k2, (d: BigInt) => {
                      rec2(i, n, d, a)
              }) }) }) })
        }) }) })
      }
    };

    def rec2(i: Int, n: BigInt, d: BigInt, a: BigInt): Unit = {
      if (cmp(a, n) >= 0) {
        withMul[Unit](n, kThree, (nMul3: BigInt) => {
          withAdd[Unit](nMul3, a, (three_n_plus_a: BigInt) => {
            withDiv[Unit](three_n_plus_a, d, (t: BigInt) => {
              // u = three_n_plus_a % d + n;
              withMul[Unit](t, d, (td: BigInt) => {
                withSub[Unit](three_n_plus_a, td, (u_n: BigInt) => {
                  withAdd[Unit](u_n, n, (u: BigInt) => {
                    if (cmp(d, u) > 0) {
                      ns = ns * 10 + t.intValue;
                      val iNext = i + 1;
                      if (iNext % 5 == 0) {
                        report5(ns);
                        ns = 0
                      };
                      withNextNAndA[Unit](a, n, td,
                        (n10: BigInt, aNext: BigInt) => {
                          rec(iNext, n10, d, aNext)
                      })
                    } else
                      rec(i, n, d, a)
                  }) }) })
            }) }) })
      } else
        rec(i, n, d, a)
    };

    def withNextNAndA[U](a: BigInt, n: BigInt, td: BigInt,
      f: (BigInt, BigInt) => U
    ): U = {
      withSub[U](a, td, (a_td: BigInt) => {
        withMul[U](a_td, kTen, (aNext: BigInt) => {
          withMul[U](n, kTen, (n10: BigInt) => {
            f(n10, aNext)
      }) }) })
    };

    rec(0, kOne, kOne, kZero)
  }

  def withBigFromInt[R](n: Int, f: BigInt => R) = fromInt(n, f)

  def main(args: Array[String]) {
    var i5 = 0
    def report5(n: Int) = {
      i5 = i5 + 1
      printf("%05d", n)
      if (i5 % 2 == 0) {
        print("\t:"); print(i5 * 5); print('\n')
      }
    }
    apply(args(1).toInt, report5)
  }
}

object BigIntWrapper extends CPS(lib.big.BigIntWrapper)

object ArrayInt extends CPS(lib.big.ArrayInt)
