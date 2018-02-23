package pidigits

class CPS[M <: lib.big.Big](protected val module: M) {
  import module.{ I => BigInt, _ }
  def BigInt(i: Int) = {
    var ret: BigInt = uninit
    fromInt(i) { ret = _ }
    ret
  }

  def apply(limit: Int, report5: Int => Unit) = {
    val (kZero, kOne, kTen) = (BigInt(0), BigInt(1), BigInt(10))
    val (kTwo, kThree) = (BigInt(2), BigInt(3))

    var k = 0
    var ns = 0

    def rec(i: Int, nDivK: BigInt, d0: BigInt, a0: BigInt): Unit = {
      if (i < limit) {
        k = k + 1;
        withMul(nDivK, kTwo) { t0 =>
          withBigFromInt(k) { kBig =>
            withMul(nDivK, kBig) { n =>
              withBigFromInt(2*k + 1) { k2 =>
                withAdd(a0, t0) { a0t0sum =>
                  withMul(a0t0sum, k2) { a =>
                    withMul(d0, k2) { d =>
                      if (a >= n) {
                        withMul(n, kThree) { nMul3 =>
                          withAdd(nMul3, a) { three_n_plus_a =>
                            withDiv(three_n_plus_a, d) { t =>
                              // u = three_n_plus_a % d + n;
                              withMul(t, d) { td =>
                                withSub(three_n_plus_a, td) { u_n =>
                                  withAdd(u_n, n) { u =>
                                    if (d > u) {
                                      ns = ns * 10 + t.intValue
                                      val iNext = i + 1;
                                      if (iNext % 5 == 0) {
                                        report5(ns)
                                        ns = 0
                                      }
                                      withSub(a, td) { a_td =>
                                        withMul(a_td, kTen) { aNext =>
                                          withMul(n, kTen) { n10 =>
                                            rec(iNext, n10, d, aNext)
                                      } } }
                                    } else
                                      rec(i, n, d, a)
                              } } }
                        } } }
                      } else
                        rec(i, n, d, a)
              } } } }
        } } }
      }
    }
    rec(0, kOne, kOne, kZero)
  }

  def withBigFromInt[R](n: Int)(f: BigInt => R) = fromInt(n)(f)

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
