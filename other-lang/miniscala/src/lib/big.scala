package lib.big

// MiniScala2 has no namespaces, so simulate this by putting everything in here,
// so that usage of big.ArrayInt.* can be renamed to bigArrayInt* via a script

trait Big {
  type I

  def uninit: I
  def fromInt[U](i: Int)(f: I => U): U
  def toInt(n: I): Int
  def withAdd[U](lhs: I, rhs: I)(f: I => U): U
  def withSub[U](lhs: I, rhs: I)(f: I => U): U
  def withMul[U](lhs: I, rhs: I)(f: I => U): U
  def withDiv[U](lhs: I, rhs: I)(f: I => U): U

  implicit class Ops(private val lhs: I) {
    // Note: assume == and != already does the right thing
    def <(rhs: I) = cmp(lhs, rhs) < 0
    def <=(rhs: I) = cmp(lhs, rhs) <= 0
    def >(rhs: I) = cmp(lhs, rhs) > 0
    def >=(rhs: I) = cmp(lhs, rhs) >= 0
    def intValue = toInt(lhs)
  }

  protected def cmp(lhs: I, rhs: I): Int

  private[big] val baseLog: Int // for fast mult/div by: <</>> base
  private[big] lazy val base = 1 << baseLog
  private[big] lazy val baseMask = base - 1 // for fast modulus by: & baseMask

  private[big] def repr(n: I): String
}


object ArrayInt extends BaselessArrayInt {
  private[big] lazy val baseLog = 3
}

private[big] abstract class BaselessArrayInt extends Big {
  type I = Array[Int]
  private[big] val I = Array

  def uninit = I()
  def fromInt[U](i0: Int)(f: I => U): U = {
    val sign = cmpElem(i0, 0)
    val i = if (sign < 0) -i0 else i0
    var len = 0
    var n = i
    while (n != 0) {
      n /= base
      len += 1
    }

    val ret = new I(len)
    n = i
    while (n != 0) {
      len = len - 1
      ret(len) = n % base
      n /= base;
    }
    if (sign < 0) ret(0) = -ret(0)
    f(ret)
  }
  def toInt(n: I): Int = {
    withSgn(n) { sgn =>
      var ret = 0
      var i = 0
      while (i < n.length) {
        ret = ret * base + n(i)
        i = i + 1
      }
      sgn * ret
    }
  }

  def withAdd[U](lhs: I, rhs: I)(f: I => U): U = {
    withSgn(lhs) { lhsSgn =>
      withSgn(rhs) { rhsSgn =>
        if (rhsSgn == 0) f(lhs)
        else if (lhsSgn == 0) f(rhs)
        else if (lhsSgn == rhsSgn) withAddMag(lhs, rhs)(f)
        else withMagDiff(lhs, rhs)(f)
      }
    }
  }
  def withSub[U](lhs: I, rhs: I)(f: I => U): U = {
    withMagDiff(lhs, rhs)(f) // FIXME incorrect, but sufficient for pidigits
  }
  def withMul[U](lhs: I, rhs: I)(f: I => U): U = {
    withSgn(lhs) { lhsSgn =>
      withSgn(rhs) { rhsSgn =>
        withMulMag(lhs, rhs) { withNoLeadingZeroes(_) { product =>
          sgnSet(product, sgnProd(lhsSgn, rhsSgn))
          f(product)
        } }
      }
    }
  }
  def withDiv[U](lhs: I, rhs: I)(f: I => U): U = {
    val c = cmpMag(lhs, rhs)
    if (c == 0) f(one)
    else if (c < 0) fromInt(0)(f)
    else withDivMag(lhs, rhs)(f)
  }

  def cmp(lhs: I, rhs: I): Int = {
    var ret = 0
    withSgn(lhs) { lhsSgn =>
      withSgn(rhs) { rhsSgn0 =>
        val rhsSgn = if (lhs eq rhs) lhsSgn else rhsSgn0 // XXX work around mut.
        ret = if (lhsSgn == rhsSgn) lhsSgn * cmpMag(lhs, rhs)
        else cmpElem(lhsSgn, rhsSgn)
      }
    }
    ret
  }

  private val hexDigitWidth = (baseLog + 3) / 4
  private[big] def repr(n: I) = withSgn(n) { sgn =>
    (if (sgn < 0) "-" else "") + n.map(s"%${hexDigitWidth}X" format _).mkString
  }

  // implementation details

  private val one = {
    var ret: I = null
    fromInt(1) { ret = _ }
    ret
  }

  private def cmpElem(lhs: Int, rhs: Int) =
    (if (lhs > rhs) 1 else 0) - (if (lhs < rhs) 1 else 0)

  private def sgnProd(lhs: Int, rhs: Int) =
    (if (lhs == rhs) 1 else 0) - (if (lhs != rhs) 1 else 0)

  private def cmpMag(lhs: I, rhs: I): Int = {
    if (lhs.length < rhs.length) -1
    else if (lhs.length > rhs.length) 1
    else {
      var i = 0
      var c = 0
      while (i < lhs.length && c == 0) {
        c = cmpElem(lhs(i), rhs(i))
        i = i + 1
      }
      c
    }
  }

  private def withMagDiff[U](lhs: I, rhs: I)(f: I => U): U = {
    withSgn(lhs) { lhsSgn =>
      val c = cmpMag(lhs, rhs)
      if (c == 0) fromInt(0)(f)
      else {
        // (if (c > 0) withSubMag(lhs, rhs) else withSubMag(rhs, lhs) _) { t =>
        def k(tmp: I) = withNoLeadingZeroes(tmp) { resultMag =>
          sgnSet(resultMag, sgnProd(c, lhsSgn))
          f(resultMag)
        }
        if (c > 0) withSubMag(lhs, rhs) { k(_) }
        else withSubMag(rhs, lhs) { k(_) }
      }
    }
  }

  private def withAddMag[U](lhs: I, rhs: I)(f: I => U): U = {
    if (lhs.length < rhs.length) withAddMag(rhs, lhs)(f)
    else {
      val ret = new I(lhs.length)
      var lhsIdx = lhs.length
      var rhsIdx = rhs.length
      var sum = 0

      // Add common parts of both numbers
      while (rhsIdx > 0) {
        lhsIdx = lhsIdx - 1
        rhsIdx = rhsIdx - 1
        sum = lhs(lhsIdx) + rhs(rhsIdx) + sum / base
        ret(lhsIdx) = sum % base
      }

      // Copy remainder of longer number
      var carry = sum >= base
      while (lhsIdx > 0 && carry) {
        lhsIdx = lhsIdx - 1
        ret(lhsIdx) = lhs(lhsIdx) + 1
        carry = (ret(lhsIdx) == 0)
      }

      // Copy remainder of longer number
      while (lhsIdx > 0) {
        lhsIdx = lhsIdx - 1
        ret(lhsIdx) = lhs(lhsIdx)
      }

      // Grow result if necessary
      if (carry) {
        val bigger = new I(ret.length + 1)
        var i = 0
        bigger(0) = 1
        while (i < ret.length) {
          bigger(i + 1) = ret(i)
          i = i + 1
        }
        f(bigger)
      } else {
        f(ret)
      }
    }
  }

  private def withSubMag[U](big: I, lil: I)(f: I => U): U = {
    var bigIdx = big.length
    val ret = new I(bigIdx)
    var lilIdx = lil.length
    require(bigIdx >= lilIdx)
    var diff = 0

        // Subtract common parts of both numbers
    while(lilIdx > 0) {
      bigIdx = bigIdx - 1
      lilIdx = lilIdx - 1
      diff = big(bigIdx) - lil(lilIdx) + (if (diff < 0) -1 else 0)
      ret(bigIdx) = (diff + base) % base
    }

    // Subtract remainder of longer number while borrow propagates
    var borrow = if (diff < 0) -1 else 0
    while (bigIdx > 0 && borrow != 0) {
      bigIdx = bigIdx - 1
      ret(bigIdx) = big(bigIdx) - 1
      borrow = (if (ret(bigIdx) == -1) 1 else 0)
    }

    // Copy remainder of longer number
    while (bigIdx > 0) {
      bigIdx = bigIdx - 1
      ret(bigIdx) = big(bigIdx)
    }

    f(ret)
  }

  private[big] def withMulMag[U](lhs: I, rhs: I)(f: I => U): U = {
    val ret = new I(lhs.length + rhs.length)
    var lhsIdx = lhs.length - 1
    var rhsIdx = rhs.length - 1
    ret(ret.length - 1) = 0
    var i = lhsIdx
    while (i >= 0) {
      var carry = 0
      var j = rhsIdx
      var k = rhsIdx + 1 + i
      while (j >= 0) {
        val product = lhs(i) * rhs(j) + ret(k) + carry
        ret(k) = product % base
        carry = product / base
        k = k - 1
        j = j - 1
      }
      ret(i) = carry % base
      i = i - 1
    }
    f(ret)
  }

  private[big] def withDivMag[U](lhs: I, rhs: I)(f: I => U): U = {
    val lhsLog = logBaseMag(lhs)
    val rhsLog = logBaseMag(rhs)
    val loLog = lhsLog - rhsLog - (if (lhsLog != rhsLog) 1 else 0)
    val hiLog = lhsLog - rhsLog + 1

    // TODO(hi-prio) rewrite in CPS style

    var lo = uninit; withPowBase(loLog) { lo = _ }

    // TODO(lo-prio) try pruning hi > base or lo <= base
    var hi = uninit; withPowBase(hiLog) { hi = _ }

    while (cmp(lo, hi) < 0) {
      // TODO signedness?
      var mid = uninit
      withAddMag(hi, one) { hiPlus1 =>
        withAddMag(lo, hiPlus1) {
          withDiv2Mag(_) { mid = _ }
        }
      }

      withMulMag(mid, rhs) { product =>
        if (cmp(product, lhs) <= 0) lo = mid
        else withSubMag(mid, one) { hi = _ }
      }
    }

    f(lo)
  }

  private[big] def withDiv2Mag[U](lhs: I)(f: I => U): U = {
    if (lhs.length == 1) f(I(lhs(0) / 2))
    else {
      var borrow = (if (lhs(0) == 1) 1 else 0)
      var lhsBeg = borrow
      var lhsEnd = lhs.length
      val ret = new I(lhsEnd - lhsBeg)
      var lhsIdx = lhsBeg
      while (lhsIdx < lhsEnd) {
        val d = lhs(lhsIdx) + base * borrow
        ret(lhsIdx - lhsBeg) = d / 2
        lhsIdx = lhsIdx + 1
      }
      f(ret)
    }
  }

  private[big] def withNoLeadingZeroes[U](n: I)(f: I => U): U = {
    var beg = 0
    while (beg < n.length && n(beg) == 0) {
      beg = beg + 1
    }

    val ret = new I(n.length - beg)
    var i = 0
    while (i < ret.length) {
      ret(i) = n(beg + i)
      i = i + 1
    }
    f(ret)
  }

  private[big] def logBaseMag(n: I): Int = n.length - 1
  private[big] def withPowBase[U](i: Int)(f: I => U): U = {
    val ret = new I(1 + i)
    ret(0) = 1
    f(ret)
  }

  private[big] def sgn(n: I): Int = {
    if (n.isEmpty || n(0) == 0) 0
    else if (n(0) > 0) +1
    else -1
  }

  // XXX encoding sign in array is _very fragile_ combined with CPS style e.g.:
  //
  //   withOp1(mut, rhs) { ret =>  // may mutate the sign of mut
  //     ...
  //     withOp2(lhs, mut) { ret2 => // ouch, what if lhs was negative?
  //   }

  private[big] def withSgn[U](n: I)(f: Int => U): U = {
    val sign = sgn(n)
    if (sign != 0) {
      val n0 = n(0)
      n(0) = n0 * sign
      val ret = f(sign)
      n(0) = n0
      ret
    } else { // n could be empty
      f(sign)
    }
  }

  private[big] def sgnSet(n: I, s: Int): Unit = {
    // TODO what if s == 0 and n non-empty?
    if (s == 0) n(0) = 0
    else if (n.nonEmpty) n(0) = sgnProd(s, cmpElem(n(0), 0)) * n(0)
  }
}


object ListInt extends BaselessListInt {
  private[big] val baseLog = 3
}

private[big] abstract class BaselessListInt extends Big {

  type I = List[Int]
  private[big] val I = List

  def uninit = Nil
  def fromInt[U](i: Int)(f: I => U): U = {
    def rec(i: Int, ret: I): U = {
      if (i == 0) reverse(ret)(f) else rec(i >> baseLog, (i & baseMask) :: ret)
    }
    rec(i, Nil)
  }

  def toInt(n: I): Int = {
    if (n.isEmpty) 0 else (toInt(n.tail) << baseLog) | n.head
  }

  private[big] def repr(n: I) = n.reverse.map("%X" format _).mkString(" ")

  private[big] def reverse[U](n: I)(f: I => U): U = {
    def rec(src: I, dst: I): U =
      if (src.isEmpty) f(dst) else rec(src.tail, src.head :: dst)
    rec(n, Nil)
  }

  def cmp(lhs0: I, rhs0: I): Int = {
    var ret = 0
    reverse(lhs0) { lhs0 => reverse(rhs0) { rhs0 =>
      def rec(lhs: I, rhs: I) = {
        val moreLhs = lhs.nonEmpty
        val moreRhs = rhs.nonEmpty
        // TODO
      }
    } }
    ret
  }

  def withAdd[U](lhs: I, rhs: I)(f: I => U): U = ???
  def withSub[U](lhs: I, rhs: I)(f: I => U): U = ???
  def withMul[U](lhs: I, rhs: I)(f: I => U): U = ???
  def withDiv[U](lhs: I, rhs: I)(f: I => U): U = ???
}


/** Dummy implementation, useful for sanity checks and in unit tests */
object BigIntWrapper extends Big {
  type I = BigInt
  val I = BigInt

  def uninit = null
  def fromInt[U](i: Int)(f: I => U): U = f(I(i))
  def toInt(n: I): Int = n.toInt
  def withAdd[R](lhs: BigInt, rhs: BigInt)(f: BigInt => R) = f(lhs + rhs)
  def withSub[R](lhs: BigInt, rhs: BigInt)(f: BigInt => R) = f(lhs - rhs)
  def withMul[R](lhs: BigInt, rhs: BigInt)(f: BigInt => R) = f(lhs * rhs)
  def withDiv[R](lhs: BigInt, rhs: BigInt)(f: BigInt => R) = f(lhs / rhs)

  protected def cmp(lhs: I, rhs: I): Int = lhs.compare(rhs)

  private[big] val baseLog = 0 // dummy value

  private[big] def repr(n: I) = ???
}
