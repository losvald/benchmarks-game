val big_baseLog = 15;
val big_base = 1 << big_baseLog;
val big_baseMask = big_base - 1;

// Note: the rest is automatically generated from a well-tested code at
//   other-lang/miniscala/src/lib/big.scala
// using GNU sed on code between "BEGIN miniscala" and "END miniscala"
// (which strips private modifiers, adds prefixes to methods, and renames types)

  val one = { var ret: Array[Int] = new Array[Int](1); ret(0) = 1; ret };

  def withBigFromInt[U](i0: Int, f: Array[Int] => U): U = {
    val sign = big_cmpElem(i0, 0);
    val i = if (sign < 0) -i0 else i0;
    var len = 0;
    var n = i;
    while (n != 0) {
      n = n >> big_baseLog;
      len = len + 1
    };

    val ret = new Array[Int](len);
    n = i;
    while (n != 0) {
      len = len - 1;
      ret(len) = n & big_baseMask;
      n = n >> big_baseLog
    };
    if (sign < 0) ret(0) = -ret(0);
    f(ret)
  };
  def toIntFromBig(n: Array[Int]): Int = {
    withSgn[Int](n, (big_sgn: Int) => {
      var ret = 0;
      var i = 0;
      while (i < n.length) {
        ret = (ret << big_baseLog) + n(i);
        i = i + 1
      };
      big_sgn * ret
    })
  };

  def withAddBig[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    withSgn[U](lhs, (lhsSgn: Int) => {
      withSgn[U](rhs, (rhsSgn: Int) => {
        if (rhsSgn == 0) f(lhs)
        else if (lhsSgn == 0) f(rhs)
        else if (lhsSgn == rhsSgn) {
          big_withAddMag[U](lhs, rhs, (sum: Array[Int]) => {
            big_sgnSet(sum, lhsSgn);
            f(sum)
          })
        }
        else {
          big_withMagDiff[U](lhs, rhs, (sum: Array[Int]) => {
            big_sgnSet(sum, big_sgnProd(lhsSgn, cmpBig(lhs, rhs)));
            f(sum)
          })
        }
      })
    })
  };
  def withSubBig[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    val lhsSgn = big_sgn(lhs);
    val rhsSgn = big_sgn(rhs);
    if (big_sgn(rhs) == 0) f(lhs)
    else if (big_sgn(lhs) == 0) f(rhs)
    else {
      withSgn[U](lhs, (lhsSgn: Int) => {
        withSgn[U](rhs, (rhsSgn: Int) => {
          def k(big_retMag: Array[Int], sign: Int) = { big_sgnSet(big_retMag, sign); f(big_retMag) };
          if (lhsSgn != rhsSgn) big_withAddMag[U](lhs, rhs, (big_retMag: Array[Int]) => {
            k(big_retMag, lhsSgn)
          })
          else big_withMagDiff[U](lhs, rhs, (big_retMag: Array[Int]) => {
            k(big_retMag, big_sgnProd(lhsSgn, cmpBig(lhs, rhs)))
          })
        })
      })
    }
  };
  def withMulBig[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    withSgn[U](lhs, (lhsSgn: Int) => {
      withSgn[U](rhs, (rhsSgn: Int) => {
        big_withMulMag[U](lhs, rhs, (t: Array[Int]) => {
          big_withNoLeadingZeroes[U](t, (product: Array[Int]) => {
            big_sgnSet(product, big_sgnProd(lhsSgn, rhsSgn));
            f(product)
          })
        })
      })
    })
  };
  def withDivBig[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    val c = big_cmpMag(lhs, rhs);
    if (c == 0) f(one)
    else if (c < 0) withBigFromInt[U](0, f)
    else big_withDivMag[U](lhs, rhs, f)
  };

  def cmpBig(lhs: Array[Int], rhs: Array[Int]): Int = {
    withSgn[Int](lhs, (lhsSgn: Int) => {
      withSgn[Int](rhs, (rhsSgn0: Int) => {
        val rhsSgn = if (lhs == rhs) lhsSgn else rhsSgn0; // XXX work around mut
        if (lhsSgn == rhsSgn) lhsSgn * big_cmpMag(lhs, rhs)
        else big_cmpElem(lhsSgn, rhsSgn)
      })
    })
  };

  // implementation details

  def big_cmpElem(lhs: Int, rhs: Int) =
    (if (lhs > rhs) 1 else 0) - (if (lhs < rhs) 1 else 0);

  def big_sgnProd(lhs: Int, rhs: Int) =
    (if (lhs == rhs) 1 else 0) - (if (lhs != rhs) 1 else 0);

  def big_cmpMag(lhs: Array[Int], rhs: Array[Int]): Int = {
    if (lhs.length < rhs.length) -1
    else if (lhs.length > rhs.length) 1
    else {
      var i = 0;
      var c = 0;
      while (i < lhs.length && c == 0) {
        c = big_cmpElem(lhs(i), rhs(i));
        i = i + 1
      };
      c
    }
  };

  def big_withMagDiff[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    val c = big_cmpMag(lhs, rhs);
    if (c == 0) withBigFromInt[U](0, f)
    else {
      def k(tmp: Array[Int]) = big_withNoLeadingZeroes[U](tmp, f);
      if (c > 0) big_withSubMag[U](lhs, rhs, k)
      else big_withSubMag[U](rhs, lhs, k)
    }
  };

  def big_withAddMag[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    if (lhs.length < rhs.length) big_withAddMag[U](rhs, lhs, f)
    else {
      val ret = new Array[Int](lhs.length);
      var lhsIdx = lhs.length;
      var rhsIdx = rhs.length;
      var sum = 0;

      // Add common parts of both numbers
      while (rhsIdx > 0) {
        lhsIdx = lhsIdx - 1;
        rhsIdx = rhsIdx - 1;
        sum = lhs(lhsIdx) + rhs(rhsIdx) + (sum >> big_baseLog);
        ret(lhsIdx) = sum & big_baseMask
      };

      // Copy remainder of longer number while carry propagation is required
      var carry = sum >= big_base;
      while (lhsIdx > 0 && carry) {
        lhsIdx = lhsIdx - 1;
        ret(lhsIdx) = (lhs(lhsIdx) + 1) & big_baseMask;
        carry = (ret(lhsIdx) == 0)
      };

      // Copy remainder of longer number
      while (lhsIdx > 0) {
        lhsIdx = lhsIdx - 1;
        ret(lhsIdx) = lhs(lhsIdx)
      };

      // Grow result if necessary
      if (carry) {
        val bigger = new Array[Int](ret.length + 1);
        var i = 0;
        bigger(0) = 1;
        while (i < ret.length) {
          bigger(i + 1) = ret(i);
          i = i + 1
        };
        f(bigger)
      } else {
        f(ret)
      }
    }
  };

  def big_withSubMag[U](big: Array[Int], lil: Array[Int], f: Array[Int] => U): U = {
    var bigIdx = big.length;
    val ret = new Array[Int](bigIdx);
    var lilIdx = lil.length;
    var diff = 0;

    // Subtract common parts of both numbers
    while(lilIdx > 0) {
      bigIdx = bigIdx - 1;
      lilIdx = lilIdx - 1;
      diff = big(bigIdx) - lil(lilIdx) + (if (diff < 0) -1 else 0);
      ret(bigIdx) = (diff + big_base) & big_baseMask
    };

    // Subtract remainder of longer number while borrow propagates
    var borrow = (diff < 0);
    while (bigIdx > 0 && borrow) {
      bigIdx = bigIdx - 1;
      ret(bigIdx) = (big(bigIdx) + big_baseMask) & big_baseMask;
      borrow = (big(bigIdx) == 0)
    };

    // Copy remainder of longer number
    while (bigIdx > 0) {
      bigIdx = bigIdx - 1;
      ret(bigIdx) = big(bigIdx)
    };

    f(ret)
  };

  def big_withMulMag[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    val ret = new Array[Int](lhs.length + rhs.length);
    var lhsIdx = lhs.length - 1;
    var rhsIdx = rhs.length - 1;
    ret(ret.length - 1) = 0;
    var i = lhsIdx;
    while (i >= 0) {
      var carry = 0;
      var j = rhsIdx;
      var k = rhsIdx + 1 + i;
      while (j >= 0) {
        val product = lhs(i) * rhs(j) + ret(k) + carry;
        ret(k) = product & big_baseMask;
        carry = product >> big_baseLog;
        k = k - 1;
        j = j - 1
      };
      ret(i) = carry & big_baseMask;
      i = i - 1
    };
    f(ret)
  };

  def big_withDivMag[U](lhs: Array[Int], rhs: Array[Int], f: Array[Int] => U): U = {
    val lhsLog = big_logBaseMag(lhs);
    val rhsLog = big_logBaseMag(rhs);
    val loLog = lhsLog - rhsLog - (if (lhsLog != rhsLog) 1 else 0);
    val hiLog = lhsLog - rhsLog + 1;

    // TODO(lo-prio) try pruning hi > big_base or lo <= big_base

    def binarySearch(lo: Array[Int], hi: Array[Int]): U = {
      if (cmpBig(lo, hi) < 0) {
        big_withAddMag[U](hi, one, (hiPlus1: Array[Int]) => {
          big_withAddMag[U](lo, hiPlus1, (twiceMid: Array[Int]) => {
            big_withDiv2Mag[U](twiceMid, (mid: Array[Int]) => {
              withMulBig[U](mid, rhs, (product: Array[Int]) => {
                if (cmpBig(product, lhs) <= 0)
                  binarySearch(mid, hi)
                else big_withSubMag[U](mid, one, (hiNext: Array[Int]) => {
                  binarySearch(lo, hiNext)
                })
              })
            })
          })
        })
      } else f(lo)
    };
    big_withPowBase[U](loLog, (lo: Array[Int]) => {
      big_withPowBase[U](hiLog, (hi: Array[Int]) => {
        binarySearch(lo, hi)
      })
    })
  };

  def big_withDiv2Mag[U](lhs: Array[Int], f: Array[Int] => U): U = {
    if (lhs.length == 1) {
      val ret = new Array[Int](1);
      ret(0) = lhs(0) / 2;
      f(ret)
    } else {
      var borrow = (if (lhs(0) == 1) 1 else 0);
      var lhsBeg = borrow;
      var lhsEnd = lhs.length;
      val ret = new Array[Int](lhsEnd - lhsBeg);
      var lhsIdx = lhsBeg;
      while (lhsIdx < lhsEnd) {
        val d = lhs(lhsIdx) + (borrow << big_baseLog);
        ret(lhsIdx - lhsBeg) = d / 2;
        borrow = d & 1;
        lhsIdx = lhsIdx + 1
      };
      f(ret)
    }
  };

  def big_withNoLeadingZeroes[U](n: Array[Int], f: Array[Int] => U): U = {
    var beg = 0;
    while (beg < n.length && n(beg) == 0) {
      beg = beg + 1
    };

    val ret = new Array[Int](n.length - beg);
    var i = 0;
    while (i < ret.length) {
      ret(i) = n(beg + i);
      i = i + 1
    };
    f(ret)
  };

  def big_logBaseMag(n: Array[Int]): Int = n.length - 1;
  def big_withPowBase[U](i: Int, f: Array[Int] => U): U = {
    val ret = new Array[Int](1 + i);
    ret(0) = 1;
    f(ret)
  };

  def big_sgn(n: Array[Int]): Int = {
    if (n.length == 0 || n(0) == 0) 0
    else if (n(0) > 0) 1
    else -1
  };

  // XXX encoding sign in array is _very fragile_ combined with CPS style e.g.:
  //
  //   withOp1(mut, rhs) { ret =>  // may mutate the sign of mut
  //     ...
  //     withOp2(lhs, mut) { ret2 => // ouch, what if lhs was negative?
  //   }

  def withSgn[U](n: Array[Int], f: Int => U): U = {
    val sign = big_sgn(n);
    if (sign != 0) {
      val n0 = n(0);
      n(0) = n0 * sign;
      val ret = f(sign);
      n(0) = n0;
      ret
    } else { // n could be empty
      f(sign)
    }
  };

  def big_sgnSet(n: Array[Int], s: Int): Unit = {
    // TODO what if s == 0 and n non-empty?
    if (s == 0) n(0) = 0
    else if (n.length != 0) n(0) = big_sgnProd(s, big_cmpElem(n(0), 0)) * n(0)
  };
