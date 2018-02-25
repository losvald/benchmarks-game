package lib
package big

import scala.util.Random

import org.junit.Assert._
import org.junit.Test

class ToUtil[M <: Big](protected val module: M) { // self: Facade[M#I] =>
  import module._

  def toNonCps[U, @specialized(Int) T](fn: (T, I => Any) => U)(src: T): I = {
    var ret: I = uninit
    fn(src, { ret = _ })
    ret
  }

  def toBig(i: Int) = toNonCps(fromInt _)(i)
  def toBig(l: Long) = ???

  def toBigPairs(ints: (Int, Int)*) = {
    for ((lhsInt, rhsInt) <- ints) yield (toBig(lhsInt), toBig(rhsInt))
  }
}

// ArrayInt tests

object Base16ArrayInt extends BaselessArrayInt {
  override lazy val baseLog = 4
}

class ArrayIntConversionTo extends ToUtil(Base16ArrayInt) {
  import module._

  @Test def test1Digit = {
    val digit = base * 2 / 3
    assertArrayEquals(I(digit), toBig(digit))
  }

  @Test def test2DigitPos = {
    assertArrayEquals(I(1, 0), toBig(1 << baseLog))
    assertArrayEquals(I(0xA, 0x5), toBig(0xA5))
  }

  @Test def test2DigitNeg = {
    assertArrayEquals(I(-1, 2), toBig(-(1 << baseLog | 2)))
  }

  @Test def test3Digit = {
    val lo = 1
    val md = base / 2
    val hi = base - 1
    assertArrayEquals(I(hi, md, lo), toBig(
      ((hi << baseLog) << baseLog) | (md << baseLog) | lo))
  }

  @Test def test5Digit = {
    val d = IndexedSeq.tabulate(5)((i: Int) => base * (i + 1) / 5 - 1)
    assertArrayEquals(I(d(4), d(3), d(2), d(1), d(0)), toBig(
      d(4) << 4*baseLog | d(3) << 3*baseLog | d(2) << 2*baseLog |
      d(1) << baseLog | d(0)))
  }
}

class ArrayIntConversionFrom {
  import Base16ArrayInt._

  @Test def testZero = {
    assertEquals(0, toInt(I(0)))
  }

  @Test def test1Digit = {
    val digit = base * 2 / 3
    assertEquals(digit, toInt(I(digit)))
  }

  @Test def test2DigitPos = {
    assertEquals(1 << baseLog, toInt(I(1, 0)))
  }

  @Test def test2DigitNeg = {
    assertEquals(-(1 << baseLog | 2), toInt(I(-1, 2)))
  }
}

class ArrayIntConversionFromAndToInt extends ToUtil(Base16ArrayInt) {
  import module._

  def reconvert(i: Int) = toInt(toBig(i))

  @Test def testZero = {
    assertEquals(0, reconvert(0))
  }

  @Test def testOne = {
    assertEquals(1, reconvert(1))
  }

  @Test def test1Digit = {
    assertEquals(base - 1, reconvert(base - 1))
  }

  @Test def testNoTrailingZero = {
    for (i <- Seq(0x1337, 0xDEA1))
      assertEquals(i, reconvert(i))
  }

  @Test def testTrailingZeroes = {
    for (i <- Seq(0x30, 0x4000))
      assertEquals(i, reconvert(i))
  }

  @Test def testNegative = {
    for (i <- Seq(-0x1, -0xA0FEED))
      assertEquals(i, reconvert(i))
  }
}

class ArrayIntSignedness {
  import Base16ArrayInt._

  @Test def testSgnPos = {
    assertEquals(+1, sgn(I(4, 3, 2)))
    assertEquals(+1, sgn(I(1)))
  }

  @Test def testSgnNeg = {
    assertEquals(-1, sgn(I(-7, 6, 5)))
    assertEquals(-1, sgn(I(-1)))
  }

  @Test def testSgnZeroEmpty = {
    assertEquals(0, sgn(I()))
  }

  @Test def testSgnZeroNonEmpty = {
    assertEquals(0, sgn(I(0)))
  }

  @Test def testWithSgnNeg = {
    val n = I(-2, 3)
    withSgn(n, { sgn =>
      assertEquals(-1, sgn)
      assertArrayEquals(I(2, 3), n)
    })
    assertArrayEquals(I(-2, 3), n)
  }

  @Test def testWithSgnPos = {
    val n = I(5)
    withSgn(n, { sgn =>
      assertEquals(+1, sgn)
      assertArrayEquals(I(5), n)
    })
    assertArrayEquals(I(5), n)
  }

  @Test def testSgnSetPosOnNeg = {
    val n = I(-3, 2)
    sgnSet(n, +1)
    assertArrayEquals(I(3, 2), n)
  }

  @Test def testSgnSetPosOnPos = {
    val n = I(2, 0)
    sgnSet(n, +1)
    assertArrayEquals(I(2, 0), n)
  }

  @Test def testSgnSetNegOnPos = {
    val n = I(4, 5)
    sgnSet(n, -1)
    assertArrayEquals(I(-4, 5), n)
  }

  @Test def testSgnSetNegOnNeg = {
    val n = I(-3, 1)
    sgnSet(n, -1)
    assertArrayEquals(I(-3, 1), n)
  }
}

class ArrayIntMul extends ToUtil(Base16ArrayInt) {
  import module._

  @Test def testMul1ByManyDigitNoCarry = {
    withMul(toBig(0x2), toBig(0x153), { act =>
      assertEquals(repr(toBig(0x2A6)), repr(act))
    })
  }

  @Test def testMulManyBy1DigitNoCarry = {
    withMul(toBig(0x503), toBig(3), { act =>
      assertEquals(repr(toBig(0xF09)), repr(act))
    })
  }

  @Test def testMul3by2NoCarryEitherNeg = {
    val exp = I(-6, 0xB, 7, 4)
    withMul(toBig(-0x211), toBig(0x34), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
    withMul(toBig(0x211), toBig(-0x34), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testMulCarryNoExtraDigit = {
    val exp = toBig(0xD6E2E2)
    withMul(toBig(0xCAFE), toBig(0x10F), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
    withMul(toBig(0x10F), toBig(0xCAFE), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testMulWithCarryAndExtraDigitBothNeg = {
    val exp = I(0xA, 6, 1, 4, 4, 9, 8, 3)
    withMul(toBig(-0xDEAD), toBig(-0xBEEF), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testRandom = {
    val rng = new Random(0)
    for (itr <- 1 to 100) {
      val lhs = rng.nextInt & 0x3FFF
      val rhs = rng.nextInt & 0x3FFF
      withMul(toBig(lhs), toBig(rhs), { act =>
        val exp = toBig(lhs * rhs)
        assertArrayEquals(
          f"$lhs%X * $rhs%X\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
      })
    }
  }
}

class Cmp extends ToUtil(Base16ArrayInt) {
  import module._

  @Test def testCmpBothPosOneSmaller = {
    for ((lhs, rhs) <- toBigPairs(
      (0x34, 0x56), (0x93, 0x142), (1, 0x10), (0, 0x100), (0x12321, 0x13211)
    )) {
      assertEquals(s"${repr(lhs)} ? ${repr(rhs)}", -1, cmp(lhs, rhs))
      assertEquals(s"${repr(rhs)} ? ${repr(lhs)}", +1, cmp(rhs, lhs))
    }
  }

  @Test def testCmpBothNegOneSmaller = {
    for ((lhs, rhs) <- toBigPairs(
      (-0x34, -0x56), (-0x93, -0x142), (-1, -0x10), (0, -0x100),
      (-0x12321, -0x13211)
    )) {
      assertEquals(s"${repr(lhs)} ? ${repr(rhs)}", +1, cmp(lhs, rhs))
      assertEquals(s"${repr(rhs)} ? ${repr(lhs)}", -1, cmp(rhs, lhs))
    }
  }

  @Test def testCmpEitherNeg = {
    for ((lhs, rhs) <- toBigPairs(
      (-0x34, 0x56), (-0x142, 0x77), (-0x100, 0)
    )) {
      assertEquals(s"${repr(lhs)} ? ${repr(rhs)}", -1, cmp(lhs, rhs))
      assertEquals(s"${repr(rhs)} ? ${repr(lhs)}", +1, cmp(rhs, lhs))
    }
  }

  @Test def testCmpEqual = {
    for (n <- Seq(0x56, -0x142, 0, 1, -0x10).map(toBig(_))) {
      assertEquals(s"${repr(n)} (<= && >=)", true, (n <= n) && (n >= n))
      assertEquals(s"${repr(n)} (cmp)", 0, cmp(n, n))
      assertEquals(s"${repr(n)} (==)", true, n == n)
    }
  }
}

class ArrayIntAddAndSub extends ToUtil(Base16ArrayInt) {
  import module._

  @Test def testAdd1DigitCarryExtraDigit = {
    withAdd(I(0xA), I(0xB), { act =>
      val exp = toBig(0x15)
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testAddUnevenDigitCarryNoExtraDigit = {
    val exp = toBig(0x5920)
    withAdd(toBig(0x572B), toBig(0x1F5), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
    withAdd(toBig(0x1F5), toBig(0x572B), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testSub1BorrowFromZero = {
    withSub(toBig(0x80800), I(1), { act =>
      val exp = toBig(0x807FF)
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testSubSmallerPosLoseTwoDigits = {
    withSub(toBig(0x5678), toBig(0x55AB), { act =>
      val exp = toBig(0xCD)
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testAddEitherNegLoseTwoDigits = {
    val exp = toBig(0xCD)
    withAdd(toBig(0x1234), toBig(-0x1167), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
    withAdd(toBig(-0x1167), toBig(0x1234), { act =>
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testRandom = {
    val r = new Random(1)
    for (itr <- 1 to 100) {
      val lhs = r.nextInt & 0x3FFFFFF
      val rhs = r.nextInt & 0x3FFFFFF
      withAdd(toBig(lhs), toBig(rhs), { act =>
        val exp = toBig(lhs + rhs)
        assertArrayEquals(
          f"$lhs%X + $rhs%X\nexp: ${repr(exp)}\nact: ${repr(act)}\n",
          exp, act)
      })
      withSub(toBig(-lhs), toBig(-rhs), { act =>
        val exp = toBig(-lhs - -rhs)
        assertArrayEquals(
          f"-$lhs%X - -$rhs%X\nexp: ${repr(exp)}\nact: ${repr(act)}\n",
          exp, act)
      })
      withAdd(toBig(-lhs), toBig(rhs), { act =>
        val exp = toBig(-lhs + rhs)
        assertArrayEquals(
          f"-$lhs%X + $rhs%X\nexp: ${repr(exp)}\nact: ${repr(act)}\n",
          exp, act)
      })
      withSub(toBig(lhs), toBig(rhs), { act =>
        val exp = toBig(lhs - rhs)
        assertArrayEquals(
          f"$lhs%X - $rhs%X\nexp: ${repr(exp)}\nact: ${repr(act)}\n",
          exp, act)
      })
    }
  }
}

class ArrayIntDiv extends ToUtil(Base16ArrayInt) {
  import module._

  @Test def test1DigitMagDiv2 = {
    for (i <- Seq(1, 3, 4, 0xA, 0xF))
      withDiv2Mag(I(i), { act => assertArrayEquals(I(i / 2), act) })
  }

  @Test def testManyDigitMagDiv2 = {
    for {
      half <- Seq(0xDEAD, 0xBEEF, 0x1002); exp = toBig(half)
      n <- Seq(2*half, 2*half + 1).map(toBig(_))
    } {
      withDiv2Mag(n, { act =>
        assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
      })
    }
  }

  @Test def testMagDiv2Borrow = {
    withDiv2Mag(toBig(0xBBBC), { act =>
      val exp = toBig(0x5DDE)
      assertArrayEquals(s"\nexp: ${repr(exp)}\nact: ${repr(act)}\n", exp, act)
    })
  }

  @Test def testManyDigitMagDiv1Digit = {
    for {
      digit <- Seq(3, 5, 0xA)
      factor <- Seq(0xDEAD, 0xBEEF, 0x1002); exp = toBig(factor)
      i = factor * digit
    } {
      withDivMag(toBig(i), I(digit), { act =>
        assertArrayEquals(
          f"$i%X/$digit%X\nexp: ${repr(exp)}\nact: ${repr(act)}\n",
          exp, act)
      })
    }
  }

  @Test def testManyDigitDivManyDigitBothPos = {
    for ((lhs, rhs) <- toBigPairs(
      (0x102FB, 0x23), (0xCAFE100, 0x807), (0x101, 0x81), (0x3010CAB, 0x2345),
      (0x3098, 0x3100), (0x344, 0x344)
    )) {
      withDiv(lhs, rhs, { act =>
        val exp = toBig(toInt(lhs) / toInt(rhs))
        assertArrayEquals(
          s"${repr(lhs)}/${repr(rhs)}\nexp: ${repr(exp)}\nact: ${repr(act)}\n",
          exp, act)
      })
    }
  }

  @Test def testRandom = {
    val r = new Random(1)
    for (itr <- 1 to 200) {
      val lhsInt = r.nextInt & 0x7FFFFFFF
      val rhsInt = r.nextInt(lhsInt) + 1
      val (lhs, rhs) = (toBig(lhsInt), toBig(rhsInt))
      val exp = toBig(lhsInt / rhsInt)
      withDiv(lhs, rhs, { act =>
        assertArrayEquals(
          s"${repr(lhs)}/${repr(rhs)}\nexp: ${repr(exp)}\nact: ${repr(act)}\n",
          exp, act)
      })
    }
  }
}


// ListInt tests

// XXX figure out how to parametrize tests with a class in JUnit
// (the inheritance does not work well with JUnit)

object Base16ListInt extends BaselessListInt {
  private[big] val baseLog = 4
}

class ListIntConversionTo extends ToUtil(Base16ListInt) {
  import module._

  @Test def test1Digit = {
    val digit = base * 2 / 3
    assertEquals(I(digit), toBig(digit))
  }

  @Test def test2Digit = {
    assertEquals(I(0, 1), toBig(1 << baseLog))
    assertEquals(I(6, 5), toBig(0x56))
  }

  @Test def test3Digit = {
    val lo = 1
    val md = base / 2
    val hi = base - 1
    assertEquals(I(lo, md, hi), toBig(
      ((hi << baseLog) << baseLog) | (md << baseLog) | lo))
  }

  @Test def test5Digit = {
    val d = IndexedSeq.tabulate(5)((i: Int) => base * (i + 1) / 5 - 1)
    assertEquals(I(d(0), d(1), d(2), d(3), d(4)), toBig(
      d(4) << 4*baseLog | d(3) << 3*baseLog | d(2) << 2*baseLog |
      d(1) << baseLog | d(0)))
  }
}

class ListIntConversionFrom {
  import Base16ListInt._

  @Test def testZero = {
    assertEquals(0, toInt(I(0)))
  }

  @Test def test1Digit = {
    val digit = base * 2 / 3
    assertEquals(digit, toInt(I(digit)))
  }

  @Test def test2Digit = {
    assertEquals(1 << baseLog, toInt(I(0, 1)))
  }
}

class ListIntConversionFromAndToInt extends ToUtil(Base16ListInt) {
  import module._

  def reconvert(i: Int) = toInt(toBig(i))

  @Test def testZero = {
    assertEquals(0, reconvert(0))
  }

  @Test def testOne = {
    assertEquals(1, reconvert(1))
  }

  @Test def test1Digit = {
    assertEquals(base - 1, reconvert(base - 1))
  }
}
