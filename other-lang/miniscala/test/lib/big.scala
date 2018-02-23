package lib
package big

import org.junit.Assert._
import org.junit.Test

class ToUtil[M <: Big](protected val module: M) { // self: Facade[M#I] =>
  import module._

  def toNonCps[U, @specialized(Int) T](fn: T => (I => Any) => U)(src: T): I = {
    var ret: I = uninit
    fn(src) { ret = _ }
    ret
  }

  def toBig(i: Int) = toNonCps(fromInt _)(i)
  def toBig(l: Long) = ???
}

// ArrayInt tests

class ArrayIntConversionTo extends ToUtil(ArrayInt) {
  import module._

  @Test def test1Digit = {
    val digit = base * 2 / 3
    assertArrayEquals(I(digit), toBig(digit))
  }

  @Test def test2Digit = {
    assertArrayEquals(I(1, 0), toBig(1 << baseLog))
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
  import ArrayInt._

  @Test def testZero = {
    assertEquals(0, toInt(I(0)))
  }

  @Test def test1Digit = {
    val digit = base * 2 / 3
    assertEquals(digit, toInt(I(digit)))
  }

  @Test def test2Digit = {
    assertEquals(1 << baseLog, toInt(I(1, 0)))
  }
}

class ArrayIntConversionFromAndToInt extends ToUtil(ArrayInt) {
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

// ListInt tests

// XXX figure out how to parametrize tests with a class in JUnit
// (the inheritance does not work well with JUnit)

class ListIntConversionTo extends ToUtil(ListInt) {
  import module._

  @Test def test1Digit = {
    val digit = base * 2 / 3
    assertEquals(I(digit), toBig(digit))
  }

  @Test def test2Digit = {
    assertEquals(I(0, 1), toBig(1 << baseLog))
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
  import ListInt._

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

class ListIntConversionFromAndToInt extends ToUtil(ListInt) {
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
