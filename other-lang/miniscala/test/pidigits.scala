package pidigits

import org.junit.Assert._
import org.junit.{ Before, Test }

protected abstract class AbstractTest {
  def mkSHA1Checker = {
    val md = java.security.MessageDigest.getInstance("SHA1")
    var i5 = 0
    (n: Int) => {
      md.update(f"$n%05d".getBytes)
      i5 += 1
      val i = i5 * 5
      val exp = if (i == 10) "814cb618ea70c57355f99cbcde0fdae02c20390f"
      else if (i == 20) "bc2781bb0ad962b5b94783dda164c837f3e301cc"
      else if (i == 40) "bffe0b0a083d8b48cbdbf3d5c9df34ecbbb7cf44"
      else if (i == 60) "88eda470590984a34441f0d884476554134afb94"
      else if (i == 200) "da3c6cc6cb8c2cfd47c832ad0f1dc16e8bac3866"
      else if (i == 500) "c09d63fcaae39f2a53e3f7425b36f47e7060da86"
      else if (i == 1000) "a03730e0b961b25376bb0623a6569d61af301161"
      else if (i == 10000) "c550dbf8dd416ac6525f854f3b3f0f87c6ca992b"
      else ""
      if (exp.nonEmpty) {
        val md2 = md.clone.asInstanceOf[java.security.MessageDigest]
        val act = md2.digest.map("%02x" format _).mkString
        assertEquals(s"@$i", exp, act)
      }
    }
  }
}

class BigIntWrapperTest extends AbstractTest {
  import BigIntWrapper._

  @Test def test1000DigitsSHA = apply(1000, mkSHA1Checker)
}

class ArrayIntTest extends AbstractTest {
  import ArrayInt._

  @Test def test60DigitsSHA = apply(60, mkSHA1Checker)
  @Test def test500DigitsSHA = apply(500, mkSHA1Checker)
}
