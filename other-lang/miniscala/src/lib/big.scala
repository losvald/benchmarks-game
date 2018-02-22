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
}
