package dev.bosatsu

import java.math.BigInteger

/** Numeric module with Double type for floating-point arithmetic.
  *
  * Operations bypass RingOpt algebraic expansion - preserves original formulas
  * for simulation code generation.
  */
object Numeric {

  /** Loads a file *at compile time* as a means of embedding external files into
    * strings. This lets us avoid resources which compilicate matters for
    * scalajs.
    */
  private[bosatsu] inline def loadFileInCompile(file: String): String =
    ${ Macro.loadFileInCompileImpl('file) }

  /** String representation of the numeric module */
  val numericString: String =
    loadFileInCompile("core/src/main/resources/bosatsu/numeric.bosatsu")

  val packageName: PackageName =
    PackageName.parse("Bosatsu/Numeric").get

  val jvmExternals: Externals =
    Externals.empty
      .add(packageName, "+.", FfiCall.Fn2(NumericImpl.add(_, _)))
      .add(packageName, "-.", FfiCall.Fn2(NumericImpl.sub(_, _)))
      .add(packageName, "*.", FfiCall.Fn2(NumericImpl.times(_, _)))
      .add(packageName, "/.", FfiCall.Fn2(NumericImpl.div(_, _)))
      .add(packageName, "from_Int", FfiCall.Fn1(NumericImpl.fromInt(_)))
      .add(packageName, "to_Int", FfiCall.Fn1(NumericImpl.toInt(_)))
      .add(packageName, "cmp_Double", FfiCall.Fn2(NumericImpl.cmp(_, _)))
      .add(packageName, "eq_Double", FfiCall.Fn2(NumericImpl.eq(_, _)))
      .add(packageName, "neg_Double", FfiCall.Fn1(NumericImpl.neg(_)))
      .add(packageName, "abs_Double", FfiCall.Fn1(NumericImpl.abs(_)))
}

object NumericImpl {

  import Value._

  private def d(v: Value): Double = v match {
    case ExternalValue(d: java.lang.Double) => d.doubleValue
    case other =>
      sys.error(s"expected Double, got ${other.getClass.getSimpleName}")
  }

  private def wrap(d: Double): Value =
    ExternalValue(java.lang.Double.valueOf(d))

  def add(a: Value, b: Value): Value = wrap(d(a) + d(b))
  def sub(a: Value, b: Value): Value = wrap(d(a) - d(b))
  def times(a: Value, b: Value): Value = wrap(d(a) * d(b))

  /** Division - Note: 0.0 / 0.0 produces NaN which propagates silently */
  def div(a: Value, b: Value): Value = wrap(d(a) / d(b))

  def fromInt(a: Value): Value = a match {
    case VInt(bi) => wrap(bi.doubleValue)
    case other =>
      sys.error(s"expected Int, got ${other.getClass.getSimpleName}")
  }

  def toInt(a: Value): Value =
    VInt(BigInteger.valueOf(d(a).toLong))

  def cmp(a: Value, b: Value): Value = {
    val da = d(a)
    val db = d(b)
    if (da < db) Comparison.LT
    else if (da > db) Comparison.GT
    else Comparison.EQ
  }

  def eq(a: Value, b: Value): Value =
    if (d(a) == d(b)) True else False

  def neg(a: Value): Value = wrap(-d(a))
  def abs(a: Value): Value = wrap(math.abs(d(a)))
}
