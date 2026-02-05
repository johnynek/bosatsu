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
      // Arithmetic operators
      .add(packageName, "+.", FfiCall.Fn2(NumericImpl.add(_, _)))
      .add(packageName, "-.", FfiCall.Fn2(NumericImpl.sub(_, _)))
      .add(packageName, "*.", FfiCall.Fn2(NumericImpl.times(_, _)))
      .add(packageName, "/.", FfiCall.Fn2(NumericImpl.div(_, _)))
      // Conversion
      .add(packageName, "from_Int", FfiCall.Fn1(NumericImpl.fromInt(_)))
      .add(packageName, "to_Int", FfiCall.Fn1(NumericImpl.toInt(_)))
      .add(packageName, "double_to_String", FfiCall.Fn1(NumericImpl.doubleToString(_)))
      .add(packageName, "string_to_Double", FfiCall.Fn1(NumericImpl.stringToDouble(_)))
      // Comparison
      .add(packageName, "cmp_Double", FfiCall.Fn2(NumericImpl.cmp(_, _)))
      .add(packageName, "eq_Double", FfiCall.Fn2(NumericImpl.eq(_, _)))
      // Unary operations
      .add(packageName, "neg_Double", FfiCall.Fn1(NumericImpl.neg(_)))
      .add(packageName, "abs_Double", FfiCall.Fn1(NumericImpl.abs(_)))
      // Trigonometric functions
      .add(packageName, "sin", FfiCall.Fn1(NumericImpl.sin(_)))
      .add(packageName, "cos", FfiCall.Fn1(NumericImpl.cos(_)))
      .add(packageName, "tan", FfiCall.Fn1(NumericImpl.tan(_)))
      // Power and exponential functions
      .add(packageName, "sqrt", FfiCall.Fn1(NumericImpl.sqrt(_)))
      .add(packageName, "pow", FfiCall.Fn2(NumericImpl.pow(_, _)))
      .add(packageName, "exp", FfiCall.Fn1(NumericImpl.exp(_)))
      .add(packageName, "log", FfiCall.Fn1(NumericImpl.log(_)))
      // Rounding functions
      .add(packageName, "floor", FfiCall.Fn1(NumericImpl.floor(_)))
      .add(packageName, "ceil", FfiCall.Fn1(NumericImpl.ceil(_)))
      .add(packageName, "round", FfiCall.Fn1(NumericImpl.round(_)))
      // Random number generation
      .add(packageName, "random", FfiCall.Fn1(NumericImpl.random(_)))
      .add(packageName, "random_range", FfiCall.Fn2(NumericImpl.randomRange(_, _)))
      // Min/max
      .add(packageName, "min_Double", FfiCall.Fn2(NumericImpl.minDouble(_, _)))
      .add(packageName, "max_Double", FfiCall.Fn2(NumericImpl.maxDouble(_, _)))
      // Constants
      .add(packageName, "pi", FfiCall.Fn0(() => NumericImpl.pi))
      .add(packageName, "e_const", FfiCall.Fn0(() => NumericImpl.eConst))
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
    // Use Java's Double.compare for a proper total ordering that handles NaN.
    // This maintains antisymmetry: NaN is considered greater than all other values,
    // so cmp(NaN, x) = GT and cmp(x, NaN) = LT for any non-NaN x.
    // Also handles: -0.0 < +0.0, and NaN == NaN for ordering purposes.
    java.lang.Double.compare(d(a), d(b)) match {
      case n if n < 0 => Comparison.LT
      case n if n > 0 => Comparison.GT
      case _          => Comparison.EQ
    }
  }

  def eq(a: Value, b: Value): Value =
    if (d(a) == d(b)) True else False

  def neg(a: Value): Value = wrap(-d(a))
  def abs(a: Value): Value = wrap(math.abs(d(a)))

  // String conversion
  def doubleToString(a: Value): Value = Str(d(a).toString)

  def stringToDouble(a: Value): Value = a match {
    case Str(s) =>
      try wrap(s.toDouble)
      catch { case _: NumberFormatException => wrap(0.0) }
    case other =>
      sys.error(s"expected String, got ${other.getClass.getSimpleName}")
  }

  // Trigonometric functions
  def sin(a: Value): Value = wrap(math.sin(d(a)))
  def cos(a: Value): Value = wrap(math.cos(d(a)))
  def tan(a: Value): Value = wrap(math.tan(d(a)))

  // Power and exponential functions
  def sqrt(a: Value): Value = wrap(math.sqrt(d(a)))
  def pow(base: Value, exp: Value): Value = wrap(math.pow(d(base), d(exp)))
  def exp(a: Value): Value = wrap(math.exp(d(a)))
  def log(a: Value): Value = wrap(math.log(d(a)))

  // Rounding functions
  def floor(a: Value): Value = wrap(math.floor(d(a)))
  def ceil(a: Value): Value = wrap(math.ceil(d(a)))
  def round(a: Value): Value = wrap(math.round(d(a)).toDouble)

  // Random number generation
  private val rng = new java.util.Random()
  def random(unit: Value): Value = wrap(rng.nextDouble())
  def randomRange(min: Value, max: Value): Value = {
    val minVal = d(min)
    val maxVal = d(max)
    wrap(minVal + rng.nextDouble() * (maxVal - minVal))
  }

  // Min/max
  def minDouble(a: Value, b: Value): Value = wrap(math.min(d(a), d(b)))
  def maxDouble(a: Value, b: Value): Value = wrap(math.max(d(a), d(b)))

  // Constants
  val pi: Value = wrap(math.Pi)
  val eConst: Value = wrap(math.E)
}
