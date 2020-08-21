package org.bykn.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated}
import fastparse.all._
import java.math.BigInteger
import language.experimental.macros

import IorMethods.IorExtension

object Predef {
  /**
   * Loads a file *at compile time* as a means of embedding
   * external files into strings. This lets us avoid resources
   * which compilicate matters for scalajs.
   */
  private[bosatsu] def loadFileInCompile(file: String): String = macro Macro.smac

  /**
   * String representation of the predef
   */
  val predefString: String =
    loadFileInCompile("core/src/main/resources/bosatsu/predef.bosatsu")

  /**
   * The parsed representation of the predef.
   */
  val predefPackage: Package.Parsed =
    Package.parser(None).parse(predefString) match {
      case Parsed.Success(pack, _) => pack
      case Parsed.Failure(exp, idx, extra) =>
        val lm = LocationMap(predefString)
        sys.error(s"couldn't parse predef: ${lm.showContext(idx, 2, LocationMap.Colorize.None)} with trace: ${extra.traced.trace}")
    }

  /**
   * Here is the fully compiled Predef
   */
  val predefCompiled: Package.Inferred = {
    import DirectEC.directEC

    implicit val showUnit: Show[Unit] = Show.show[Unit](_ => "predefCompiled")
    val inferred = PackageMap.resolveThenInfer(((), predefPackage) :: Nil, Nil).strictToValidated

    inferred match {
      case Validated.Valid(v) =>
        v.toMap.get(packageName) match {
          case None => sys.error("internal error: predef package not found after compilation")
          case Some(inf) => inf
        }
      case Validated.Invalid(errs) =>
        sys.error(s"expected no errors, found: $errs")
    }
  }

  private def packageName: PackageName =
    PackageName.PredefName

  val predefImportList = predefCompiled.exports
    .map(_.name)
    .distinct
    .sorted
    .map(ImportedName.OriginalName(_, ()))

  val predefImports: Import[PackageName, Unit] =
    Import(packageName, NonEmptyList.fromList(predefImportList).get)

  val jvmExternals: Externals =
    Externals
      .empty
      .add(packageName, "add", FfiCall.Fn2(PredefImpl.add(_, _)))
      .add(packageName, "div", FfiCall.Fn2(PredefImpl.div(_, _)))
      .add(packageName, "sub", FfiCall.Fn2(PredefImpl.sub(_, _)))
      .add(packageName, "times", FfiCall.Fn2(PredefImpl.times(_, _)))
      .add(packageName, "eq_Int", FfiCall.Fn2(PredefImpl.eq_Int(_, _)))
      .add(packageName, "cmp_Int", FfiCall.Fn2(PredefImpl.cmp_Int(_, _)))
      .add(packageName, "gcd_Int", FfiCall.Fn2(PredefImpl.gcd_Int(_, _)))
      .add(packageName, "mod_Int", FfiCall.Fn2(PredefImpl.mod_Int(_, _)))
      .add(packageName, "int_loop", FfiCall.Fn3(PredefImpl.intLoop(_, _, _)))
      .add(packageName, "int_to_String", FfiCall.Fn1(PredefImpl.int_to_String(_)))
      .add(packageName, "trace", FfiCall.Fn2(PredefImpl.trace(_, _)))
      .add(packageName, "string_Order_fn", FfiCall.Fn2(PredefImpl.string_Order_Fn(_, _)))
      .add(packageName, "concat_String", FfiCall.Fn1(PredefImpl.concat_String(_)))

  def withPredef(ps: List[Package.Parsed]): List[Package.Parsed] =
    predefPackage :: ps.map(_.withImport(predefImports))

  def withPredefA[A](predefA: A, ps: List[(A, Package.Parsed)]): List[(A, Package.Parsed)] =
    (predefA, predefPackage) :: withPredefImportsA(ps)

  def withPredefImportsA[A](ps: List[(A, Package.Parsed)]): List[(A, Package.Parsed)] =
    ps.map { case (a, p) => (a, p.withImport(predefImports)) }
}

object PredefImpl {

  import Value._

  private def i(a: Value): BigInteger =
    a match {
      case VInt(bi) => bi
      case other => sys.error(s"expected integer: $a")
    }

  def add(a: Value, b: Value): Value =
    VInt(i(a).add(i(b)))

  def divBigInteger(a: BigInteger, b: BigInteger): BigInteger = {
    if (b == BigInteger.ZERO) BigInteger.ZERO
    else if (b == BigInteger.ONE) a
    else {
      val mod = modBigInteger(a, b)
      a.subtract(mod).divide(b)
    }
  }

  def modBigInteger(a: BigInteger, b: BigInteger): BigInteger = {
    val s = b.signum
    if (s == 0) a
    else if (s > 0) a.mod(b)
    else {
      // java does not support negative modulus
      // d = a / b
      // we want a = d * b + (a % b)
      // and sign of (a % b).signum == b.signum
      // so (a % b) = a - d * b
      val res0 = a.remainder(b)
      val sr = res0.signum
      if (sr == 0) res0
      else if (res0.signum == s) res0
      else res0.add(b)
    }
  }

  def div(a: Value, b: Value): Value =
    VInt(divBigInteger(i(a), i(b)))

  def sub(a: Value, b: Value): Value =
    VInt(i(a).subtract(i(b)))

  def times(a: Value, b: Value): Value =
    VInt(i(a).multiply(i(b)))

  def eq_Int(a: Value, b: Value): Value =
    // since we have already typechecked, standard equals works
    if (a.equals(b)) True else False

  def cmp_Int(a: Value, b: Value): Value =
    Comparison.fromInt(i(a).compareTo(i(b)))

  def mod_Int(a: Value, b: Value): Value =
    VInt(modBigInteger(i(a), i(b)))

  def gcdBigInteger(a: BigInteger, b: BigInteger): BigInteger = {
    @annotation.tailrec
    def gcd(a: BigInteger, b: BigInteger): BigInteger =
      if (b == BigInteger.ZERO) a
      else {
        gcd(b, modBigInteger(a, b))
      }

    if (b.signum > 0) a.gcd(b)
    else gcd(a, b)
  }

  def gcd_Int(a: Value, b: Value): Value =
    VInt(gcdBigInteger(i(a), i(b)))

  //def intLoop(intValue: Int, state: a, fn: Int -> a -> TupleCons[Int, TupleCons[a, Unit]]) -> a
  final def intLoop(intValue: Value, state: Value, fn: Value): Value = {
    val fnT = fn.asFn

    @annotation.tailrec
    def loop(biValue: Value, bi: BigInteger, state: Value): Value =
      if (bi.compareTo(BigInteger.ZERO) <= 0) state
      else {
        val fn0 = fnT(biValue).asFn
        fn0(state) match {
          case ConsValue(nextI, ConsValue(ConsValue(nextA, _), _)) =>
            val n = i(nextI)
            if (n.compareTo(bi) >= 0) {
              // we are done in this case
              nextA
            }
            else loop(nextI, n, nextA)
          case other => sys.error(s"unexpected ill-typed value: at $bi, $state, $other")
        }
      }

    loop(intValue, i(intValue), state)
  }

  final def int_to_String(intValue: Value): Value =
    Value.Str(i(intValue).toString)

  def trace(prefix: Value, v: Value): Value = {
    val Value.Str(prestr) = prefix
    println(s"$prestr: $v")
    v
  }

  def string_Order_Fn(a: Value, b: Value): Value =
    (a, b) match {
      case (Value.Str(sa), Value.Str(sb)) =>
        Value.Comparison.fromInt(sa.compareTo(sb))
      case other => sys.error(s"type error: $other")
    }

  def concat_String(items: Value): Value =
    items match {
      case Value.VList(parts) =>
        Value.Str(parts.iterator.map {
          case Value.Str(s) => s
          case other =>
            //$COVERAGE-OFF$
            sys.error(s"type error: $other")
            //$COVERAGE-ON$
        }
        .mkString)

      case other =>
        //$COVERAGE-OFF$
        sys.error(s"type error: $other")
        //$COVERAGE-ON$
    }
}

