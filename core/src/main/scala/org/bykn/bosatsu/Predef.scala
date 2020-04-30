package org.bykn.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated}
import fastparse.all._
import java.math.BigInteger
import scala.collection.immutable.SortedMap
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
      .add(packageName, "clear_Dict", FfiCall.Fn1(PredefImpl.clear_Dict(_)))
      .add(packageName, "empty_Dict", FfiCall.Fn1(PredefImpl.empty_Dict(_)))
      .add(packageName, "add_key", FfiCall.Fn3(PredefImpl.add_key(_, _, _)))
      .add(packageName, "get_key", FfiCall.Fn2(PredefImpl.get_key(_, _)))
      .add(packageName, "items", FfiCall.Fn1(PredefImpl.items(_)))
      .add(packageName, "remove_key", FfiCall.Fn2(PredefImpl.remove_key(_, _)))
      .add(packageName, "concat_String", FfiCall.Fn1(PredefImpl.concat_String(_)))
      .add(packageName, "make_vis", FfiCall.ExprFn(PredefImpl.VisWrapper(_)))

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

  def div(a: Value, b: Value): Value = {
    val bi = i(b)
    if (bi.equals(BigInteger.ZERO)) VOption.none
    else VOption.some(VInt(i(a).divide(bi)))
  }

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
    VInt(i(a).mod(i(b).abs()))

  def gcd_Int(a: Value, b: Value): Value =
    VInt(i(a).gcd(i(b)))

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

  def clear_Dict(dictv: Value): Value = {
    val d = toDict(dictv)
    val ord = d.ordering
    ExternalValue(SortedMap.empty[Value, Value](ord))
  }


  def empty_Dict(ord: Value): Value = {
    implicit val ordValue: Ordering[Value] =
      new Ordering[Value] {
        val fnV = ord.asFn
        def compare(a: Value, b: Value): Int = {
          fnV(a).asFn(b).asSum.variant - 1
        }
      }
    ExternalValue(SortedMap.empty[Value, Value])
  }

  def toDict(v: Value): SortedMap[Value, Value] =
    v.asExternal.toAny.asInstanceOf[SortedMap[Value, Value]]

  def add_key(dict: Value, k: Value, value: Value): Value =
    ExternalValue(toDict(dict).updated(k, value))

  def get_key(dict: Value, k: Value): Value =
    toDict(dict).get(k) match {
      case None => VOption.none
      case Some(v) => VOption.some(v)
    }

  def remove_key(dict: Value, k: Value): Value =
    ExternalValue(toDict(dict) - k)

  def items(dict: Value): Value = {
    val d = toDict(dict)
    Value.VList(d.iterator.map { case (k, v) =>
      ProductValue.fromList(k :: ProductValue.fromList(v :: Nil) :: Nil)
    }
    .toList)
  }


  case class VisWrapper(vis: NormalEvaluation.NormalValue)
}

