package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated}
import fastparse.all._
import java.math.BigInteger
import scala.collection.immutable.SortedMap
import language.experimental.macros

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
  lazy val predefPackage: Package.Parsed =
    Package.parser.parse(predefString) match {
      case Parsed.Success(pack, _) => pack
      case Parsed.Failure(exp, idx, extra) =>
        val lm = LocationMap(predefString)
        sys.error(s"couldn't parse predef: ${lm.showContext(idx)} with trace: ${extra.traced.trace}")
    }

  /**
   * Here is the fully compiled Predef
   */
  lazy val predefCompiled: Package.Inferred = {
    val (bad, good) = PackageMap.resolveThenInfer(((), predefPackage) :: Nil, Nil)
    require(bad.isEmpty, s"expected no bad packages, found: $bad")
    good match {
      case Validated.Valid(v) =>
        v.toMap.get(Name) match {
          case None => sys.error("internal error: predef package not found after compilation")
          case Some(inf) => inf
        }
      case Validated.Invalid(errs) =>
        sys.error(s"internal error: predef didn't compile: $errs")
    }
  }

  def packageName: PackageName =
    PackageName.predef

  // For pattern matching
  val Name: PackageName =
    PackageName.predef

  val predefImportList = predefCompiled.exports
    .map(_.name)
    .distinct
    .sorted
    .map(ImportedName.OriginalName(_, ()))

  val predefImports: Import[PackageName, Unit] =
    Import(packageName,NonEmptyList.fromList(predefImportList).get)

  def jvmExternals[T[_]](implicit
    tokenize: Evaluation.Value[T] => Json,
    predefImpl: PredefImpl[T],
    valueT: Evaluation.ValueT[T]
  ): Externals[T] =
    Externals
      .empty
      .add(packageName, "add", FfiCall.Fn2[T](predefImpl.add(_, _)))
      .add(packageName, "div", FfiCall.Fn2[T](predefImpl.div(_, _)))
      .add(packageName, "sub", FfiCall.Fn2[T](predefImpl.sub(_, _)))
      .add(packageName, "times", FfiCall.Fn2[T](predefImpl.times(_, _)))
      .add(packageName, "eq_Int", FfiCall.Fn2[T](predefImpl.eq_Int(_, _)))
      .add(packageName, "cmp_Int", FfiCall.Fn2[T](predefImpl.cmp_Int(_, _)))
      .add(packageName, "gcd_Int", FfiCall.Fn2[T](predefImpl.gcd_Int(_, _)))
      .add(packageName, "mod_Int", FfiCall.Fn2[T](predefImpl.mod_Int(_, _)))
      .add(packageName, "range", FfiCall.Fn1[T](predefImpl.range(_)))
      .add(packageName, "int_loop", FfiCall.Fn3[T](predefImpl.intLoop(_, _, _)))
      .add(packageName, "trace", FfiCall.Fn2[T](predefImpl.trace(_, _)))
      .add(packageName, "string_Order_fn", FfiCall.Fn2[T](predefImpl.string_Order_Fn(_, _)))
      .add(packageName, "clear_Dict", FfiCall.Fn1[T](predefImpl.clear_Dict(_)))
      .add(packageName, "empty_Dict", FfiCall.Fn1[T](predefImpl.empty_Dict(_)))
      .add(packageName, "add_key", FfiCall.Fn3[T](predefImpl.add_key(_, _, _)))
      .add(packageName, "get_key", FfiCall.Fn2[T](predefImpl.get_key(_, _)))
      .add(packageName, "items", FfiCall.Fn1[T](predefImpl.items(_)))
      .add(packageName, "remove_key", FfiCall.Fn2[T](predefImpl.remove_key(_, _)))

  def withPredef(ps: List[Package.Parsed]): List[Package.Parsed] =
    predefPackage :: ps.map(_.withImport(predefImports))

  def withPredefA[A](predefA: A, ps: List[(A, Package.Parsed)]): List[(A, Package.Parsed)] =
    (predefA, predefPackage) :: withPredefImportsA(ps)

  def withPredefImportsA[A](ps: List[(A, Package.Parsed)]): List[(A, Package.Parsed)] =
    ps.map { case (a, p) => (a, p.withImport(predefImports)) }
}

case class PredefImpl[T[_]]()(implicit valueT: Evaluation.ValueT[T]) {

  import Evaluation.Value
  import valueT.{VInt, VOption, Comparison, VList, ConsValue, Str, ExternalValue, SumValue, ProductValue}

  private def i(a: Value[T]): BigInteger =
    a match {
      case valueT.VInt(bi) => bi
      case other => sys.error(s"expected integer: $a")
    }

  def add(a: Value[T], b: Value[T]): Value[T] =
    VInt(i(a).add(i(b)))

  def div(a: Value[T], b: Value[T]): Value[T] = {
    val bi = i(b)
    if (bi.equals(BigInteger.ZERO)) valueT.VOption.none
    else VOption.some(valueT.VInt(i(a).divide(bi)))
  }

  def sub(a: Value[T], b: Value[T]): Value[T] =
    valueT.VInt(i(a).subtract(i(b)))

  def times(a: Value[T], b: Value[T]): Value[T] =
    valueT.VInt(i(a).multiply(i(b)))

  def eq_Int(a: Value[T], b: Value[T]): Value[T] =
    // since we have already typechecked, standard equals works
    if (a.equals(b)) valueT.True else valueT.False

  def cmp_Int(a: Value[T], b: Value[T]): Value[T] =
    Comparison.fromInt(i(a).compareTo(i(b)))

  def mod_Int(a: Value[T], b: Value[T]): Value[T] =
    valueT.VInt(i(a).mod(i(b).abs()))

  def gcd_Int(a: Value[T], b: Value[T]): Value[T] =
    valueT.VInt(i(a).gcd(i(b)))

  def range(v: Value[T]): Value[T] = {
    val max = i(v)
    @annotation.tailrec
    def loop(i: BigInteger, acc: List[Value[T]]): Value[T] = {
      if (i.compareTo(max) >= 0) {
        // build the list
        @annotation.tailrec
        def build(vs: List[Value[T]], acc: Value[T]): Value[T] =
          vs.asInstanceOf[List[Any]] match {
            case Nil => acc
            case h :: tail => build(tail.asInstanceOf[List[Value[T]]], VList.Cons(h.asInstanceOf[Value[T]], acc))
          }

        build(acc, VList.VNil)
      }
      else {
        loop(i.add(BigInteger.ONE), VInt(i) :: acc)
      }
    }

    loop(BigInteger.ZERO, Nil)
  }

  //def intLoop(intValue: Int, state: a, fn: Int -> a -> TupleCons[Int, TupleCons[a, Unit]]) -> a
  final def intLoop(intValue: Value[T], state: Value[T], fn: Value[T]): Value[T] = {
    val fnT = fn.asFn

    @annotation.tailrec
    def loop(biValue: Value[T], bi: BigInteger, state: Value[T]): Value[T] =
      if (bi.compareTo(BigInteger.ZERO) <= 0) state
      else {
        val fn0 = fnT(biValue).value.asFn
        fn0(state).value match {
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

  def trace(prefix: Value[T], v: Value[T]): Value[T] = {
    val Str(prestr) = prefix
    println(s"$prestr: $v")
    v
  }

  def string_Order_Fn(a: Value[T], b: Value[T]): Value[T] =
    (a, b) match {
      case (Str(sa), Str(sb)) =>
        Comparison.fromInt(sa.compareTo(sb))
      case other => sys.error(s"type error: $other")
    }

  def tokenizeDict(implicit tokenize: Value[T] => Json): Any => Json = { sm =>
    val lst = sm.asInstanceOf[SortedMap[Value[T], Value[T]]].toList.map {
      case (v1, v2) => tokenize(v1).render() -> tokenize(v2)
    }
    Json.JObject(lst)
  }

  def clear_Dict(dictv: Value[T]): Value[T] = {
    val (d, tokenize) = toDict(dictv)
    val ord = d.ordering
    ExternalValue(SortedMap.empty[Value[T], Value[T]](ord), tokenize)
  }


  def empty_Dict(ord: Value[T])(implicit tokenize: Value[T] => Json): Value[T] =
    ord match {
      case ConsValue(fn, _) =>
        implicit val ordValue: Ordering[Value[T]] =
          new Ordering[Value[T]] {
            val fnV = fn.asFn
            def compare(a: Value[T], b: Value[T]): Int =
              fnV(a).flatMap(_.asFn(b)).value match {
                case SumValue(v, _) =>
                  v - 1
                case other => sys.error(s"type error: $other")
              }
          }
        ExternalValue(SortedMap.empty[Value[T], Value[T]], tokenizeDict)
      case other => sys.error(s"type error: $other")
    }

  def toDict(v: Value[T]): (SortedMap[Value[T], Value[T]], Any => Json) =
    v match {
      case ExternalValue(sm, tokenize) =>
        (sm.asInstanceOf[SortedMap[Value[T], Value[T]]], tokenize)
      case other => sys.error(s"type error: $other")
    }

  def add_key(dict: Value[T], k: Value[T], value: Value[T]): Value[T] = {
    val (d, tokenize) = toDict(dict)
    ExternalValue(d.updated(k, value), tokenize)
  }

  def get_key(dict: Value[T], k: Value[T]): Value[T] =
    toDict(dict)._1.get(k) match {
      case None => VOption.none
      case Some(v) => VOption.some(v)
    }

  def remove_key(dict: Value[T], k: Value[T]): Value[T] = {
    val (d, tokenize) = toDict(dict)
    val sm = d - k
    ExternalValue(sm, tokenize)
  }

  def items(dict: Value[T]): Value[T] = {
    val d = toDict(dict)._1
    VList(d.iterator.map { case (k, v) =>
      ProductValue.fromList(k :: ProductValue.fromList(v :: Nil) :: Nil)
    }
    .toList)
  }
}
