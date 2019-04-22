package org.bykn.bosatsu

import cats.data.NonEmptyList
import fastparse.all._
import java.math.BigInteger
import scala.collection.immutable.SortedMap

object Predef {
  private def resourceToString(path: String): Option[String] = {
    Option(getClass().getResourceAsStream(path)).map { stream =>
      scala.io.Source.fromInputStream(stream)("UTF-8").mkString
    }
  }

  private val predefString: String =
    resourceToString("/bosatsu/predef.bosatsu")
      .getOrElse {
        scala.io.Source.fromFile("target/scala-2.12/classes/bosatsu/predef.bosatsu").mkString
      }

  lazy val predefPackage: Package.Parsed =
    Package.parser.parse(predefString) match {
      case Parsed.Success(pack, _) => pack
      case Parsed.Failure(exp, idx, extra) =>
        val lm = LocationMap(predefString)
        sys.error(s"couldn't parse predef: ${lm.showContext(idx)} with trace: ${extra.traced.trace}")
    }

  def packageName: PackageName =
    PackageName.predef

  // For pattern matching
  val Name: PackageName =
    PackageName.predef

  /*
   * TODO: we should be able to compute this from predefPackage
   */
  val predefImports: Import[PackageName, Unit] =
    Import(packageName,
      NonEmptyList.of(
        "Assertion",
        "Bool",
        "Comparison",
        "Dict",
        "EQ",
        "EmptyList",
        "False",
        "GT",
        "Int",
        "LT",
        "List",
        "NonEmptyList",
        "None",
        "Option",
        "Order",
        "Some",
        "String",
        "Test",
        "TestSuite",
        "True",
        "TupleCons",
        "Unit",
        "add",
        "add_key",
        "cmp_Int",
        "concat",
        "div",
        "clear_Dict",
        "empty_Dict",
        "eq_Int",
        "flat_map_List",
        "foldLeft",
        "gcd_Int",
        "get_key",
        "int_loop",
        "items",
        "map_List",
        "mod_Int",
        "range",
        "range_fold",
        "remove_key",
        "reverse",
        "reverse_concat",
        "sub",
        "string_Order_fn",
        "string_Order",
        "times",
        "trace",
        "uncurry2",
        "uncurry3"
        )
        .map(Identifier.unsafe(_))
        .map(ImportedName.OriginalName(_, ())))

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
      .add(packageName, "range", FfiCall.Fn1(PredefImpl.range(_)))
      .add(packageName, "int_loop", FfiCall.Fn3(PredefImpl.intLoop(_, _, _)))
      .add(packageName, "trace", FfiCall.Fn2(PredefImpl.trace(_, _)))
      .add(packageName, "string_Order_fn", FfiCall.Fn2(PredefImpl.string_Order_Fn(_, _)))
      .add(packageName, "clear_Dict", FfiCall.Fn1(PredefImpl.clear_Dict(_)))
      .add(packageName, "empty_Dict", FfiCall.Fn1(PredefImpl.empty_Dict(_)))
      .add(packageName, "add_key", FfiCall.Fn3(PredefImpl.add_key(_, _, _)))
      .add(packageName, "get_key", FfiCall.Fn2(PredefImpl.get_key(_, _)))
      .add(packageName, "items", FfiCall.Fn1(PredefImpl.items(_)))
      .add(packageName, "remove_key", FfiCall.Fn2(PredefImpl.remove_key(_, _)))

  def withPredef(ps: List[Package.Parsed]): List[Package.Parsed] =
    predefPackage :: ps.map(_.withImport(predefImports))

  def withPredefA[A](predefA: A, ps: List[(A, Package.Parsed)]): List[(A, Package.Parsed)] =
    (predefA, predefPackage) :: ps.map { case (a, p) => (a, p.withImport(predefImports)) }
}

object PredefImpl {

  import Evaluation.Value
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

  def range(v: Value): Value = {
    val max = i(v)
    @annotation.tailrec
    def loop(i: BigInteger, acc: List[Value]): Value = {
      if (i.compareTo(max) >= 0) {
        // build the list
        @annotation.tailrec
        def build(vs: List[Value], acc: Value): Value =
          vs match {
            case Nil => acc
            case h :: tail => build(tail, VList.Cons(h, acc))
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
  final def intLoop(intValue: Value, state: Value, fn: Value): Value = {
    val fnT = fn.asFn
    @annotation.tailrec
    def loop(biValue: Value, bi: BigInteger, state: Value): Value =
      if (bi.compareTo(BigInteger.ZERO) <= 0) state
      else {
        fnT(biValue).flatMap(_.asFn(state)).value match {
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

  def clear_Dict(dictv: Value): Value = {
    val d = toDict(dictv)
    val ord = d.ordering
    ExternalValue(SortedMap.empty[Value, Value](ord))
  }


  def empty_Dict(ord: Value): Value =
    ord match {
      case ConsValue(fn, _) =>
        implicit val ordValue: Ordering[Value] =
          new Ordering[Value] {
            val fnV = fn.asFn
            def compare(a: Value, b: Value): Int =
              fnV(a).flatMap(_.asFn(b)).value match {
                case SumValue(v, _) =>
                  v - 1
                case other => sys.error(s"type error: $other")
              }
          }
        ExternalValue(SortedMap.empty[Value, Value])
      case other => sys.error(s"type error: $other")
    }

  def toDict(v: Value): SortedMap[Value, Value] =
    v match {
      case ExternalValue(sm) =>
        sm.asInstanceOf[SortedMap[Value, Value]]
      case other => sys.error(s"type error: $other")
    }

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
}

