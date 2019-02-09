package org.bykn.bosatsu

import cats.data.NonEmptyList
import fastparse.all._
import java.math.BigInteger

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

  val predefImports: Import[PackageName, Unit] =
    Import(packageName,
      NonEmptyList.of(
        "Assertion",
        "Bool",
        "Comparison",
        "LT",
        "EQ",
        "GT",
        "False",
        "Int",
        "List",
        "None",
        "Option",
        "Some",
        "String",
        "True",
        "Test",
        "TestSuite",
        "Tuple2",
        "Unit",
        "add",
        "consList",
        "div",
        "emptyList",
        "eq_Int",
        "concat",
        "cmp_Int",
        "foldLeft",
        "gcd_Int",
        "int_loop",
        "mod_Int",
        "range",
        "range_fold",
        "reverse",
        "reverse_concat",
        "sub",
        "times",
        "trace"
        )
        .map(ImportedName.OriginalName(_, ())))

  val jvmExternals: Externals =
    Externals
      .empty
      .add(packageName, "add", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.add"))
      .add(packageName, "div", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.div"))
      .add(packageName, "sub", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.sub"))
      .add(packageName, "times", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.times"))
      .add(packageName, "eq_Int", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.eq_Int"))
      .add(packageName, "emptyList", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.emptyList"))
      .add(packageName, "consList", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.consList"))
      .add(packageName, "cmp_Int", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.cmp_Int"))
      .add(packageName, "foldLeft", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.foldLeft"))
      .add(packageName, "gcd_Int", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.gcd_Int"))
      .add(packageName, "mod_Int", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.mod_Int"))
      .add(packageName, "range", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.range"))
      .add(packageName, "int_loop", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.intLoop"))
      .add(packageName, "trace", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.trace"))

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

  def foldLeft(list: Value, bv: Value, fn: Value): Value = {
    val fnT = fn.asFn
    @annotation.tailrec
    def loop(list: Value, bv: Value): Value =
      list match {
        case VList.VNil => bv
        case VList.Cons(head, tail) =>
          val nextBv = fnT(bv).flatMap(_.asFn(head)).value
          loop(tail, nextBv)
        case _ => sys.error(s"expected a list, found: loop($list, $bv)")
      }

    loop(list, bv)
  }

  def gcd_Int(a: Value, b: Value): Value =
    VInt(i(a).gcd(i(b)))

  def emptyList: Value = VList.VNil
  def consList(head: Value, tail: Value): Value =
    VList.Cons(head, tail)

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

  //def intLoop(intValue: Int, state: a, fn: Int -> a -> Tuple2[Int, Tuple2[a, Unit]]) -> a
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
}

