package org.bykn.bosatsu

import java.math.BigInteger
import language.experimental.macros

object Predef {

  /** Loads a file *at compile time* as a means of embedding external files into
    * strings. This lets us avoid resources which compilicate matters for
    * scalajs.
    */
  private[bosatsu] def loadFileInCompile(file: String): String =
    macro Macro.loadFileInCompileImpl

  /** String representation of the predef
    */
  val predefString: String =
    loadFileInCompile("core/src/main/resources/bosatsu/predef.bosatsu")

  private def packageName: PackageName =
    PackageName.PredefName

  val jvmExternals: Externals =
    Externals.empty
      .add(packageName, "add", FfiCall.Fn2(PredefImpl.add(_, _)))
      .add(packageName, "div", FfiCall.Fn2(PredefImpl.div(_, _)))
      .add(packageName, "sub", FfiCall.Fn2(PredefImpl.sub(_, _)))
      .add(packageName, "times", FfiCall.Fn2(PredefImpl.times(_, _)))
      .add(packageName, "eq_Int", FfiCall.Fn2(PredefImpl.eq_Int(_, _)))
      .add(packageName, "cmp_Int", FfiCall.Fn2(PredefImpl.cmp_Int(_, _)))
      .add(packageName, "gcd_Int", FfiCall.Fn2(PredefImpl.gcd_Int(_, _)))
      .add(packageName, "mod_Int", FfiCall.Fn2(PredefImpl.mod_Int(_, _)))
      .add(packageName, "int_loop", FfiCall.Fn3(PredefImpl.intLoop(_, _, _)))
      .add(
        packageName,
        "int_to_String",
        FfiCall.Fn1(PredefImpl.int_to_String(_))
      )
      .add(packageName, "trace", FfiCall.Fn2(PredefImpl.trace(_, _)))
      .add(
        packageName,
        "string_Order_fn",
        FfiCall.Fn2(PredefImpl.string_Order_Fn(_, _))
      )
      .add(
        packageName,
        "concat_String",
        FfiCall.Fn1(PredefImpl.concat_String(_))
      )
      .add(
        packageName,
        "partition_String",
        FfiCall.Fn2(PredefImpl.partitionString(_, _))
      )
      .add(
        packageName,
        "rpartition_String",
        FfiCall.Fn2(PredefImpl.rightPartitionString(_, _))
      )
}

object PredefImpl {

  import Value._

  private def i(a: Value): BigInteger =
    a match {
      case VInt(bi) => bi
      case _        => sys.error(s"expected integer: $a")
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

  // def intLoop(intValue: Int, state: a, fn: Int -> a -> TupleCons[Int, TupleCons[a, Unit]]) -> a
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
            } else loop(nextI, n, nextA)
          case other =>
            sys.error(s"unexpected ill-typed value: at $bi, $state, $other")
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
          case other        =>
            // $COVERAGE-OFF$
            sys.error(s"type error: $other")
          // $COVERAGE-ON$
        }.mkString)

      case other =>
        // $COVERAGE-OFF$
        sys.error(s"type error: $other")
      // $COVERAGE-ON$
    }

  // return an Option[(String, String)]
  def partitionString(arg: Value, sep: Value): Value = {
    val sepS = sep.asExternal.toAny.asInstanceOf[String]

    if (sepS.isEmpty) Value.VOption.none
    else {
      val argS = arg.asExternal.toAny.asInstanceOf[String]

      val idx = argS.indexOf(sepS)
      if (idx < 0) Value.VOption.none
      else
        Value.VOption.some {
          val left = argS.substring(0, idx)
          val right = argS.substring(idx + sepS.length)
          Value.Tuple(Value.ExternalValue(left), Value.ExternalValue(right))
        }
    }
  }

  def rightPartitionString(arg: Value, sep: Value): Value = {
    val sepS = sep.asExternal.toAny.asInstanceOf[String]

    if (sepS.isEmpty) Value.VOption.none
    else {
      val argS = arg.asExternal.toAny.asInstanceOf[String]
      val idx = argS.lastIndexOf(sepS)
      if (idx < 0) Value.VOption.none
      else
        Value.VOption.some {
          val left = argS.substring(0, idx)
          val right = argS.substring(idx + sepS.length)
          Value.Tuple(Value.ExternalValue(left), Value.ExternalValue(right))
        }
    }
  }
}
