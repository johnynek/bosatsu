package dev.bosatsu

import cats.data.NonEmptyList
import java.math.BigInteger
import java.util.Locale
object Predef {

  /** Loads a file *at compile time* as a means of embedding external files into
    * strings. This lets us avoid resources which compilicate matters for
    * scalajs.
    */
  private[bosatsu] inline def loadFileInCompile(file: String): String =
    ${ Macro.loadFileInCompileImpl('file) }

  /** String representation of the predef
    */
  val predefString: String =
    loadFileInCompile("core/src/main/resources/bosatsu/predef.bosatsu")

  private def predefPackageName: PackageName =
    PackageName.PredefName
  private def arrayPackageName: PackageName =
    PackageName.parts("Bosatsu", "Collection", "Array")
  private def float64PackageName: PackageName =
    PackageName.parts("Bosatsu", "Float", "Float64")

  val jvmExternals: Externals =
    Externals.empty
      .add(predefPackageName, "add", FfiCall.Fn2(PredefImpl.add(_, _)))
      .add(predefPackageName, "addf", FfiCall.Fn2(PredefImpl.addf(_, _)))
      .add(predefPackageName, "div", FfiCall.Fn2(PredefImpl.div(_, _)))
      .add(predefPackageName, "divf", FfiCall.Fn2(PredefImpl.divf(_, _)))
      .add(predefPackageName, "sub", FfiCall.Fn2(PredefImpl.sub(_, _)))
      .add(predefPackageName, "subf", FfiCall.Fn2(PredefImpl.subf(_, _)))
      .add(predefPackageName, "times", FfiCall.Fn2(PredefImpl.times(_, _)))
      .add(predefPackageName, "timesf", FfiCall.Fn2(PredefImpl.timesf(_, _)))
      .add(predefPackageName, "eq_Int", FfiCall.Fn2(PredefImpl.eq_Int(_, _)))
      .add(
        predefPackageName,
        "cmp_Int",
        FfiCall.Fn2(PredefImpl.cmp_Int(_, _))
      )
      .add(
        predefPackageName,
        "cmp_Float64",
        FfiCall.Fn2(PredefImpl.cmp_Float64(_, _))
      )
      .add(
        predefPackageName,
        "gcd_Int",
        FfiCall.Fn2(PredefImpl.gcd_Int(_, _))
      )
      .add(
        predefPackageName,
        "mod_Int",
        FfiCall.Fn2(PredefImpl.mod_Int(_, _))
      )
      .add(
        predefPackageName,
        "shift_right_Int",
        FfiCall.Fn2(PredefImpl.shift_right_Int(_, _))
      )
      .add(
        predefPackageName,
        "shift_left_Int",
        FfiCall.Fn2(PredefImpl.shift_left_Int(_, _))
      )
      .add(predefPackageName, "and_Int", FfiCall.Fn2(PredefImpl.and_Int(_, _)))
      .add(predefPackageName, "or_Int", FfiCall.Fn2(PredefImpl.or_Int(_, _)))
      .add(predefPackageName, "xor_Int", FfiCall.Fn2(PredefImpl.xor_Int(_, _)))
      .add(predefPackageName, "not_Int", FfiCall.Fn1(PredefImpl.not_Int(_)))
      .add(
        predefPackageName,
        "int_loop",
        FfiCall.Fn3(PredefImpl.intLoop(_, _, _))
      )
      .add(
        predefPackageName,
        "int_to_String",
        FfiCall.Fn1(PredefImpl.int_to_String(_))
      )
      .add(
        predefPackageName,
        "string_to_Int",
        FfiCall.Fn1(PredefImpl.string_to_Int(_))
      )
      .add(predefPackageName, "trace", FfiCall.Fn2(PredefImpl.trace(_, _)))
      .add(
        predefPackageName,
        "cmp_String",
        FfiCall.Fn2(PredefImpl.cmp_String(_, _))
      )
      .add(
        predefPackageName,
        "concat_String",
        FfiCall.Fn1(PredefImpl.concat_String(_))
      )
      .add(
        predefPackageName,
        "char_to_String",
        FfiCall.Fn1(PredefImpl.char_to_String(_))
      )
      .add(
        predefPackageName,
        "partition_String",
        FfiCall.Fn2(PredefImpl.partitionString(_, _))
      )
      .add(
        predefPackageName,
        "rpartition_String",
        FfiCall.Fn2(PredefImpl.rightPartitionString(_, _))
      )
      .add(arrayPackageName, "empty_Array", FfiCall.Const(PredefImpl.emptyArray))
      .add(
        arrayPackageName,
        "tabulate_Array",
        FfiCall.Fn2(PredefImpl.tabulate_Array(_, _))
      )
      .add(
        arrayPackageName,
        "from_List_Array",
        FfiCall.Fn1(PredefImpl.from_List_Array(_))
      )
      .add(
        arrayPackageName,
        "to_List_Array",
        FfiCall.Fn1(PredefImpl.to_List_Array(_))
      )
      .add(
        arrayPackageName,
        "size_Array",
        FfiCall.Fn1(PredefImpl.size_Array(_))
      )
      .add(
        arrayPackageName,
        "get_map_Array",
        FfiCall.Fn4(PredefImpl.get_map_Array(_, _, _, _))
      )
      .add(
        arrayPackageName,
        "get_or_Array",
        FfiCall.Fn3(PredefImpl.get_or_Array(_, _, _))
      )
      .add(
        arrayPackageName,
        "foldl_Array",
        FfiCall.Fn3(PredefImpl.foldl_Array(_, _, _))
      )
      .add(
        arrayPackageName,
        "map_Array",
        FfiCall.Fn2(PredefImpl.map_Array(_, _))
      )
      .add(
        arrayPackageName,
        "set_or_self_Array",
        FfiCall.Fn3(PredefImpl.set_or_self_Array(_, _, _))
      )
      .add(
        arrayPackageName,
        "sort_Array",
        FfiCall.Fn2(PredefImpl.sort_Array(_, _))
      )
      .add(
        arrayPackageName,
        "concat_all_Array",
        FfiCall.Fn1(PredefImpl.concat_all_Array(_))
      )
      .add(
        arrayPackageName,
        "slice_Array",
        FfiCall.Fn3(PredefImpl.slice_Array(_, _, _))
      )
      .add(float64PackageName, "abs", FfiCall.Fn1(PredefImpl.abs_Float64(_)))
      .add(float64PackageName, "acos", FfiCall.Fn1(PredefImpl.acos_Float64(_)))
      .add(float64PackageName, "asin", FfiCall.Fn1(PredefImpl.asin_Float64(_)))
      .add(float64PackageName, "atan", FfiCall.Fn1(PredefImpl.atan_Float64(_)))
      .add(
        float64PackageName,
        "atan2",
        FfiCall.Fn2(PredefImpl.atan2_Float64(_, _))
      )
      .add(float64PackageName, "ceil", FfiCall.Fn1(PredefImpl.ceil_Float64(_)))
      .add(float64PackageName, "cos", FfiCall.Fn1(PredefImpl.cos_Float64(_)))
      .add(float64PackageName, "cosh", FfiCall.Fn1(PredefImpl.cosh_Float64(_)))
      .add(float64PackageName, "exp", FfiCall.Fn1(PredefImpl.exp_Float64(_)))
      .add(
        float64PackageName,
        "floor",
        FfiCall.Fn1(PredefImpl.floor_Float64(_))
      )
      .add(
        float64PackageName,
        "hypot",
        FfiCall.Fn2(PredefImpl.hypot_Float64(_, _))
      )
      .add(float64PackageName, "log", FfiCall.Fn1(PredefImpl.log_Float64(_)))
      .add(
        float64PackageName,
        "log10",
        FfiCall.Fn1(PredefImpl.log10_Float64(_))
      )
      .add(float64PackageName, "pow", FfiCall.Fn2(PredefImpl.pow_Float64(_, _)))
      .add(float64PackageName, "sin", FfiCall.Fn1(PredefImpl.sin_Float64(_)))
      .add(float64PackageName, "sinh", FfiCall.Fn1(PredefImpl.sinh_Float64(_)))
      .add(float64PackageName, "sqrt", FfiCall.Fn1(PredefImpl.sqrt_Float64(_)))
      .add(float64PackageName, "tan", FfiCall.Fn1(PredefImpl.tan_Float64(_)))
      .add(float64PackageName, "tanh", FfiCall.Fn1(PredefImpl.tanh_Float64(_)))
      .add(
        float64PackageName,
        "copy_sign",
        FfiCall.Fn2(PredefImpl.copySign_Float64(_, _))
      )
      .add(
        float64PackageName,
        "is_nan",
        FfiCall.Fn1(PredefImpl.isNaN_Float64(_))
      )
      .add(
        float64PackageName,
        "is_infinite",
        FfiCall.Fn1(PredefImpl.isInfinite_Float64(_))
      )
      .add(
        float64PackageName,
        "float64_to_String",
        FfiCall.Fn1(PredefImpl.float64_to_String(_))
      )
      .add(
        float64PackageName,
        "string_to_Float64",
        FfiCall.Fn1(PredefImpl.string_to_Float64(_))
      )
      .add(
        float64PackageName,
        "int_bits_to_Float64",
        FfiCall.Fn1(PredefImpl.int_bits_to_Float64(_))
      )
      .add(
        float64PackageName,
        "float64_bits_to_Int",
        FfiCall.Fn1(PredefImpl.float64_bits_to_Int(_))
      )
}

object PredefImpl {

  import Value._
  private val NaNBitsPrefix = "NaN:0x"

  final case class ArrayValue(data: Array[Value], offset: Int, len: Int) {
    Require(offset >= 0, s"offset must be >= 0: $offset")
    Require(len >= 0, s"len must be >= 0: $len")
    Require(offset + len <= data.length, s"invalid view ($offset, $len)")
  }

  private val EmptyArrayData: Array[Value] = Array.empty[Value]
  private val EmptyArrayRepr: ArrayValue = ArrayValue(EmptyArrayData, 0, 0)
  val emptyArray: Value = ExternalValue(EmptyArrayRepr)

  private def i(a: Value): BigInteger =
    a match {
      case VInt(bi) => bi
      case _        => sys.error(s"expected integer: $a")
    }

  private def d(a: Value): Double =
    a match {
      case VFloat(v) => v
      case _         => sys.error(s"expected float64: $a")
    }

  private def vf(v: Double): Value =
    VFloat(v)

  private def bool(b: Boolean): Value =
    if (b) True else False

  def compareFloat64Total(a: Double, b: Double): Int = {
    val aNaN = java.lang.Double.isNaN(a)
    val bNaN = java.lang.Double.isNaN(b)
    if (aNaN) {
      if (bNaN) 0 else -1
    } else if (bNaN) {
      1
    } else if (a < b) {
      -1
    } else if (a > b) {
      1
    } else {
      0
    }
  }

  private def unsignedLongToBigInteger(bits: Long): BigInteger =
    if (bits >= 0L) BigInteger.valueOf(bits)
    else BigInteger.valueOf(bits & Long.MaxValue).setBit(63)

  private def toUnsignedHex64(bits: Long): String = {
    val raw = java.lang.Long.toUnsignedString(bits, 16)
    if (raw.length >= 16) raw
    else {
      val zeros = new java.lang.String(Array.fill(16 - raw.length)('0'))
      zeros + raw
    }
  }

  private def parseUnsignedHex64(hex: String): Option[Long] =
    if (hex.length != 16) None
    else {
      try Some(java.lang.Long.parseUnsignedLong(hex, 16))
      catch {
        case _: NumberFormatException => None
      }
    }

  private def parseFloat64String(str: String): Option[Double] = {
    val cleaned =
      if (str.indexOf('_') >= 0) str.filter(_ != '_')
      else str
    val lowered = cleaned.toLowerCase(Locale.ROOT)

    if (lowered.startsWith("nan:0x")) {
      parseUnsignedHex64(cleaned.drop(6))
        .map(java.lang.Double.longBitsToDouble(_))
    } else {
      val normalized =
        lowered match {
          case ".nan" | "nan"                   => "NaN"
          case "∞" | "+∞" | "infinity" | "+infinity" | "inf" | "+inf" =>
            "Infinity"
          case "-∞" | "-infinity" | "-inf"      => "-Infinity"
          case _                                 => cleaned
        }
      try Some(java.lang.Double.parseDouble(normalized))
      catch {
        case _: NumberFormatException => None
      }
    }
  }

  private def asArray(a: Value): ArrayValue =
    a.asExternal.toAny match {
      case arr: ArrayValue => arr
      case other           =>
        // $COVERAGE-OFF$
        sys.error(s"expected array external value, found: $other")
      // $COVERAGE-ON$
    }

  def add(a: Value, b: Value): Value =
    VInt(i(a).add(i(b)))

  def addf(a: Value, b: Value): Value =
    vf(d(a) + d(b))

  def divBigInteger(a: BigInteger, b: BigInteger): BigInteger =
    if (b == BigInteger.ZERO) BigInteger.ZERO
    else if (b == BigInteger.ONE) a
    else {
      val mod = modBigInteger(a, b)
      a.subtract(mod).divide(b)
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

  def divf(a: Value, b: Value): Value =
    vf(d(a) / d(b))

  def sub(a: Value, b: Value): Value =
    VInt(i(a).subtract(i(b)))

  def subf(a: Value, b: Value): Value =
    vf(d(a) - d(b))

  def times(a: Value, b: Value): Value =
    VInt(i(a).multiply(i(b)))

  def timesf(a: Value, b: Value): Value =
    vf(d(a) * d(b))

  def eq_Int(a: Value, b: Value): Value =
    // since we have already typechecked, standard equals works
    if (a == b) True else False

  def cmp_Int(a: Value, b: Value): Value =
    Comparison.fromInt(i(a).compareTo(i(b)))

  def cmp_Float64(a: Value, b: Value): Value =
    Comparison.fromInt(compareFloat64Total(d(a), d(b)))

  def abs_Float64(a: Value): Value = vf(java.lang.Math.abs(d(a)))
  def acos_Float64(a: Value): Value = vf(java.lang.Math.acos(d(a)))
  def asin_Float64(a: Value): Value = vf(java.lang.Math.asin(d(a)))
  def atan_Float64(a: Value): Value = vf(java.lang.Math.atan(d(a)))
  def atan2_Float64(a: Value, b: Value): Value =
    vf(java.lang.Math.atan2(d(a), d(b)))
  def ceil_Float64(a: Value): Value = vf(java.lang.Math.ceil(d(a)))
  def cos_Float64(a: Value): Value = vf(java.lang.Math.cos(d(a)))
  def cosh_Float64(a: Value): Value = vf(java.lang.Math.cosh(d(a)))
  def exp_Float64(a: Value): Value = vf(java.lang.Math.exp(d(a)))
  def floor_Float64(a: Value): Value = vf(java.lang.Math.floor(d(a)))
  def hypot_Float64(a: Value, b: Value): Value =
    vf(java.lang.Math.hypot(d(a), d(b)))
  def log_Float64(a: Value): Value = vf(java.lang.Math.log(d(a)))
  def log10_Float64(a: Value): Value = vf(java.lang.Math.log10(d(a)))
  def pow_Float64(a: Value, b: Value): Value =
    vf(java.lang.Math.pow(d(a), d(b)))
  def sin_Float64(a: Value): Value = vf(java.lang.Math.sin(d(a)))
  def sinh_Float64(a: Value): Value = vf(java.lang.Math.sinh(d(a)))
  def sqrt_Float64(a: Value): Value = vf(java.lang.Math.sqrt(d(a)))
  def tan_Float64(a: Value): Value = vf(java.lang.Math.tan(d(a)))
  def tanh_Float64(a: Value): Value = vf(java.lang.Math.tanh(d(a)))
  def copySign_Float64(a: Value, b: Value): Value =
    vf(java.lang.Math.copySign(d(a), d(b)))
  def isNaN_Float64(a: Value): Value =
    bool(java.lang.Double.isNaN(d(a)))
  def isInfinite_Float64(a: Value): Value =
    bool(java.lang.Double.isInfinite(d(a)))
  def float64_to_String(a: Value): Value = {
    val bits = java.lang.Double.doubleToRawLongBits(d(a))
    val value = java.lang.Double.longBitsToDouble(bits)
    if (java.lang.Double.isNaN(value))
      Value.Str(s"${NaNBitsPrefix}${toUnsignedHex64(bits)}")
    else Value.Str(Lit.Float64.toLiteralString(Lit.Float64.fromRawLongBits(bits)))
  }
  def string_to_Float64(a: Value): Value =
    a match {
      case Value.Str(s) =>
        parseFloat64String(s) match {
          case Some(v) => Value.VOption.some(vf(v))
          case None    => Value.VOption.none
        }
      case other        => sys.error(s"type error: $other")
    }
  def int_bits_to_Float64(a: Value): Value =
    vf(java.lang.Double.longBitsToDouble(i(a).longValue()))
  def float64_bits_to_Int(a: Value): Value =
    Value.VInt(unsignedLongToBigInteger(java.lang.Double.doubleToRawLongBits(d(a))))

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

  private val MaxIntBI = BigInteger.valueOf(Int.MaxValue.toLong)

  final def shiftRight(a: BigInteger, b: BigInteger): BigInteger = {
    val bi = b.intValue()
    val a1 = a.shiftRight(bi)
    if (b.compareTo(MaxIntBI) > 0) {
      // $COVERAGE-OFF$
      // java bigInteger can't actually store arbitrarily large
      // integers, just blow up here
      sys.error(s"invalid huge shiftRight($a, $b)")
      // $COVERAGE-ON$
    } else {
      a1
    }
  }

  def shift_right_Int(a: Value, b: Value): Value =
    VInt(shiftRight(i(a), i(b)))

  final def shiftLeft(a: BigInteger, b: BigInteger): BigInteger = {
    val bi = b.intValue()
    val a1 = a.shiftLeft(bi)
    if (b.compareTo(MaxIntBI) > 0) {
      // $COVERAGE-OFF$
      // java bigInteger can't actually store arbitrarily large
      // integers, just blow up here
      sys.error(s"invalid huge shiftLeft($a, $b)")
      // $COVERAGE-ON$
    } else {
      a1
    }
  }

  def shift_left_Int(a: Value, b: Value): Value =
    VInt(shiftLeft(i(a), i(b)))

  def and_Int(a: Value, b: Value): Value =
    VInt(i(a).and(i(b)))

  def or_Int(a: Value, b: Value): Value =
    VInt(i(a).or(i(b)))

  def xor_Int(a: Value, b: Value): Value =
    VInt(i(a).xor(i(b)))

  def not_Int(a: Value): Value =
    VInt(i(a).not())

  private def toIntExactIfRepresentable(v: BigInteger): Option[Int] = {
    val minInt = BigInteger.valueOf(Int.MinValue.toLong)
    if (v.compareTo(minInt) < 0 || v.compareTo(MaxIntBI) > 0) None
    else Some(v.intValue())
  }

  private def inRangeIndex(idx: BigInteger, len: Int): Option[Int] =
    if (idx.signum < 0) None
    else if (idx.compareTo(BigInteger.valueOf(len.toLong)) >= 0) None
    else toIntExactIfRepresentable(idx)

  private def copyView(arr: ArrayValue): Array[Value] =
    java.util.Arrays.copyOfRange(arr.data, arr.offset, arr.offset + arr.len)

  def tabulate_Array(size: Value, fn: Value): Value = {
    val sizeBI = i(size)
    if (sizeBI.signum <= 0) emptyArray
    else if (sizeBI.compareTo(MaxIntBI) > 0) emptyArray
    else {
      val sz = sizeBI.intValue()
      val data = new Array[Value](sz)
      val fnT = fn.asFn
      var idx = 0
      while (idx < sz) {
        data(idx) = fnT(NonEmptyList(VInt(idx), Nil))
        idx = idx + 1
      }
      ExternalValue(ArrayValue(data, 0, sz))
    }
  }

  def from_List_Array(items: Value): Value = {
    val bldr = List.newBuilder[Value]

    @annotation.tailrec
    def loop(rem: Value): Unit =
      rem match {
        case VList.VNil            => ()
        case VList.Cons(head, t1) =>
          bldr += head
          loop(t1)
        case other                =>
          // $COVERAGE-OFF$
          sys.error(s"type error: expected list, found $other")
        // $COVERAGE-ON$
      }

    loop(items)
    val arr = bldr.result().toArray
    if (arr.isEmpty) emptyArray
    else ExternalValue(ArrayValue(arr, 0, arr.length))
  }

  def to_List_Array(array: Value): Value = {
    val arr = asArray(array)
    var idx = arr.len - 1
    var res: Value = VList.VNil
    while (idx >= 0) {
      res = VList.Cons(arr.data(arr.offset + idx), res)
      idx = idx - 1
    }
    res
  }

  def size_Array(array: Value): Value =
    VInt(BigInt(asArray(array).len))

  def get_map_Array(
      array: Value,
      index: Value,
      default: Value,
      fn: Value
  ): Value = {
    val arr = asArray(array)
    inRangeIndex(i(index), arr.len) match {
      case Some(idx) => fn.asFn(NonEmptyList(arr.data(arr.offset + idx), Nil))
      case None      => default.asFn(NonEmptyList(UnitValue, Nil))
    }
  }

  def get_or_Array(array: Value, index: Value, default: Value): Value = {
    get_map_Array(array, index, default, FnValue.identity)
  }

  def foldl_Array(array: Value, init: Value, fn: Value): Value = {
    val arr = asArray(array)
    val fnT = fn.asFn
    var idx = 0
    var acc = init
    while (idx < arr.len) {
      acc = fnT(NonEmptyList(acc, arr.data(arr.offset + idx) :: Nil))
      idx = idx + 1
    }
    acc
  }

  def map_Array(array: Value, fn: Value): Value = {
    val arr = asArray(array)
    if (arr.len == 0) emptyArray
    else {
      val fnT = fn.asFn
      val mapped = new Array[Value](arr.len)
      var idx = 0
      while (idx < arr.len) {
        mapped(idx) = fnT(NonEmptyList(arr.data(arr.offset + idx), Nil))
        idx = idx + 1
      }
      ExternalValue(ArrayValue(mapped, 0, arr.len))
    }
  }

  def set_or_self_Array(array: Value, index: Value, value: Value): Value = {
    val arr = asArray(array)
    inRangeIndex(i(index), arr.len) match {
      case Some(idx) =>
        val copied = copyView(arr)
        copied(idx) = value
        ExternalValue(ArrayValue(copied, 0, copied.length))
      case None      =>
        array
    }
  }

  def sort_Array(array: Value, cmpFn: Value): Value = {
    val arr = asArray(array)
    if (arr.len < 2) array
    else {
      val data = copyView(arr)
      val cmp = cmpFn.asFn
      java.util.Arrays.sort(
        data.asInstanceOf[Array[AnyRef]],
        new java.util.Comparator[AnyRef] {
          def compare(left: AnyRef, right: AnyRef): Int = {
            val res = cmp(
              NonEmptyList(left.asInstanceOf[Value], right.asInstanceOf[Value] :: Nil)
            ).asSum.variant
            if (res < 1) -1 else if (res > 1) 1 else 0
          }
        }
      )
      ExternalValue(ArrayValue(data, 0, data.length))
    }
  }

  def concat_all_Array(arrays: Value): Value = {
    val as = List.newBuilder[ArrayValue]
    var current = arrays
    var total = 0L

    while (current != VList.VNil) {
      current match {
        case VList.Cons(head, tail) =>
          val arr = asArray(head)
          as += arr
          total = total + arr.len.toLong
          current = tail
        case other                  =>
          // $COVERAGE-OFF$
          sys.error(s"type error: expected list, found $other")
        // $COVERAGE-ON$
      }
    }

    if (total <= 0L || total > Int.MaxValue.toLong) emptyArray
    else {
      val totalInt = total.toInt
      val data = new Array[Value](totalInt)
      var offset = 0
      as.result().iterator.foreach { arr =>
        if (arr.len > 0) {
          java.lang.System.arraycopy(
            arr.data,
            arr.offset,
            data,
            offset,
            arr.len
          )
          offset = offset + arr.len
        }
      }
      ExternalValue(ArrayValue(data, 0, totalInt))
    }
  }

  def slice_Array(array: Value, start: Value, end: Value): Value = {
    val arr = asArray(array)
    val lenBI = BigInteger.valueOf(arr.len.toLong)
    val startBI = {
      val raw = i(start)
      if (raw.signum < 0) BigInteger.ZERO else raw
    }
    val endBI = {
      val raw = i(end)
      if (raw.compareTo(lenBI) > 0) lenBI else raw
    }

    val valid =
      startBI.signum >= 0 &&
        endBI.signum >= 0 &&
        startBI.compareTo(endBI) <= 0 &&
        endBI.compareTo(lenBI) <= 0

    if (!valid) emptyArray
    else {
      val sliceLenBI = endBI.subtract(startBI)
      if (sliceLenBI.signum <= 0) emptyArray
      else {
        val startIdx = startBI.intValue()
        val sliceLen = sliceLenBI.intValue()
        ExternalValue(ArrayValue(arr.data, arr.offset + startIdx, sliceLen))
      }
    }
  }

  // def intLoop(intValue: Int, state: a, fn: Int -> a -> Tuple2[Int, a]) -> a
  final def intLoop(intValue: Value, state: Value, fn: Value): Value = {
    val fnT = fn.asFn

    @annotation.tailrec
    def loop(biValue: Value, bi: BigInteger, state: Value): Value =
      if (bi.compareTo(BigInteger.ZERO) <= 0) state
      else {
        fnT(NonEmptyList(biValue, state :: Nil)) match {
          case ProductValue(nextI, nextA) =>
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

  final def string_to_Int(strValue: Value): Value = {
    val str = strValue match {
      case Value.Str(s) => s
      case other        => sys.error(s"type error: $other")
    }
    try Value.VOption.some(VInt(new BigInteger(str)))
    catch {
      case _: NumberFormatException => Value.VOption.none
    }
  }

  def trace(prefix: Value, v: Value): Value = {
    val prestr = prefix match {
      case Value.Str(s) => s
      case other        => sys.error(s"type error: $other")
    }
    println(s"$prestr: $v")
    v
  }

  def cmp_String(a: Value, b: Value): Value =
    (a, b) match {
      case (Value.Str(sa), Value.Str(sb)) =>
        Value.Comparison.fromInt(sa.compareTo(sb))
      case other => sys.error(s"type error: $other")
    }

  // we represent chars as single code-point strings
  def char_to_String(item: Value): Value = item

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
