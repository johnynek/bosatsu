package dev.bosatsu

import cats.data.NonEmptyList
import java.io.{
  BufferedReader,
  BufferedWriter,
  InputStreamReader,
  OutputStreamWriter,
  Reader,
  Writer
}
import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.charset.{
  CharacterCodingException,
  CodingErrorAction,
  StandardCharsets
}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{
  AccessDeniedException,
  DirectoryNotEmptyException,
  FileAlreadyExistsException,
  Files,
  InvalidPathException,
  LinkOption,
  NoSuchFileException,
  NotDirectoryException,
  OpenOption,
  Path => JPath,
  Paths,
  StandardOpenOption
}
import java.util.Locale
import scala.util.control.NonFatal
import scala.util.DynamicVariable
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
    PackageName.parts("Bosatsu", "Num", "Float64")
  private def progPackageName: PackageName =
    PackageName.parts("Bosatsu", "Prog")
  private def ioCorePackageName: PackageName =
    PackageName.parts("Bosatsu", "IO", "Core")

  private def addIoCoreExternals(externals: Externals): Externals =
    if (Platform.isScalaJvm) {
      externals
        .add(
          ioCorePackageName,
          "path_sep",
          FfiCall.Const(PredefImpl.core_path_sep)
        )
        .add(ioCorePackageName, "stdin", FfiCall.Const(PredefImpl.core_stdin))
        .add(
          ioCorePackageName,
          "stdout",
          FfiCall.Const(PredefImpl.core_stdout)
        )
        .add(
          ioCorePackageName,
          "stderr",
          FfiCall.Const(PredefImpl.core_stderr)
        )
        .add(
          ioCorePackageName,
          "read_utf8",
          FfiCall.Fn2(PredefImpl.prog_core_read_utf8(_, _))
        )
        .add(
          ioCorePackageName,
          "write_utf8",
          FfiCall.Fn2(PredefImpl.prog_core_write_utf8(_, _))
        )
        .add(
          ioCorePackageName,
          "flush",
          FfiCall.Fn1(PredefImpl.prog_core_flush(_))
        )
        .add(
          ioCorePackageName,
          "close",
          FfiCall.Fn1(PredefImpl.prog_core_close(_))
        )
        .add(
          ioCorePackageName,
          "open_file",
          FfiCall.Fn2(PredefImpl.prog_core_open_file(_, _))
        )
        .add(
          ioCorePackageName,
          "list_dir",
          FfiCall.Fn1(PredefImpl.prog_core_list_dir(_))
        )
        .add(
          ioCorePackageName,
          "stat",
          FfiCall.Fn1(PredefImpl.prog_core_stat(_))
        )
        .add(
          ioCorePackageName,
          "mkdir",
          FfiCall.Fn2(PredefImpl.prog_core_mkdir(_, _))
        )
        .add(
          ioCorePackageName,
          "remove",
          FfiCall.Fn2(PredefImpl.prog_core_remove(_, _))
        )
        .add(
          ioCorePackageName,
          "rename",
          FfiCall.Fn2(PredefImpl.prog_core_rename(_, _))
        )
        .add(
          ioCorePackageName,
          "get_env",
          FfiCall.Fn1(PredefImpl.prog_core_get_env(_))
        )
        .add(
          ioCorePackageName,
          "spawn",
          FfiCall.Fn3(PredefImpl.prog_core_spawn(_, _, _))
        )
        .add(
          ioCorePackageName,
          "wait",
          FfiCall.Fn1(PredefImpl.prog_core_wait(_))
        )
        .add(
          ioCorePackageName,
          "now_wall",
          FfiCall.Const(PredefImpl.prog_core_now_wall)
        )
        .add(
          ioCorePackageName,
          "now_mono",
          FfiCall.Const(PredefImpl.prog_core_now_mono)
        )
        .add(
          ioCorePackageName,
          "sleep",
          FfiCall.Fn1(PredefImpl.prog_core_sleep(_))
        )
    } else externals

  val jvmExternals: Externals =
    addIoCoreExternals(
      Externals.empty
      .add(predefPackageName, "add", FfiCall.Fn2(PredefImpl.add(_, _)))
      .add(predefPackageName, "addf", FfiCall.Fn2(PredefImpl.addf(_, _)))
      .add(predefPackageName, "div", FfiCall.Fn2(PredefImpl.div(_, _)))
      .add(predefPackageName, "divf", FfiCall.Fn2(PredefImpl.divf(_, _)))
      .add(predefPackageName, "sub", FfiCall.Fn2(PredefImpl.sub(_, _)))
      .add(predefPackageName, "subf", FfiCall.Fn2(PredefImpl.subf(_, _)))
      .add(predefPackageName, "mul", FfiCall.Fn2(PredefImpl.mul(_, _)))
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
        "char_to_Int",
        FfiCall.Fn1(PredefImpl.char_to_Int(_))
      )
      .add(
        predefPackageName,
        "char_List_to_String",
        FfiCall.Fn1(PredefImpl.char_List_to_String(_))
      )
      .add(
        predefPackageName,
        "int_to_Char",
        FfiCall.Fn1(PredefImpl.int_to_Char(_))
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
      .add(
        predefPackageName,
        "uncons_String",
        FfiCall.Fn1(PredefImpl.uncons_String(_))
      )
      .add(
        predefPackageName,
        "tail_or_empty_String",
        FfiCall.Fn1(PredefImpl.tail_or_empty_String(_))
      )
      .add(
        arrayPackageName,
        "empty_Array",
        FfiCall.Const(PredefImpl.emptyArray)
      )
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
      .add(
        arrayPackageName,
        "char_Array_to_String",
        FfiCall.Fn1(PredefImpl.char_Array_to_String(_))
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
      .add(
        float64PackageName,
        "float64_to_Int",
        FfiCall.Fn1(PredefImpl.float64_to_Int(_))
      )
      .add(
        float64PackageName,
        "int_to_Float64",
        FfiCall.Fn1(PredefImpl.int_to_Float64(_))
      )
      .add(progPackageName, "pure", FfiCall.Fn1(PredefImpl.prog_pure(_)))
      .add(
        progPackageName,
        "raise_error",
        FfiCall.Fn1(PredefImpl.prog_raise_error(_))
      )
      .add(
        progPackageName,
        "flat_map",
        FfiCall.Fn2(PredefImpl.prog_flat_map(_, _))
      )
      .add(
        progPackageName,
        "recover",
        FfiCall.Fn2(PredefImpl.prog_recover(_, _))
      )
      .add(
        progPackageName,
        "apply_fix",
        FfiCall.Fn2(PredefImpl.prog_apply_fix(_, _))
      )
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

  private val MinIntLong = Int.MinValue.toLong
  private val MaxIntLong = Int.MaxValue.toLong

  private def intValue(i: Int): java.lang.Integer =
    java.lang.Integer.valueOf(i)

  private def intRaw(a: Value): Value.BosatsuInt =
    a match {
      case ExternalValue(v: java.lang.Integer) => v
      case ExternalValue(v: BigInteger)        => v
      case _                                   => sys.error(s"expected integer: $a")
    }

  private def intFromBigInteger(bi: BigInteger): Value.BosatsuInt =
    Value.intFromBigInteger(bi)

  private def i(a: Value): BigInteger =
    Value.intToBigInteger(intRaw(a))

  private def addInt(a: Value.BosatsuInt, b: Value.BosatsuInt): Value.BosatsuInt =
    (a, b) match {
      case (ai: java.lang.Integer, bi: java.lang.Integer) =>
        val aiL = ai.longValue()
        val biL = bi.longValue()
        val sum = aiL + biL
        if (sum >= MinIntLong && sum <= MaxIntLong) intValue(sum.toInt)
        else BigInteger.valueOf(aiL).add(BigInteger.valueOf(biL))
      case _ =>
        intFromBigInteger(Value.intToBigInteger(a).add(Value.intToBigInteger(b)))
    }

  private def subInt(a: Value.BosatsuInt, b: Value.BosatsuInt): Value.BosatsuInt =
    (a, b) match {
      case (ai: java.lang.Integer, bi: java.lang.Integer) =>
        val aiL = ai.longValue()
        val biL = bi.longValue()
        val diff = aiL - biL
        if (diff >= MinIntLong && diff <= MaxIntLong) intValue(diff.toInt)
        else BigInteger.valueOf(aiL).subtract(
            BigInteger.valueOf(biL)
          )
      case _ =>
        intFromBigInteger(
          Value.intToBigInteger(a).subtract(Value.intToBigInteger(b))
        )
    }

  private def mulInt(a: Value.BosatsuInt, b: Value.BosatsuInt): Value.BosatsuInt =
    (a, b) match {
      case (ai: java.lang.Integer, bi: java.lang.Integer) =>
        val aiL = ai.longValue()
        val biL = bi.longValue()
        val prod = aiL * biL
        if (prod >= MinIntLong && prod <= MaxIntLong) intValue(prod.toInt)
        else BigInteger.valueOf(aiL).multiply(
            BigInteger.valueOf(biL)
          )
      case _ =>
        intFromBigInteger(
          Value.intToBigInteger(a).multiply(Value.intToBigInteger(b))
        )
    }

  private def andInt(a: Value.BosatsuInt, b: Value.BosatsuInt): Value.BosatsuInt =
    (a, b) match {
      case (ai: java.lang.Integer, bi: java.lang.Integer) =>
        intValue(ai.intValue() & bi.intValue())
      case _ =>
        intFromBigInteger(
          Value.intToBigInteger(a).and(Value.intToBigInteger(b))
        )
    }

  private def orInt(a: Value.BosatsuInt, b: Value.BosatsuInt): Value.BosatsuInt =
    (a, b) match {
      case (ai: java.lang.Integer, bi: java.lang.Integer) =>
        intValue(ai.intValue() | bi.intValue())
      case _ =>
        intFromBigInteger(Value.intToBigInteger(a).or(Value.intToBigInteger(b)))
    }

  private def xorInt(a: Value.BosatsuInt, b: Value.BosatsuInt): Value.BosatsuInt =
    (a, b) match {
      case (ai: java.lang.Integer, bi: java.lang.Integer) =>
        intValue(ai.intValue() ^ bi.intValue())
      case _ =>
        intFromBigInteger(
          Value.intToBigInteger(a).xor(Value.intToBigInteger(b))
        )
    }

  private def notInt(a: Value.BosatsuInt): Value.BosatsuInt =
    a match {
      case ai: java.lang.Integer => intValue(~ai.intValue())
      case _                     => intFromBigInteger(Value.intToBigInteger(a).not())
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

  private def copySignDouble(magnitude: Double, sign: Double): Double = {
    val magnitudeBits =
      java.lang.Double.doubleToRawLongBits(magnitude) & 0x7fffffffffffffffL
    val signBits =
      java.lang.Double.doubleToRawLongBits(sign) & 0x8000000000000000L
    java.lang.Double.longBitsToDouble(magnitudeBits | signBits)
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
          case ".nan" | "nan"                                         => "NaN"
          case "∞" | "+∞" | "infinity" | "+infinity" | "inf" | "+inf" =>
            "Infinity"
          case "-∞" | "-infinity" | "-inf" => "-Infinity"
          case _                           => cleaned
        }
      try Some(java.lang.Double.parseDouble(normalized))
      catch {
        case _: NumberFormatException => None
      }
    }
  }

  private def finiteDoubleToNearestInt(d: Double): BigInteger = {
    val rounded = java.lang.Math.rint(d)
    if (rounded == 0.0d) BigInteger.ZERO
    else if (rounded >= Long.MinValue.toDouble && rounded <= Long.MaxValue.toDouble)
      BigInteger.valueOf(rounded.toLong)
    else {
      val bits = java.lang.Double.doubleToRawLongBits(rounded)
      val isNegative = (bits >>> 63) == 1L
      val exponentBits = ((bits >>> 52) & 0x7ffL).toInt
      val significandBits = bits & 0x000fffffffffffffL
      val unbiasedExponent = exponentBits - 1023
      val significand =
        if (exponentBits == 0) BigInteger.valueOf(significandBits)
        else BigInteger.valueOf((1L << 52) | significandBits)

      val shift = unbiasedExponent - 52
      val magnitude =
        if (shift >= 0) significand.shiftLeft(shift)
        else significand.shiftRight(-shift)

      if (isNegative) magnitude.negate
      else magnitude
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
    ExternalValue(addInt(intRaw(a), intRaw(b)))

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
    ExternalValue(subInt(intRaw(a), intRaw(b)))

  def subf(a: Value, b: Value): Value =
    vf(d(a) - d(b))

  def mul(a: Value, b: Value): Value =
    ExternalValue(mulInt(intRaw(a), intRaw(b)))

  def timesf(a: Value, b: Value): Value =
    vf(d(a) * d(b))

  def eq_Int(a: Value, b: Value): Value =
    if (Value.intEq(intRaw(a), intRaw(b))) True else False

  def cmp_Int(a: Value, b: Value): Value =
    Comparison.fromInt(Value.intCompare(intRaw(a), intRaw(b)))

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
    vf(copySignDouble(d(a), d(b)))
  def isNaN_Float64(a: Value): Value =
    bool(java.lang.Double.isNaN(d(a)))
  def isInfinite_Float64(a: Value): Value =
    bool(java.lang.Double.isInfinite(d(a)))
  def float64_to_String(a: Value): Value = {
    val bits = java.lang.Double.doubleToRawLongBits(d(a))
    val value = java.lang.Double.longBitsToDouble(bits)
    if (java.lang.Double.isNaN(value))
      Value.Str(s"${NaNBitsPrefix}${toUnsignedHex64(bits)}")
    else
      Value.Str(Lit.Float64.toLiteralString(Lit.Float64.fromRawLongBits(bits)))
  }
  def string_to_Float64(a: Value): Value =
    a match {
      case Value.Str(s) =>
        parseFloat64String(s) match {
          case Some(v) => Value.VOption.some(vf(v))
          case None    => Value.VOption.none
        }
      case other => sys.error(s"type error: $other")
    }
  def int_bits_to_Float64(a: Value): Value =
    vf(java.lang.Double.longBitsToDouble(i(a).longValue()))
  def float64_bits_to_Int(a: Value): Value =
    Value.VInt(
      unsignedLongToBigInteger(java.lang.Double.doubleToRawLongBits(d(a)))
    )
  def float64_to_Int(a: Value): Value = {
    val value = d(a)
    if (java.lang.Double.isFinite(value))
      Value.VOption.some(Value.VInt(finiteDoubleToNearestInt(value)))
    else Value.VOption.none
  }
  def int_to_Float64(a: Value): Value =
    intRaw(a) match {
      case i: java.lang.Integer => vf(i.doubleValue())
      case bi: BigInteger       => vf(bi.doubleValue())
    }

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

  private val ProgTagPure = 0
  private val ProgTagRaise = 1
  private val ProgTagFlatMap = 2
  private val ProgTagRecover = 3
  private val ProgTagApplyFix = 4
  private val ProgTagEffect = 5

  private val IOErrorTagInvalidArgument = 12
  private val IOErrorTagInvalidUtf8 = 13

  sealed private trait ProgStack derives CanEqual
  private case object ProgStackDone extends ProgStack
  final private case class ProgStackFlatMap(fn: Value, tail: ProgStack)
      extends ProgStack
  final private case class ProgStackRecover(fn: Value, tail: ProgStack)
      extends ProgStack

  final case class ProgRuntimeState(
      stdin: Array[Byte],
      var stdinOffset: Int,
      stdout: StringBuilder,
      stderr: StringBuilder
  )

  final case class ProgRunResult(
      result: Either[Value, Value],
      stdout: String,
      stderr: String
  )

  private val currentProgRuntime: DynamicVariable[Option[ProgRuntimeState]] =
    new DynamicVariable(None)

  private def callFn1(fn: Value, arg: Value): Value =
    fn.asFn(NonEmptyList(arg, Nil))

  private def ioerror_known(tag: Int, context: String): Value =
    SumValue(tag, ProductValue.single(Str(context)))

  private def ioerror_invalid_argument(context: String): Value =
    ioerror_known(IOErrorTagInvalidArgument, context)

  private def ioerror_invalid_utf8(context: String): Value =
    ioerror_known(IOErrorTagInvalidUtf8, context)

  private def prog_effect(arg: Value, fn: Value => Value): Value =
    SumValue(
      ProgTagEffect,
      ProductValue.fromList(
        arg :: FnValue { case NonEmptyList(a, _) => fn(a) } :: Nil
      )
    )

  def prog_pure(a: Value): Value =
    SumValue(ProgTagPure, ProductValue.single(a))

  def prog_raise_error(e: Value): Value =
    SumValue(ProgTagRaise, ProductValue.single(e))

  def prog_flat_map(prog: Value, fn: Value): Value =
    SumValue(ProgTagFlatMap, ProductValue.fromList(prog :: fn :: Nil))

  def prog_recover(prog: Value, fn: Value): Value =
    SumValue(ProgTagRecover, ProductValue.fromList(prog :: fn :: Nil))

  def prog_apply_fix(a: Value, fn: Value): Value =
    SumValue(ProgTagApplyFix, ProductValue.fromList(a :: fn :: Nil))

  private def decodeUtf8(bytes: Array[Byte]): Option[String] = {
    val decoder =
      StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)

    try Some(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch {
      case _: CharacterCodingException => None
    }
  }

  private def runtimeRead(
      runtime: ProgRuntimeState,
      count: Int
  ): Array[Byte] = {
    val remaining = runtime.stdin.length - runtime.stdinOffset
    if (count <= 0 || remaining <= 0) Array.emptyByteArray
    else {
      val toRead = if (count <= remaining) count else remaining
      val out = java.util.Arrays.copyOfRange(
        runtime.stdin,
        runtime.stdinOffset,
        runtime.stdinOffset + toRead
      )
      runtime.stdinOffset = runtime.stdinOffset + toRead
      out
    }
  }

  private def runtimeReadOne(runtime: ProgRuntimeState): Option[Byte] = {
    val remaining = runtime.stdin.length - runtime.stdinOffset
    if (remaining <= 0) None
    else {
      val b = runtime.stdin(runtime.stdinOffset)
      runtime.stdinOffset = runtime.stdinOffset + 1
      Some(b)
    }
  }

  private def read_utf8_chunk(
      runtime: ProgRuntimeState,
      requestedRaw: BigInteger
  ): Either[Value, String] =
    if (requestedRaw.signum < 0) {
      Left(
        ioerror_invalid_argument(
          s"read_stdin_utf8_bytes negative argument: ${requestedRaw.toString}"
        )
      )
    } else {
      val requested =
        if (requestedRaw.signum == 0) 1
        else if (requestedRaw.compareTo(MaxIntBI) > 0) Int.MaxValue
        else requestedRaw.intValue

      val initial = runtimeRead(runtime, requested)
      if (initial.isEmpty) Right("")
      else
        decodeUtf8(initial) match {
          case Some(s) => Right(s)
          case None    =>
            if (initial.length < requested)
              Left(
                ioerror_invalid_utf8("decoding bytes from stdin")
              )
            else {
              val bytes = new java.io.ByteArrayOutputStream(initial.length + 4)
              bytes.write(initial)
              var extras = 0
              var done = false
              var result: Option[String] = None

              while (!done && extras < 4) {
                runtimeReadOne(runtime) match {
                  case None       => done = true
                  case Some(byte) =>
                    bytes.write(byte.toInt & 0xff)
                    extras = extras + 1
                    decodeUtf8(bytes.toByteArray) match {
                      case Some(valid) =>
                        result = Some(valid)
                        done = true
                      case None => ()
                    }
                }
              }

              result match {
                case Some(valid) => Right(valid)
                case None        =>
                  Left(ioerror_invalid_utf8("decoding bytes from stdin"))
              }
            }
        }
    }

  private val IOErrorTagNotFound = 0
  private val IOErrorTagAccessDenied = 1
  private val IOErrorTagAlreadyExists = 2
  private val IOErrorTagNotDirectory = 3
  private val IOErrorTagNotEmpty = 5
  private val IOErrorTagBadFileDescriptor = 14
  private val IOErrorTagInterrupted = 15
  private val IOErrorTagBrokenPipe = 18
  private val IOErrorTagUnsupported = 19
  private val IOErrorTagOther = 20

  private val OneBillionBI = BigInteger.valueOf(1000000000L)
  private val OneMillionBI = BigInteger.valueOf(1000000L)
  private val LongMaxBI = BigInteger.valueOf(Long.MaxValue)

  private sealed trait HandleValue derives CanEqual
  private case object HandleStdin extends HandleValue
  private case object HandleStdout extends HandleValue
  private case object HandleStderr extends HandleValue
  private final case class ReaderHandle(
      reader: Reader,
      var closed: Boolean = false
  ) extends HandleValue
  private final case class WriterHandle(
      writer: Writer,
      var closed: Boolean = false
  ) extends HandleValue

  final case class ProcessValue(
      process: java.lang.Process,
      var cachedExitCode: Option[Int]
  )

  private val defaultPathSep: String =
    if (Platform.detectOs() eq OsPlatformId.Windows) "\\"
    else "/"

  val core_path_sep: Value = Str(defaultPathSep)
  val core_stdin: Value = ExternalValue(HandleStdin)
  val core_stdout: Value = ExternalValue(HandleStdout)
  val core_stderr: Value = ExternalValue(HandleStderr)

  private lazy val systemStdinReader: BufferedReader =
    new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8))

  private def normalizePathString(raw: String): String =
    raw.replace('\\', '/')

  private def instantValueFromNanos(nanos: BigInteger): Value =
    ProductValue.single(VInt(nanos))

  private def durationValueFromNanos(nanos: BigInteger): Value =
    ProductValue.single(VInt(nanos))

  private def fileKindValue(kindTag: Int): Value =
    SumValue(kindTag, UnitValue)

  private def ioerror_other(
      context: String,
      code: Int,
      message: String
  ): Value =
    SumValue(
      IOErrorTagOther,
      ProductValue.fromList(Str(context) :: VInt(code) :: Str(message) :: Nil)
    )

  private def ioerror_from_throwable(context: String, t: Throwable): Value =
    t match {
      case _: NoSuchFileException =>
        ioerror_known(IOErrorTagNotFound, context)
      case _: AccessDeniedException | _: SecurityException =>
        ioerror_known(IOErrorTagAccessDenied, context)
      case _: FileAlreadyExistsException =>
        ioerror_known(IOErrorTagAlreadyExists, context)
      case _: NotDirectoryException =>
        ioerror_known(IOErrorTagNotDirectory, context)
      case _: DirectoryNotEmptyException =>
        ioerror_known(IOErrorTagNotEmpty, context)
      case _: java.nio.channels.NonReadableChannelException =>
        ioerror_known(IOErrorTagBadFileDescriptor, context)
      case _: java.nio.channels.NonWritableChannelException =>
        ioerror_known(IOErrorTagBadFileDescriptor, context)
      case _: java.nio.channels.ClosedChannelException =>
        ioerror_known(IOErrorTagBadFileDescriptor, context)
      case _: java.io.EOFException =>
        ioerror_known(IOErrorTagBrokenPipe, context)
      case _: java.io.InterruptedIOException =>
        ioerror_known(IOErrorTagInterrupted, context)
      case _: UnsupportedOperationException =>
        ioerror_known(IOErrorTagUnsupported, context)
      case _: InvalidPathException =>
        ioerror_invalid_argument(context)
      case _: IllegalArgumentException =>
        ioerror_invalid_argument(context)
      case _: CharacterCodingException =>
        ioerror_invalid_utf8(context)
      case other =>
        ioerror_other(
          context,
          0,
          Option(other.getMessage).getOrElse(other.getClass.getSimpleName)
        )
    }

  private def asPathString(path: Value): Either[Value, String] =
    path match {
      case p: ProductValue if p.values.length == 1 =>
        p.get(0) match {
          case Str(s) => Right(s)
          case _ =>
            Left(ioerror_invalid_argument("invalid Path value"))
        }
      case _ =>
        Left(ioerror_invalid_argument("invalid Path value"))
    }

  private def asJavaPath(path: Value, context: String): Either[Value, JPath] =
    asPathString(path).flatMap { raw =>
      try Right(Paths.get(raw))
      catch {
        case _: InvalidPathException =>
          Left(ioerror_invalid_argument(context))
      }
    }

  private def asHandleValue(v: Value): Either[Value, HandleValue] =
    v match {
      case ExternalValue(h: HandleValue) => Right(h)
      case _ =>
        Left(ioerror_invalid_argument("expected Handle"))
    }

  private def asProcessValue(v: Value): Either[Value, ProcessValue] =
    v match {
      case ExternalValue(p: ProcessValue) => Right(p)
      case _ =>
        Left(ioerror_invalid_argument("expected Process"))
    }

  private def asBool(v: Value): Either[Value, Boolean] =
    v match {
      case s: SumValue if s.variant == 1 && (s.value == UnitValue) => Right(true)
      case s: SumValue if s.variant == 0 && (s.value == UnitValue) => Right(false)
      case _ =>
        Left(ioerror_invalid_argument("expected Bool"))
    }

  private def asStringList(v: Value): Either[Value, List[String]] =
    Value.VList.unapply(v) match {
      case None =>
        Left(ioerror_invalid_argument("expected List[String]"))
      case Some(items) =>
        items.foldRight(Right(Nil): Either[Value, List[String]]) { (item, acc) =>
          for {
            tail <- acc
            str <- item match {
              case Str(s) => Right(s)
              case _      =>
                Left(ioerror_invalid_argument("expected List[String]"))
            }
          } yield str :: tail
        }
    }

  private def asOpenModeTag(v: Value): Either[Value, Int] =
    v match {
      case s: SumValue =>
        if ((s.variant >= 0) && (s.variant <= 2)) Right(s.variant)
        else Left(ioerror_invalid_argument("invalid OpenMode value"))
      case _ =>
        Left(ioerror_invalid_argument("invalid OpenMode value"))
    }

  private def asDurationNanos(v: Value): Either[Value, BigInteger] =
    v match {
      case p: ProductValue if p.values.length == 1 =>
        p.get(0) match {
          case VInt(i) => Right(i)
          case _       =>
            Left(ioerror_invalid_argument("invalid Duration value"))
        }
      case _ =>
        Left(ioerror_invalid_argument("invalid Duration value"))
    }

  private def optionValue(v: Option[Value]): Value =
    v match {
      case Some(value) => VOption.some(value)
      case None        => VOption.none
    }

  private def readFromReader(
      reader: Reader,
      maxChars: Int,
      context: String
  ): Either[Value, Option[String]] =
    try {
      val buffer = new Array[Char](maxChars)
      val count = reader.read(buffer, 0, maxChars)
      if (count <= 0) Right(None)
      else Right(Some(new String(buffer, 0, count)))
    } catch {
      case NonFatal(t) =>
        Left(ioerror_from_throwable(context, t))
    }

  private def writeToWriter(
      writer: Writer,
      text: String,
      context: String
  ): Either[Value, Unit] =
    try {
      writer.write(text)
      Right(())
    } catch {
      case NonFatal(t) =>
        Left(ioerror_from_throwable(context, t))
    }

  private def flushWriter(writer: Writer, context: String): Either[Value, Unit] =
    try {
      writer.flush()
      Right(())
    } catch {
      case NonFatal(t) =>
        Left(ioerror_from_throwable(context, t))
    }

  private def closeReader(reader: Reader, context: String): Either[Value, Unit] =
    try {
      reader.close()
      Right(())
    } catch {
      case NonFatal(t) =>
        Left(ioerror_from_throwable(context, t))
    }

  private def closeWriter(writer: Writer, context: String): Either[Value, Unit] =
    try {
      writer.close()
      Right(())
    } catch {
      case NonFatal(t) =>
        Left(ioerror_from_throwable(context, t))
    }

  private def prog_effect2(
      a: Value,
      b: Value,
      fn: (Value, Value) => Value
  ): Value =
    prog_effect(
      Value.Tuple(a, b),
      {
        case p: ProductValue if p.values.length == 2 =>
          fn(p.get(0), p.get(1))
        case other =>
          sys.error(s"invalid effect2 payload: $other")
      }
    )

  private def prog_effect3(
      a: Value,
      b: Value,
      c: Value,
      fn: (Value, Value, Value) => Value
  ): Value =
    prog_effect(
      Value.Tuple(a, b, c),
      {
        case p: ProductValue if p.values.length == 3 =>
          fn(p.get(0), p.get(1), p.get(2))
        case other =>
          sys.error(s"invalid effect3 payload: $other")
      }
    )

  private def core_read_utf8_impl(
      handleValue: Value,
      maxCharsValue: Value
  ): Either[Value, Option[String]] =
    for {
      handle <- asHandleValue(handleValue)
      maxCharsBI <- maxCharsValue match {
        case VInt(i) => Right(i)
        case _       => Left(ioerror_invalid_argument("expected Int for max_chars"))
      }
      maxChars <- {
        if (maxCharsBI.signum <= 0)
          Left(ioerror_invalid_argument("read_utf8 max_chars must be > 0"))
        else if (maxCharsBI.compareTo(MaxIntBI) > 0) Right(Int.MaxValue)
        else Right(maxCharsBI.intValue)
      }
      result <- handle match {
        case HandleStdin =>
          currentProgRuntime.value match {
            case Some(runtime) =>
              read_utf8_chunk(runtime, maxCharsBI).map { chunk =>
                if (chunk.isEmpty) None else Some(chunk)
              }
            case None =>
              readFromReader(systemStdinReader, maxChars, "reading from stdin")
          }
        case ReaderHandle(reader, closed) =>
          if (closed)
            Left(ioerror_known(IOErrorTagBadFileDescriptor, "reading closed handle"))
          else readFromReader(reader, maxChars, "reading utf8 from handle")
        case HandleStdout | HandleStderr | WriterHandle(_, _) =>
          Left(ioerror_known(IOErrorTagBadFileDescriptor, "reading from write-only handle"))
      }
    } yield result

  private def core_write_utf8_impl(
      handleValue: Value,
      textValue: Value
  ): Either[Value, Unit] =
    for {
      handle <- asHandleValue(handleValue)
      text <- textValue match {
        case Str(s) => Right(s)
        case _      => Left(ioerror_invalid_argument("expected String for write_utf8"))
      }
      _ <- handle match {
        case HandleStdout =>
          currentProgRuntime.value match {
            case Some(runtime) =>
              runtime.stdout.append(text)
              Right(())
            case None =>
              try {
                System.out.print(text)
                Right(())
              } catch {
                case NonFatal(t) =>
                  Left(ioerror_from_throwable("writing to stdout", t))
              }
          }
        case HandleStderr =>
          currentProgRuntime.value match {
            case Some(runtime) =>
              runtime.stderr.append(text)
              Right(())
            case None =>
              try {
                System.err.print(text)
                Right(())
              } catch {
                case NonFatal(t) =>
                  Left(ioerror_from_throwable("writing to stderr", t))
              }
          }
        case WriterHandle(writer, closed) =>
          if (closed)
            Left(ioerror_known(IOErrorTagBadFileDescriptor, "writing closed handle"))
          else writeToWriter(writer, text, "writing utf8 to handle")
        case HandleStdin | ReaderHandle(_, _) =>
          Left(ioerror_known(IOErrorTagBadFileDescriptor, "writing to read-only handle"))
      }
    } yield ()

  private def core_flush_impl(handleValue: Value): Either[Value, Unit] =
    for {
      handle <- asHandleValue(handleValue)
      _ <- handle match {
        case HandleStdout =>
          currentProgRuntime.value match {
            case Some(_) => Right(())
            case None    =>
              try {
                System.out.flush()
                Right(())
              } catch {
                case NonFatal(t) =>
                  Left(ioerror_from_throwable("flushing stdout", t))
              }
          }
        case HandleStderr =>
          currentProgRuntime.value match {
            case Some(_) => Right(())
            case None    =>
              try {
                System.err.flush()
                Right(())
              } catch {
                case NonFatal(t) =>
                  Left(ioerror_from_throwable("flushing stderr", t))
              }
          }
        case WriterHandle(writer, closed) =>
          if (closed)
            Left(ioerror_known(IOErrorTagBadFileDescriptor, "flushing closed handle"))
          else flushWriter(writer, "flushing handle")
        case HandleStdin | ReaderHandle(_, _) =>
          Right(())
      }
    } yield ()

  private def core_close_impl(handleValue: Value): Either[Value, Unit] =
    for {
      handle <- asHandleValue(handleValue)
      _ <- handle match {
        case HandleStdin | HandleStdout | HandleStderr =>
          Right(())
        case rh @ ReaderHandle(reader, closed) =>
          if (closed) Right(())
          else closeReader(reader, "closing handle").map { _ =>
            rh.closed = true
          }
        case wh @ WriterHandle(writer, closed) =>
          if (closed) Right(())
          else closeWriter(writer, "closing handle").map { _ =>
            wh.closed = true
          }
      }
    } yield ()

  private def basicFileStatValue(attrs: BasicFileAttributes): Value = {
    val kindTag =
      if (attrs.isSymbolicLink) 2
      else if (attrs.isRegularFile) 0
      else if (attrs.isDirectory) 1
      else 3
    val mtimeInstant = attrs.lastModifiedTime.toInstant
    val mtimeNanos =
      BigInteger
        .valueOf(mtimeInstant.getEpochSecond)
        .multiply(OneBillionBI)
        .add(BigInteger.valueOf(mtimeInstant.getNano.toLong))
    ProductValue.fromList(
      fileKindValue(kindTag) ::
        VInt(BigInteger.valueOf(attrs.size)) ::
        instantValueFromNanos(mtimeNanos) :: Nil
    )
  }

  private def removePathRecursive(path: JPath): Either[Value, Unit] =
    try {
      val attrs = Files.readAttributes(
        path,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      if (attrs.isDirectory && !attrs.isSymbolicLink) {
        val directory = Files.newDirectoryStream(path)
        try {
          val iterator = directory.iterator()
          while (iterator.hasNext) {
            removePathRecursive(iterator.next()) match {
              case Left(err) => return Left(err)
              case Right(_)  => ()
            }
          }
        } finally {
          directory.close()
        }
        Files.delete(path)
      } else {
        Files.delete(path)
      }
      Right(())
    } catch {
      case NonFatal(t) =>
        Left(ioerror_from_throwable(s"removing path: ${path.toString}", t))
    }

  private def stdioMode(
      value: Value,
      streamName: String
  ): Either[Value, Int] =
    value match {
      case s: SumValue =>
        s.variant match {
          case 0 | 1 | 2 => Right(s.variant)
          case 3          =>
            Left(
              ioerror_known(
                IOErrorTagUnsupported,
                s"spawn ${streamName}: UseHandle is unsupported in JVM evaluator"
              )
            )
          case _          =>
            Left(ioerror_invalid_argument(s"invalid Stdio value for ${streamName}"))
        }
      case _ =>
        Left(ioerror_invalid_argument(s"invalid Stdio value for ${streamName}"))
    }

  private def spawnResultValue(
      processValue: ProcessValue,
      stdinHandle: Option[HandleValue],
      stdoutHandle: Option[HandleValue],
      stderrHandle: Option[HandleValue]
  ): Value =
    ProductValue.fromList(
      ExternalValue(processValue) ::
        optionValue(stdinHandle.map(ExternalValue(_))) ::
        optionValue(stdoutHandle.map(ExternalValue(_))) ::
        optionValue(stderrHandle.map(ExternalValue(_))) :: Nil
    )

  def prog_core_read_utf8(handle: Value, maxChars: Value): Value =
    prog_effect2(
      handle,
      maxChars,
      (h, n) =>
        core_read_utf8_impl(h, n) match {
          case Right(result) =>
            prog_pure(
              optionValue(result.map(Str(_)))
            )
          case Left(err)     =>
            prog_raise_error(err)
        }
    )

  def prog_core_write_utf8(handle: Value, text: Value): Value =
    prog_effect2(
      handle,
      text,
      (h, t) =>
        core_write_utf8_impl(h, t) match {
          case Right(_)  => prog_pure(UnitValue)
          case Left(err) => prog_raise_error(err)
        }
    )

  def prog_core_flush(handle: Value): Value =
    prog_effect(
      handle,
      h =>
        core_flush_impl(h) match {
          case Right(_)  => prog_pure(UnitValue)
          case Left(err) => prog_raise_error(err)
        }
    )

  def prog_core_close(handle: Value): Value =
    prog_effect(
      handle,
      h =>
        core_close_impl(h) match {
          case Right(_)  => prog_pure(UnitValue)
          case Left(err) => prog_raise_error(err)
        }
    )

  def prog_core_open_file(path: Value, mode: Value): Value =
    prog_effect2(
      path,
      mode,
      (pathValue, modeValue) => {
        val result = for {
          javaPath <- asJavaPath(pathValue, "invalid path for open_file")
          modeTag <- asOpenModeTag(modeValue)
          handle <- {
            try {
              modeTag match {
                case 0 =>
                  Right(
                    ExternalValue(
                      ReaderHandle(
                        Files.newBufferedReader(javaPath, StandardCharsets.UTF_8)
                      )
                    )
                  )
                case 1 =>
                  val options: Array[OpenOption] = Array(
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
                  )
                  Right(
                    ExternalValue(
                      WriterHandle(
                        Files.newBufferedWriter(
                          javaPath,
                          StandardCharsets.UTF_8,
                          options*
                        )
                      )
                    )
                  )
                case 2 =>
                  val options: Array[OpenOption] = Array(
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
                  )
                  Right(
                    ExternalValue(
                      WriterHandle(
                        Files.newBufferedWriter(
                          javaPath,
                          StandardCharsets.UTF_8,
                          options*
                        )
                      )
                    )
                  )
                case _ =>
                  Left(ioerror_invalid_argument("invalid OpenMode value"))
              }
            } catch {
              case NonFatal(t) =>
                Left(ioerror_from_throwable("open_file", t))
            }
          }
        } yield handle

        result match {
          case Right(handleOut) => prog_pure(handleOut)
          case Left(err)        => prog_raise_error(err)
        }
      }
    )

  def prog_core_list_dir(path: Value): Value =
    prog_effect(
      path,
      pathValue => {
        val result = for {
          javaPath <- asJavaPath(pathValue, "invalid path for list_dir")
          children <- {
            try {
              val stream = Files.list(javaPath)
              try {
                val iterator = stream.iterator()
                val builder = List.newBuilder[String]
                while (iterator.hasNext) {
                  builder += normalizePathString(iterator.next().toString)
                }
                Right(builder.result().sorted.map(str => ProductValue.single(Str(str))))
              } finally {
                stream.close()
              }
            } catch {
              case NonFatal(t) =>
                Left(ioerror_from_throwable("list_dir", t))
            }
          }
        } yield Value.VList(children)

        result match {
          case Right(paths) => prog_pure(paths)
          case Left(err)    => prog_raise_error(err)
        }
      }
    )

  def prog_core_stat(path: Value): Value =
    prog_effect(
      path,
      pathValue => {
        val result = for {
          javaPath <- asJavaPath(pathValue, "invalid path for stat")
          statValue <- {
            try {
              val attrs = Files.readAttributes(
                javaPath,
                classOf[BasicFileAttributes],
                LinkOption.NOFOLLOW_LINKS
              )
              Right(Some(basicFileStatValue(attrs)))
            } catch {
              case _: NoSuchFileException =>
                Right(None)
              case NonFatal(t)            =>
                Left(ioerror_from_throwable("stat", t))
            }
          }
        } yield optionValue(statValue)

        result match {
          case Right(v)   => prog_pure(v)
          case Left(err)  => prog_raise_error(err)
        }
      }
    )

  def prog_core_mkdir(path: Value, recursive: Value): Value =
    prog_effect2(
      path,
      recursive,
      (pathValue, recursiveValue) => {
        val result = for {
          javaPath <- asJavaPath(pathValue, "invalid path for mkdir")
          recursiveFlag <- asBool(recursiveValue)
          _ <- {
            try {
              if (recursiveFlag) Files.createDirectories(javaPath)
              else Files.createDirectory(javaPath)
              Right(())
            } catch {
              case NonFatal(t) =>
                Left(ioerror_from_throwable("mkdir", t))
            }
          }
        } yield ()

        result match {
          case Right(_)  => prog_pure(UnitValue)
          case Left(err) => prog_raise_error(err)
        }
      }
    )

  def prog_core_remove(path: Value, recursive: Value): Value =
    prog_effect2(
      path,
      recursive,
      (pathValue, recursiveValue) => {
        val result = for {
          javaPath <- asJavaPath(pathValue, "invalid path for remove")
          recursiveFlag <- asBool(recursiveValue)
          _ <- {
            if (recursiveFlag) removePathRecursive(javaPath)
            else {
              try {
                Files.delete(javaPath)
                Right(())
              } catch {
                case NonFatal(t) =>
                  Left(ioerror_from_throwable("remove", t))
              }
            }
          }
        } yield ()

        result match {
          case Right(_)  => prog_pure(UnitValue)
          case Left(err) => prog_raise_error(err)
        }
      }
    )

  def prog_core_rename(from: Value, to: Value): Value =
    prog_effect2(
      from,
      to,
      (fromValue, toValue) => {
        val result = for {
          fromPath <- asJavaPath(fromValue, "invalid source path for rename")
          toPath <- asJavaPath(toValue, "invalid destination path for rename")
          _ <- {
            try {
              Files.move(fromPath, toPath)
              Right(())
            } catch {
              case NonFatal(t) =>
                Left(ioerror_from_throwable("rename", t))
            }
          }
        } yield ()

        result match {
          case Right(_)  => prog_pure(UnitValue)
          case Left(err) => prog_raise_error(err)
        }
      }
    )

  def prog_core_get_env(name: Value): Value =
    prog_effect(
      name,
      n =>
        n match {
          case Str(s) =>
            prog_pure(optionValue(Option(System.getenv(s)).map(Str(_))))
          case _      =>
            prog_raise_error(ioerror_invalid_argument("get_env expects String"))
        }
    )

  def prog_core_spawn(cmd: Value, args: Value, stdio: Value): Value =
    prog_effect3(
      cmd,
      args,
      stdio,
      (cmdValue, argsValue, stdioValue) => {
        val result = for {
          cmdStr <- cmdValue match {
            case Str(s) => Right(s)
            case _      => Left(ioerror_invalid_argument("spawn command must be String"))
          }
          argList <- asStringList(argsValue)
          stdioModes <- stdioValue match {
            case p: ProductValue if p.values.length == 3 =>
              for {
                inMode <- stdioMode(p.get(0), "stdin")
                outMode <- stdioMode(p.get(1), "stdout")
                errMode <- stdioMode(p.get(2), "stderr")
              } yield (inMode, outMode, errMode)
            case _ =>
              Left(ioerror_invalid_argument("invalid StdioConfig value"))
          }
          spawnResult <- {
            val (stdinMode, stdoutMode, stderrMode) = stdioModes
            try {
              val command = new java.util.ArrayList[String]()
              command.add(cmdStr)
              argList.foreach(command.add)
              val pb = new java.lang.ProcessBuilder(command)

              stdinMode match {
                case 0 => pb.redirectInput(java.lang.ProcessBuilder.Redirect.INHERIT)
                case 1 => pb.redirectInput(java.lang.ProcessBuilder.Redirect.PIPE)
                case 2 => pb.redirectInput(java.lang.ProcessBuilder.Redirect.DISCARD)
                case _ => ()
              }
              stdoutMode match {
                case 0 => pb.redirectOutput(java.lang.ProcessBuilder.Redirect.INHERIT)
                case 1 => pb.redirectOutput(java.lang.ProcessBuilder.Redirect.PIPE)
                case 2 => pb.redirectOutput(java.lang.ProcessBuilder.Redirect.DISCARD)
                case _ => ()
              }
              stderrMode match {
                case 0 => pb.redirectError(java.lang.ProcessBuilder.Redirect.INHERIT)
                case 1 => pb.redirectError(java.lang.ProcessBuilder.Redirect.PIPE)
                case 2 => pb.redirectError(java.lang.ProcessBuilder.Redirect.DISCARD)
                case _ => ()
              }

              val process = pb.start()
              val processValue = ProcessValue(process, None)
              val stdinHandle =
                if (stdinMode == 1)
                  Some(
                    WriterHandle(
                      new BufferedWriter(
                        new OutputStreamWriter(
                          process.getOutputStream,
                          StandardCharsets.UTF_8
                        )
                      )
                    )
                  )
                else None
              val stdoutHandle =
                if (stdoutMode == 1)
                  Some(
                    ReaderHandle(
                      new BufferedReader(
                        new InputStreamReader(
                          process.getInputStream,
                          StandardCharsets.UTF_8
                        )
                      )
                    )
                  )
                else None
              val stderrHandle =
                if (stderrMode == 1)
                  Some(
                    ReaderHandle(
                      new BufferedReader(
                        new InputStreamReader(
                          process.getErrorStream,
                          StandardCharsets.UTF_8
                        )
                      )
                    )
                  )
                else None

              Right(
                spawnResultValue(processValue, stdinHandle, stdoutHandle, stderrHandle)
              )
            } catch {
              case NonFatal(t) =>
                Left(ioerror_from_throwable("spawn", t))
            }
          }
        } yield spawnResult

        result match {
          case Right(v)   => prog_pure(v)
          case Left(err)  => prog_raise_error(err)
        }
      }
    )

  def prog_core_wait(process: Value): Value =
    prog_effect(
      process,
      p => {
        val result = for {
          procValue <- asProcessValue(p)
          exitCode <- procValue.cachedExitCode match {
            case Some(code) => Right(code)
            case None       =>
              try {
                val code = procValue.process.waitFor()
                procValue.cachedExitCode = Some(code)
                Right(code)
              } catch {
                case _: InterruptedException =>
                  Thread.currentThread().interrupt()
                  Left(ioerror_known(IOErrorTagInterrupted, "wait interrupted"))
                case NonFatal(t)             =>
                  Left(ioerror_from_throwable("wait", t))
              }
          }
        } yield exitCode

        result match {
          case Right(code) => prog_pure(VInt(code))
          case Left(err)   => prog_raise_error(err)
        }
      }
    )

  private def nowWallEpochNanos(): BigInteger = {
    val instant = java.time.Instant.now()
    BigInteger
      .valueOf(instant.getEpochSecond)
      .multiply(OneBillionBI)
      .add(BigInteger.valueOf(instant.getNano.toLong))
  }

  val prog_core_now_wall: Value =
    prog_effect(UnitValue, _ => prog_pure(instantValueFromNanos(nowWallEpochNanos())))

  val prog_core_now_mono: Value =
    prog_effect(
      UnitValue,
      _ => prog_pure(durationValueFromNanos(BigInteger.valueOf(System.nanoTime())))
    )

  def prog_core_sleep(duration: Value): Value =
    prog_effect(
      duration,
      d => {
        val result = for {
          nanos <- asDurationNanos(d)
          _ <- {
            if (nanos.signum < 0)
              Left(ioerror_invalid_argument("sleep duration must be non-negative"))
            else {
              val millisBI = nanos.divide(OneMillionBI)
              val extraNanos = nanos.remainder(OneMillionBI).intValue
              val millis =
                if (millisBI.compareTo(LongMaxBI) > 0) Long.MaxValue
                else millisBI.longValue
              try {
                Thread.sleep(millis, extraNanos)
                Right(())
              } catch {
                case _: InterruptedException =>
                  Thread.currentThread().interrupt()
                  Left(ioerror_known(IOErrorTagInterrupted, "sleep interrupted"))
                case NonFatal(t)             =>
                  Left(ioerror_from_throwable("sleep", t))
              }
            }
          }
        } yield ()

        result match {
          case Right(_)  => prog_pure(UnitValue)
          case Left(err) => prog_raise_error(err)
        }
      }
    )

  private def prog_step_fix(arg: Value, fixfn: Value): Value = {
    lazy val fixed: Value =
      FnValue { case NonEmptyList(a, _) =>
        prog_apply_fix(a, fixfn)
      }

    callFn1(callFn1(fixfn, fixed), arg)
  }

  private def run_prog(prog: Value): Either[Value, Value] = {
    var stack: ProgStack = ProgStackDone
    var arg: Value = prog

    while (true) {
      val sum = arg.asSum
      sum.variant match {
        case ProgTagFlatMap =>
          stack = ProgStackFlatMap(sum.value.get(1), stack)
          arg = sum.value.get(0)

        case ProgTagPure =>
          val item = sum.value.get(0)
          var searching = true
          while (searching) {
            stack match {
              case ProgStackDone =>
                return Right(item)
              case ProgStackFlatMap(fn, tail) =>
                stack = tail
                arg = callFn1(fn, item)
                searching = false
              case ProgStackRecover(_, tail) =>
                stack = tail
            }
          }

        case ProgTagRaise =>
          val err = sum.value.get(0)
          var searching = true
          while (searching) {
            stack match {
              case ProgStackDone =>
                return Left(err)
              case ProgStackFlatMap(_, tail) =>
                stack = tail
              case ProgStackRecover(fn, tail) =>
                stack = tail
                arg = callFn1(fn, err)
                searching = false
            }
          }

        case ProgTagRecover =>
          stack = ProgStackRecover(sum.value.get(1), stack)
          arg = sum.value.get(0)

        case ProgTagApplyFix =>
          arg = prog_step_fix(sum.value.get(0), sum.value.get(1))

        case ProgTagEffect =>
          arg = callFn1(sum.value.get(1), sum.value.get(0))

        case other =>
          sys.error(s"invalid Prog tag: $other")
      }
    }

    // unreachable
    Left(Str("unreachable"))
  }

  def runProg(
      prog: Value,
      stdin: String = ""
  ): ProgRunResult = {
    val runtime =
      ProgRuntimeState(
        stdin = stdin.getBytes(StandardCharsets.UTF_8),
        stdinOffset = 0,
        stdout = new StringBuilder,
        stderr = new StringBuilder
      )

    val result = currentProgRuntime.withValue(Some(runtime)) {
      run_prog(prog)
    }
    ProgRunResult(result, runtime.stdout.toString, runtime.stderr.toString)
  }

  private def unwrapMain(value: Value): Value =
    value match {
      case p: ProductValue if p.values.nonEmpty => unwrapMain(p.get(0))
      case other                                => other
    }

  def runProgMain(
      main: Value,
      args: List[String],
      stdin: String = ""
  ): ProgRunResult = {
    val argList = VList(args.map(Str(_)))
    val prog = unwrapMain(main) match {
      case fn: FnValue => callFn1(fn, argList)
      case other       => other
    }
    runProg(prog, stdin)
  }

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
    ExternalValue(andInt(intRaw(a), intRaw(b)))

  def or_Int(a: Value, b: Value): Value =
    ExternalValue(orInt(intRaw(a), intRaw(b)))

  def xor_Int(a: Value, b: Value): Value =
    ExternalValue(xorInt(intRaw(a), intRaw(b)))

  def not_Int(a: Value): Value =
    ExternalValue(notInt(intRaw(a)))

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
        case VList.VNil           => ()
        case VList.Cons(head, t1) =>
          bldr += head
          loop(t1)
        case other =>
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
    VInt(asArray(array).len)

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

  def get_or_Array(array: Value, index: Value, default: Value): Value =
    get_map_Array(array, index, default, FnValue.identity)

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
      case None =>
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
              NonEmptyList(
                left.asInstanceOf[Value],
                right.asInstanceOf[Value] :: Nil
              )
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
        case other =>
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
    intRaw(intValue) match {
      case i: java.lang.Integer => Value.Str(i.toString)
      case bi: BigInteger       => Value.Str(bi.toString)
    }

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

  private val maxUnicodeScalarValue = BigInteger.valueOf(0x10ffffL)
  private val highSurrogateStart = BigInteger.valueOf(0xd800L)
  private val highSurrogateEnd = BigInteger.valueOf(0xdfffL)

  def char_to_Int(item: Value): Value =
    item match {
      case Value.Str(s) =>
        Value.VInt(BigInteger.valueOf(s.codePointAt(0).toLong))
      case other =>
        // $COVERAGE-OFF$
        sys.error(s"type error: $other")
      // $COVERAGE-ON$
    }

  def int_to_Char(item: Value): Value = {
    val codePoint = i(item)
    val isInRange =
      codePoint.signum >= 0 && codePoint.compareTo(maxUnicodeScalarValue) <= 0
    val isSurrogate =
      codePoint.compareTo(highSurrogateStart) >= 0 &&
        codePoint.compareTo(highSurrogateEnd) <= 0

    if (!isInRange || isSurrogate) Value.VOption.none
    else {
      val cp = codePoint.intValue()
      Value.VOption.some(Value.Str(new String(Character.toChars(cp))))
    }
  }

  def char_List_to_String(chars: Value): Value =
    concat_String(chars)

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

  // return an Option[(String, String)] with a single-codepoint head and the remaining tail
  def uncons_String(arg: Value): Value = {
    val argS = arg.asExternal.toAny.asInstanceOf[String]
    if (argS.isEmpty) Value.VOption.none
    else {
      val nextOff = argS.offsetByCodePoints(0, 1)
      val head = argS.substring(0, nextOff)
      val tail = argS.substring(nextOff)
      Value.VOption.some(
        Value.Tuple(Value.ExternalValue(head), Value.ExternalValue(tail))
      )
    }
  }

  def tail_or_empty_String(arg: Value): Value = {
    val argS = arg.asExternal.toAny.asInstanceOf[String]
    if (argS.isEmpty) Value.Str("")
    else {
      val nextOff = argS.offsetByCodePoints(0, 1)
      Value.Str(argS.substring(nextOff))
    }
  }

  def char_Array_to_String(chars: Value): Value = {
    val arr = asArray(chars)
    val sb = new java.lang.StringBuilder
    var idx = 0
    while (idx < arr.len) {
      arr.data(arr.offset + idx) match {
        case Value.Str(s) => sb.append(s)
        case other        =>
          // $COVERAGE-OFF$
          sys.error(s"type error: $other")
        // $COVERAGE-ON$
      }
      idx = idx + 1
    }
    Value.Str(sb.toString)
  }
}
