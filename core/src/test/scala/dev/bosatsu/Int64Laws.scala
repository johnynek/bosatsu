package dev.bosatsu

import java.math.BigInteger
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class Int64Laws extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 5000 else 500)

  private val LongMinBI = BigInteger.valueOf(Long.MinValue)
  private val LongMaxBI = BigInteger.valueOf(Long.MaxValue)
  private val Int64UpperExclusiveDouble = java.lang.Math.scalb(1.0d, 63)
  private val Int64LowerInclusiveDouble = -Int64UpperExclusiveDouble

  private val genBoundaryBigInt: Gen[BigInteger] =
    Gen.choose(-4L, 4L).flatMap { offset =>
      Gen.oneOf(
        LongMinBI.add(BigInteger.valueOf(offset)),
        LongMaxBI.add(BigInteger.valueOf(offset))
      )
    }

  private val genWideBigInt: Gen[BigInteger] =
    for {
      sign <- Gen.oneOf(-1L, 1L)
      bits <- Gen.choose(64, 256)
      extra <- Gen.choose(0L, 2048L)
    } yield {
      val magnitude =
        BigInteger.ONE
          .shiftLeft(bits)
          .add(BigInteger.valueOf(extra))
      if (sign < 0L) magnitude.negate()
      else magnitude
    }

  private val genBigInt: Gen[BigInteger] =
    Gen.frequency(
      8 -> Arbitrary.arbitrary[Long].map(BigInteger.valueOf(_)),
      2 -> genBoundaryBigInt,
      1 -> genWideBigInt
    )

  private val genShiftCount: Gen[BigInteger] =
    Gen.frequency(
      8 -> Gen.choose(-200L, 200L).map(BigInteger.valueOf(_)),
      1 -> Gen.const(BigInteger.valueOf(64L)),
      1 -> Gen.const(BigInteger.valueOf(-64L)),
      1 -> Gen.const(BigInteger.valueOf(1000L)),
      1 -> Gen.const(BigInteger.valueOf(-1000L))
    )

  private def int64Value(value: Value): Long =
    value match {
      case Value.ExternalValue(v: java.lang.Long) => v.longValue
      case other => fail(s"expected Int64 value, found: $other")
    }

  private def int64OptionValue(value: Value): Option[Long] =
    value match {
      case Value.VOption(Some(v)) => Some(int64Value(v))
      case Value.VOption(None)    => None
      case other => fail(s"expected Option[Int64], found: $other")
    }

  private def valueInt64(v: Long): Value =
    Value.ExternalValue(PredefImpl.Int64Value(v))

  private def valueInt(v: BigInteger): Value =
    Value.VInt(v)

  // Keep this model aligned with the documented Bosatsu contract:
  // IEEE-754 ties-to-even rounding via Math.rint, then clamp to [-2^63, 2^63).
  private def modelFloat64ToInt64(value: Double): Option[Long] =
    if (!java.lang.Double.isFinite(value)) None
    else {
      val rounded = java.lang.Math.rint(value)
      if (
        (rounded < Int64LowerInclusiveDouble) || (rounded >= Int64UpperExclusiveDouble)
      )
        None
      else Some(rounded.toLong)
    }

  private def assertBinaryLaw(
      left: Long,
      right: Long,
      fn: (Value, Value) => Value,
      model: (BigInteger, BigInteger) => BigInteger
  ): Unit = {
    val expected =
      model(BigInteger.valueOf(left), BigInteger.valueOf(right)).longValue()
    val actual = int64Value(fn(valueInt64(left), valueInt64(right)))
    assertEquals(actual, expected)
  }

  test("int_to_Int64 succeeds exactly on the signed 64-bit range") {
    forAll(genBigInt) { value =>
      val actual = int64OptionValue(PredefImpl.int_to_Int64(valueInt(value)))
      val expected =
        if (
          (value.compareTo(LongMinBI) >= 0) && (value.compareTo(LongMaxBI) <= 0)
        )
          Some(value.longValue())
        else None
      assertEquals(actual, expected)
    }
  }

  test("int_low_bits_to_Int64 matches low-bit truncation and is idempotent") {
    forAll(genBigInt, Arbitrary.arbitrary[Long]) { (value, seed) =>
      val truncated =
        int64Value(PredefImpl.int_low_bits_to_Int64(valueInt(value)))
      assertEquals(truncated, value.longValue())

      val roundTripped =
        PredefImpl.int_low_bits_to_Int64(
          PredefImpl.int64_to_Int(valueInt64(seed))
        )
      assertEquals(int64Value(roundTripped), seed)
    }
  }

  test("add_Int64 matches Int addition followed by truncation") {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        assertBinaryLaw(left, right, PredefImpl.add_Int64, (a, b) => a.add(b))
    }
  }

  test("sub_Int64 matches Int subtraction followed by truncation") {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        assertBinaryLaw(
          left,
          right,
          PredefImpl.sub_Int64,
          (a, b) => a.subtract(b)
        )
    }
  }

  test("mul_Int64 matches Int multiplication followed by truncation") {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        assertBinaryLaw(
          left,
          right,
          PredefImpl.mul_Int64,
          (a, b) => a.multiply(b)
        )
    }
  }

  test("div_Int64 matches Int division followed by truncation") {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        assertBinaryLaw(
          left,
          right,
          PredefImpl.div_Int64,
          PredefImpl.divBigInteger
        )
    }
  }

  test("div_Int64 and mod_Int64 satisfy the Int64 divmod law") {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        val quot =
          int64Value(PredefImpl.div_Int64(valueInt64(left), valueInt64(right)))
        val rem =
          int64Value(PredefImpl.mod_Int64(valueInt64(left), valueInt64(right)))
        val recomposed =
          int64Value(
            PredefImpl.add_Int64(
              PredefImpl.mul_Int64(valueInt64(quot), valueInt64(right)),
              valueInt64(rem)
            )
          )
        assertEquals(recomposed, left)
    }
  }

  test(
    "bitwise Int64 functions match Int bitwise operations followed by truncation"
  ) {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        assertEquals(
          int64Value(PredefImpl.and_Int64(valueInt64(left), valueInt64(right))),
          left & right
        )
        assertEquals(
          int64Value(PredefImpl.or_Int64(valueInt64(left), valueInt64(right))),
          left | right
        )
        assertEquals(
          int64Value(PredefImpl.xor_Int64(valueInt64(left), valueInt64(right))),
          left ^ right
        )
    }
  }

  test("not_Int64 matches Int bitwise not followed by truncation") {
    forAll(Arbitrary.arbitrary[Long]) { value =>
      val expected = BigInteger.valueOf(value).not().longValue()
      val actual = int64Value(PredefImpl.not_Int64(valueInt64(value)))
      assertEquals(actual, expected)
    }
  }

  test("Int64 shifts match the Int shift model followed by truncation") {
    forAll(Arbitrary.arbitrary[Long], genShiftCount) { (value, shift) =>
      val leftExpected =
        PredefImpl.shiftLeft(BigInteger.valueOf(value), shift).longValue()
      val rightExpected =
        PredefImpl.shiftRight(BigInteger.valueOf(value), shift).longValue()

      val leftActual =
        int64Value(
          PredefImpl.shift_left_Int64(valueInt64(value), valueInt(shift))
        )
      val rightActual =
        int64Value(
          PredefImpl.shift_right_Int64(valueInt64(value), valueInt(shift))
        )

      assertEquals(leftActual, leftExpected)
      assertEquals(rightActual, rightExpected)
    }
  }

  test("Int64 unsigned right shifts match the raw-bit logical shift model") {
    forAll(Arbitrary.arbitrary[Long], genShiftCount) { (value, shift) =>
      val expected =
        if (shift.signum == 0) value
        else if (shift.signum > 0) {
          if (shift.compareTo(BigInteger.valueOf(64L)) >= 0) 0L
          else value >>> shift.intValue()
        } else {
          val absShift = shift.negate()
          if (absShift.compareTo(BigInteger.valueOf(64L)) >= 0) 0L
          else value << absShift.intValue()
        }

      val actual =
        int64Value(
          PredefImpl.shift_right_unsigned_Int64(
            valueInt64(value),
            valueInt(shift)
          )
        )

      assertEquals(actual, expected)
    }
  }

  test("popcount_Int64 counts set bits in the raw 64-bit payload") {
    forAll(Arbitrary.arbitrary[Long]) { value =>
      assertEquals(
        PredefImpl.popcount_Int64(valueInt64(value)),
        Value.VInt(BigInteger.valueOf(java.lang.Long.bitCount(value).toLong))
      )
    }
  }

  test("eq_Int64 matches signed Long equality") {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        assertEquals(
          PredefImpl.eq_Int64(valueInt64(left), valueInt64(right)),
          if (left == right) Value.True else Value.False
        )
    }
  }

  test("cmp_Int64 matches signed Long ordering") {
    forAll(Arbitrary.arbitrary[Long], Arbitrary.arbitrary[Long]) {
      (left, right) =>
        assertEquals(
          PredefImpl.cmp_Int64(valueInt64(left), valueInt64(right)),
          Value.Comparison.fromInt(java.lang.Long.compare(left, right))
        )
    }
  }

  test(
    "float64_to_Int64 matches the documented rounding and signed-range contract"
  ) {
    forAll(Arbitrary.arbitrary[Long]) { bits =>
      val input = java.lang.Double.longBitsToDouble(bits)
      val floatValue = Value.VFloat(input)
      val actual = int64OptionValue(PredefImpl.float64_to_Int64(floatValue))
      val expected = modelFloat64ToInt64(input)
      assertEquals(actual, expected)
    }
  }

  test("float64_to_Int64 rejects 2^63 and accepts -2^63") {
    assertEquals(
      int64OptionValue(
        PredefImpl.float64_to_Int64(Value.VFloat(Int64UpperExclusiveDouble))
      ),
      None
    )
    assertEquals(
      int64OptionValue(
        PredefImpl.float64_to_Int64(Value.VFloat(Int64LowerInclusiveDouble))
      ),
      Some(Long.MinValue)
    )
  }

  test("int64_to_Float64 agrees with int_to_Float64(int64_to_Int(x))") {
    forAll(Arbitrary.arbitrary[Long]) { value =>
      val input = valueInt64(value)
      assertEquals(
        PredefImpl.int64_to_Float64(input),
        PredefImpl.int_to_Float64(PredefImpl.int64_to_Int(input))
      )
    }
  }
}
