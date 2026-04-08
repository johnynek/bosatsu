package dev.bosatsu

import java.math.BigInteger
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class BosatsuIntRuntimeLaws extends munit.ScalaCheckSuite {
  import TestUtils.runBosatsuTest

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 16 else 4)

  private val zero = BigInteger.ZERO
  private val one = BigInteger.ONE
  private val negOne = BigInteger.ONE.negate()

  private def pow2(bits: Int): BigInteger =
    BigInteger.ONE.shiftLeft(bits)

  private val boundaryInts: List[BigInteger] =
    List(
      zero,
      one,
      negOne,
      pow2(61).subtract(one),
      pow2(61),
      pow2(61).negate(),
      pow2(62).subtract(one),
      pow2(62).negate(),
      pow2(63),
      pow2(63).negate(),
      pow2(100).add(BigInteger.valueOf(305419896L)),
      pow2(100).add(BigInteger.valueOf(305419896L)).negate(),
      pow2(300).add(BigInteger.valueOf(123456789L)),
      pow2(300).add(BigInteger.valueOf(123456789L)).negate()
    )

  private val genBoundaryBigInt: Gen[BigInteger] =
    Gen.oneOf(boundaryInts)

  private val genWideBigInt: Gen[BigInteger] =
    for {
      sign <- Gen.oneOf(-1L, 1L)
      bits <- Gen.choose(64, 520)
      extra <- Gen.choose(0L, 1L << 20)
    } yield {
      val magnitude =
        BigInteger.ONE.shiftLeft(bits).add(BigInteger.valueOf(extra))
      if (sign < 0L) magnitude.negate() else magnitude
    }

  private val genBosatsuInt: Gen[BigInteger] =
    Gen.frequency(
      8 -> Arbitrary.arbitrary[Long].map(BigInteger.valueOf(_)),
      4 -> genBoundaryBigInt,
      3 -> genWideBigInt
    )

  private val genShiftCount: Gen[BigInteger] =
    Gen.frequency(
      8 -> Gen.choose(-200L, 200L).map(BigInteger.valueOf(_)),
      1 -> Gen.const(BigInteger.valueOf(64L)),
      1 -> Gen.const(BigInteger.valueOf(96L)),
      1 -> Gen.const(BigInteger.valueOf(300L)),
      1 -> Gen.const(BigInteger.valueOf(-64L)),
      1 -> Gen.const(BigInteger.valueOf(-96L)),
      1 -> Gen.const(BigInteger.valueOf(-300L))
    )

  private val genPowerOfTwoDivisor: Gen[BigInteger] =
    for {
      sign <- Gen.oneOf(-1L, 1L)
      bits <- Gen.oneOf(1, 5, 31, 32, 63, 64, 65, 96, 127, 255, 300)
    } yield {
      val magnitude = BigInteger.ONE.shiftLeft(bits)
      if (sign < 0L) magnitude.negate() else magnitude
    }

  private val genDivisor: Gen[BigInteger] =
    Gen.frequency(
      1 -> Gen.const(zero),
      1 -> Gen.const(one),
      1 -> Gen.const(negOne),
      4 -> genPowerOfTwoDivisor,
      8 -> genBosatsuInt
    )

  private def batchGen[A](gen: Gen[A], size: Int = 4): Gen[List[A]] =
    Gen.listOfN(size, gen)

  private def bosatsuInt(value: BigInteger): String =
    value.toString

  private def bosatsuString(value: String): String =
    "\"" + value + "\""

  private def bosatsuCmp(value: Int): String =
    if (value < 0) "LT"
    else if (value > 0) "GT"
    else "EQ"

  private def assertion(expr: String, label: String): String =
    s"""Assertion($expr, "$label")"""

  private def runBosatsuAssertions(
      suiteName: String,
      assertions: List[String]
  ): Unit = {
    val src =
      s"""package Bosatsu/Int/RuntimeProps
         |
         |tests = TestSuite("$suiteName", [
         |  ${assertions.mkString(",\n  ")}
         |])
         |""".stripMargin

    runBosatsuTest(
      List(src),
      "Bosatsu/Int/RuntimeProps",
      assertions.length
    )
  }

  property("Bosatsu add/sub/mul/cmp agree with JVM Int semantics") {
    forAll(batchGen(Gen.zip(genBosatsuInt, genBosatsuInt))) { pairs =>
      val assertions =
        pairs.zipWithIndex.flatMap { case ((left, right), idx) =>
          List(
            assertion(
              s"add(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(left.add(right))}",
              s"add_$idx"
            ),
            assertion(
              s"sub(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(left.subtract(right))}",
              s"sub_$idx"
            ),
            assertion(
              s"mul(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(left.multiply(right))}",
              s"mul_$idx"
            ),
            assertion(
              s"cmp_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuCmp(left.compareTo(right))}",
              s"cmp_$idx"
            )
          )
        }

      runBosatsuAssertions("bosatsu_int_arith_props", assertions)
    }
  }

  property("Bosatsu bitwise Int externals agree with JVM BigInteger semantics") {
    forAll(batchGen(Gen.zip(genBosatsuInt, genBosatsuInt))) { pairs =>
      val assertions =
        pairs.zipWithIndex.flatMap { case ((left, right), idx) =>
          List(
            assertion(
              s"and_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(left.and(right))}",
              s"and_$idx"
            ),
            assertion(
              s"or_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(left.or(right))}",
              s"or_$idx"
            ),
            assertion(
              s"xor_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(left.xor(right))}",
              s"xor_$idx"
            ),
            assertion(
              s"not_Int(${bosatsuInt(left)}) matches ${bosatsuInt(left.not())}",
              s"not_$idx"
            )
          )
        }

      runBosatsuAssertions("bosatsu_int_bitwise_props", assertions)
    }
  }

  property("Bosatsu shift Int externals agree with JVM Int semantics") {
    forAll(batchGen(Gen.zip(genBosatsuInt, genShiftCount))) { pairs =>
      val assertions =
        pairs.zipWithIndex.flatMap { case ((value, shift), idx) =>
          val expectedLeft = PredefImpl.shiftLeft(value, shift)
          val expectedRight = PredefImpl.shiftRight(value, shift)
          List(
            assertion(
              s"shift_left_Int(${bosatsuInt(value)}, ${bosatsuInt(shift)}) matches ${bosatsuInt(expectedLeft)}",
              s"shift_left_$idx"
            ),
            assertion(
              s"shift_right_Int(${bosatsuInt(value)}, ${bosatsuInt(shift)}) matches ${bosatsuInt(expectedRight)}",
              s"shift_right_$idx"
            )
          )
        }

      runBosatsuAssertions("bosatsu_int_shift_props", assertions)
    }
  }

  property("Bosatsu div/mod/gcd agree with JVM Int semantics") {
    forAll(batchGen(Gen.zip(genBosatsuInt, genDivisor))) { pairs =>
      val assertions =
        pairs.zipWithIndex.flatMap { case ((left, right), idx) =>
          val expectedDiv = PredefImpl.divBigInteger(left, right)
          val expectedMod = PredefImpl.modBigInteger(left, right)
          val expectedGcd = PredefImpl.gcdBigInteger(left, right)
          List(
            assertion(
              s"div(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(expectedDiv)}",
              s"div_$idx"
            ),
            assertion(
              s"mod_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(expectedMod)}",
              s"mod_$idx"
            ),
            assertion(
              s"gcd_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(expectedGcd)}",
              s"gcd_$idx"
            )
          )
        }

      runBosatsuAssertions("bosatsu_int_divmod_props", assertions)
    }
  }

  property("Bosatsu power-of-two div/mod/gcd agree with JVM Int semantics") {
    forAll(batchGen(Gen.zip(genBosatsuInt, genPowerOfTwoDivisor))) { pairs =>
      val assertions =
        pairs.zipWithIndex.flatMap { case ((left, right), idx) =>
          val expectedDiv = PredefImpl.divBigInteger(left, right)
          val expectedMod = PredefImpl.modBigInteger(left, right)
          val expectedGcd = PredefImpl.gcdBigInteger(left, right)
          List(
            assertion(
              s"div(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(expectedDiv)}",
              s"pow2_div_$idx"
            ),
            assertion(
              s"mod_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(expectedMod)}",
              s"pow2_mod_$idx"
            ),
            assertion(
              s"gcd_Int(${bosatsuInt(left)}, ${bosatsuInt(right)}) matches ${bosatsuInt(expectedGcd)}",
              s"pow2_gcd_$idx"
            )
          )
        }

      runBosatsuAssertions("bosatsu_int_divmod_pow2_props", assertions)
    }
  }

  property("Bosatsu string/int conversions agree with JVM Int semantics") {
    forAll(batchGen(genBosatsuInt)) { values =>
      val assertions =
        values.zipWithIndex.flatMap { case (value, idx) =>
          val rendered = value.toString
          List(
            assertion(
              s"int_to_String(${bosatsuInt(value)}) matches ${bosatsuString(rendered)}",
              s"to_string_$idx"
            ),
            assertion(
              s"string_to_Int(${bosatsuString(rendered)}) matches Some(${bosatsuInt(value)})",
              s"from_string_$idx"
            )
          )
        }

      runBosatsuAssertions("bosatsu_int_string_props", assertions)
    }
  }
}
