package dev.bosatsu

import Value.*
import java.math.BigInteger
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class ArrayInt64Laws extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 2000 else 200)

  private def int64Value(value: Value): Long =
    value match {
      case ExternalValue(v: java.lang.Long) => v.longValue
      case other => fail(s"expected Int64 value, found: $other")
    }

  private def intValue(value: Value): Int =
    value match {
      case VInt(v) => v.intValueExact()
      case other   => fail(s"expected Int value, found: $other")
    }

  private def floatValue(value: Value): Double =
    value match {
      case VFloat(v) => v
      case other     => fail(s"expected Float64 value, found: $other")
    }

  private def intArray(values: Vector[Int]): Value =
    if (values.isEmpty) PredefImpl.emptyArray
    else
      ExternalValue(
        PredefImpl.ArrayValue(
          values.iterator.map(v => VInt(BigInteger.valueOf(v.toLong))).toArray,
          0,
          values.length
        )
      )

  private def int64(value: Int): Value =
    ExternalValue(java.lang.Long.valueOf(value.toLong))

  private def floatArray(values: Vector[Double]): Value =
    if (values.isEmpty) PredefImpl.emptyArray
    else
      ExternalValue(
        PredefImpl
          .ArrayValue(values.iterator.map(VFloat(_)).toArray, 0, values.length)
      )

  private def readIntArray(value: Value): Vector[Int] =
    value match {
      case ExternalValue(arr: PredefImpl.ArrayValue) =>
        arr.data
          .slice(arr.offset, arr.offset + arr.len)
          .iterator
          .map(intValue)
          .toVector
      case other =>
        fail(s"expected array value, found: $other")
    }

  private val genSliceCase: Gen[(Vector[Int], Int, Int)] =
    for {
      values <- Gen.listOf(Gen.choose(-50, 50)).map(_.toVector)
      start <- Gen.choose(0, values.length)
      end <- Gen.choose(start, values.length)
    } yield (values, start, end)

  private val genZipCase: Gen[(Vector[Int], Vector[Int])] =
    for {
      left <- Gen.listOf(Gen.choose(-50, 50)).map(_.toVector)
      right <- Gen.listOf(Gen.choose(-50, 50)).map(_.toVector)
    } yield (left, right)

  private val genFloatSliceCase: Gen[(Vector[Double], Int, Int)] =
    for {
      values <- Gen.listOf(Gen.choose(-25, 25).map(_.toDouble)).map(_.toVector)
      start <- Gen.choose(0, values.length)
      end <- Gen.choose(start, values.length)
    } yield (values, start, end)

  test("map_with_index_Array uses visible slice indices") {
    forAll(genSliceCase) { case (values, start, end) =>
      val sliced =
        PredefImpl.slice_Array(intArray(values), int64(start), int64(end))
      val mapped =
        PredefImpl.map_with_index_Array(
          sliced,
          FnValue { args =>
            val item = intValue(args.head)
            val idx = int64Value(args.tail.head)
            VInt(BigInteger.valueOf(item.toLong + idx))
          }
        )

      val expected =
        values
          .slice(start, end)
          .zipWithIndex
          .map { case (item, idx) => item + idx }

      assertEquals(readIntArray(mapped), expected)
    }
  }

  test("zip helpers stop at the shorter visible length") {
    forAll(genZipCase) { case (left, right) =>
      val mapped =
        PredefImpl.zip_map_Array(
          intArray(left),
          intArray(right),
          FnValue { args =>
            val l = intValue(args.head)
            val r = intValue(args.tail.head)
            VInt(BigInteger.valueOf((l - r).toLong))
          }
        )

      val folded =
        intValue(
          PredefImpl.zip_foldl_Array(
            intArray(left),
            intArray(right),
            VInt(0),
            FnValue { args =>
              val acc = intValue(args.head)
              val l = intValue(args.tail.head)
              val r = intValue(args.tail.tail.head)
              VInt(BigInteger.valueOf((acc + l - r).toLong))
            }
          )
        )

      val expectedPrefix = left.zip(right).map { case (l, r) => l - r }
      assertEquals(readIntArray(mapped), expectedPrefix.toVector)
      assertEquals(folded, expectedPrefix.sum)
    }
  }

  test("Float64 Array reductions follow visible left-to-right order") {
    forAll(genFloatSliceCase, genFloatSliceCase) {
      case (
            (leftValues, leftStart, leftEnd),
            (rightValues, rightStart, rightEnd)
          ) =>
        val leftSlice = leftValues.slice(leftStart, leftEnd)
        val rightSlice = rightValues.slice(rightStart, rightEnd)
        val left =
          PredefImpl.slice_Array(
            floatArray(leftValues),
            int64(leftStart),
            int64(leftEnd)
          )
        val right =
          PredefImpl.slice_Array(
            floatArray(rightValues),
            int64(rightStart),
            int64(rightEnd)
          )

        val sumExpected = leftSlice.foldLeft(0.0)(_ + _)
        val sumsqExpected = leftSlice.foldLeft(0.0) { (acc, item) =>
          acc + (item * item)
        }
        val dotExpected =
          leftSlice.zip(rightSlice).foldLeft(0.0) { case (acc, (l, r)) =>
            acc + (l * r)
          }
        val zipSumExpected = dotExpected

        assertEquals(floatValue(PredefImpl.sumf_Array(left)), sumExpected)
        assertEquals(floatValue(PredefImpl.sumsqf_Array(left)), sumsqExpected)
        assertEquals(
          floatValue(PredefImpl.dotf_Array(left, right)),
          dotExpected
        )
        assertEquals(
          floatValue(
            PredefImpl.zip_sumf_Array(
              left,
              right,
              FnValue { args =>
                val l = floatValue(args.head)
                val r = floatValue(args.tail.head)
                VFloat(l * r)
              }
            )
          ),
          zipSumExpected
        )
    }
  }
}
