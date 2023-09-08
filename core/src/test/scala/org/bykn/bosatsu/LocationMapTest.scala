package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite

class LocationMapTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful =
      if (Platform.isScalaJvm) 50000 else 100
    )

  test("single line locations") {
    val singleLine: Gen[String] =
      Arbitrary.arbitrary[String].map(_.filterNot(_ == '\n'))

    forAll(singleLine, Arbitrary.arbitrary[Int]) { (sline, offset) =>
      val lm = LocationMap(sline)

      assert(lm.getLine(0) == Some(sline))
      lm.toLineCol(offset) match {
        case None =>
          assert(offset < 0 || offset >= sline.length)
        case Some((row, col)) =>
          assert(row == 0)
          assert(col == offset)
      }
    }
  }

  test("we can reassemble input with getLine") {
    forAll { (str: String) =>
      val lm = LocationMap(str)

      val reconstruct = Iterator
        .iterate(0)(_ + 1)
        .map(lm.getLine _)
        .takeWhile(_.isDefined)
        .collect { case Some(l) => l }
        .mkString("\n")

      assert(reconstruct === str)
    }
  }
  test(
    "toLineCol is defined for all valid offsets, and getLine isDefined consistently"
  ) {

    forAll { (s: String, offset: Int) =>
      val lm = LocationMap(s)

      def test(offset: Int) =
        lm.toLineCol(offset) match {
          case None =>
            assert(offset < 0 || offset > s.length)
          case Some((row, col)) =>
            lm.getLine(row) match {
              case None => assert(offset == s.length)
              case Some(line) =>
                assert(line.length >= col)
                if (line.length == col)
                  assert(offset == s.length || s(offset) == '\n')
                else assert(line(col) == s(offset))
            }
        }

      test(offset)
      if (s.nonEmpty) test(math.abs(offset % s.length))
    }
  }

  test("if a string is not empty, 0 offset is (0, 0)") {
    forAll { (s: String) =>
      LocationMap(s).toLineCol(0) match {
        case Some(r) => assert(r == ((0, 0)))
        case None    => assert(s.isEmpty)
      }
    }
  }
}
