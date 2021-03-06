package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.funsuite.AnyFunSuite

class LocationMapTest extends AnyFunSuite {
  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = if (Platform.isScalaJvm) 50000 else 100)

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

  test("some specific examples") {
    val lm0 = LocationMap("\n")
    assert(lm0.toLineCol(0) == Some((0, 0)))
    assert(lm0.toLineCol(1) == None)

    val lm1 = LocationMap("012\n345\n678")
    assert(lm1.toLineCol(-1) == None)
    assert(lm1.toLineCol(0) == Some((0, 0)))
    assert(lm1.toLineCol(1) == Some((0, 1)))
    assert(lm1.toLineCol(2) == Some((0, 2)))
    assert(lm1.toLineCol(3) == Some((0, 3)))
    assert(lm1.toLineCol(4) == Some((1, 0)))
    assert(lm1.toLineCol(5) == Some((1, 1)))
    assert(lm1.toLineCol(6) == Some((1, 2)))
    assert(lm1.toLineCol(7) == Some((1, 3)))
    assert(lm1.toLineCol(8) == Some((2, 0)))
    assert(lm1.toLineCol(9) == Some((2, 1)))
    assert(lm1.toLineCol(10) == Some((2, 2)))
    assert(lm1.toLineCol(11) == None)
  }

  test("we can reassemble input with getLine") {
    forAll { str: String =>
      val lm = LocationMap(str)

      val reconstruct = Iterator.iterate(0)(_ + 1)
        .map(lm.getLine _)
        .takeWhile(_.isDefined)
        .collect { case Some(l) => l }
        .mkString("\n")

      assert(reconstruct === str)
    }
  }
  test("toLineCol is defined for all valid offsets, and getLine isDefined consistently") {

    forAll { (s: String, offset: Int) =>
      val lm = LocationMap(s)

      def test(offset: Int) =
        lm.toLineCol(offset) match {
          case None =>
            assert(offset < 0 || offset >= s.length)
          case Some((row, col)) =>
            lm.getLine(row) match {
              case None => fail
              case Some(line) =>
                assert(line.length >= col)
                if (line.length == col) assert(s(offset) == '\n')
                else assert(line(col) == s(offset))
            }
        }

      test(offset)
      if (s.nonEmpty) test(math.abs(offset % s.length))
    }
  }

  test("if a string is not empty, 0 offset is (0, 0)") {
    forAll { s: String =>
      LocationMap(s).toLineCol(0) match {
        case Some(r) => assert(r == ((0, 0)))
        case None => assert(s.isEmpty)
      }
    }
  }

  test("slow toLineCol matches") {

    def slow(str: String, offset: Int): Option[(Int, Int)] = {
      val split = str.split("\n", -1)
      def lineCol(off: Int, row: Int): Option[(Int, Int)] =
        if (row == split.length) None
        else {
          val r = split(row)
          val extraNewLine = if (row < (split.length - 1)) 1 else 0 // all but the last have an extra newline
          val chars = r.length + extraNewLine

          if (off >= chars) lineCol(off - chars, row + 1)
          else Some((row, off))
        }

      if (offset < 0 || offset >= str.length) None
      else lineCol(offset, 0)
    }

    assert(slow("\n", 0) == Some((0, 0)))
    assert(LocationMap("\n").toLineCol(0) == Some((0, 0)))

    assert(slow(" \n", 1) == Some((0, 1)))
    assert(LocationMap(" \n").toLineCol(1) == Some((0, 1)))

    assert(slow(" \n ", 1) == Some((0, 1)))
    assert(LocationMap(" \n ").toLineCol(1) == Some((0, 1)))

    assert(slow("\n ", 1) == Some((1, 0)))
    assert(LocationMap("\n ").toLineCol(1) == Some((1, 0)))

    forAll { (str: String, offset: Int) =>
      val lm = LocationMap(str)
      assert(lm.toLineCol(offset) === slow(str, offset))
      if (str.length > 0) {
        val validOffset = math.abs(offset % str.length)
        assert(lm.toLineCol(validOffset) === slow(str, validOffset))
      }
    }
  }
}
