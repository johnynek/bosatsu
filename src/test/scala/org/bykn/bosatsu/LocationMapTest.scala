package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }

class LocationMapTest extends FunSuite {
  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50000)

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

      lm.toLineCol(offset) match {
        case None =>
          assert(offset < 0 || offset >= s.length)
        case Some((row, col)) =>
          lm.getLine(row) match {
            case None => fail
            case Some(line) => assert(line.length >= col)
          }
      }
    }
  }

  test("slow toLineCol matches") {

    def slow(str: String, offset: Int): Option[(Int, Int)] = {
      val split = str.split("\n", -1)
      def lineCol(off: Int, row: Int): Option[(Int, Int)] =
        if (row == split.length) None
        else if (off < 0) None
        else {
          val r = split(row)
          val extraNewLine = if (row != (split.length - 1)) 1 else 0 // all but the last have an extra newline
          val chars = r.length + extraNewLine
          if (off > chars) lineCol(off - chars, row + 1)
          else {
            Some((row, off))
          }
        }

      if (offset < 0 || offset >= str.length) None
      else lineCol(offset, 0)
    }

    forAll { (str: String, offset: Int) =>
      val lm = LocationMap(str)
      assert(lm.toLineCol(offset) === slow(str, offset))
    }
  }
}
