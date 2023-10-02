package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalacheck.Arbitrary
import org.typelevel.paiges.Document

class LitTest extends AnyFunSuite {
  def config: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = if (Platform.isScalaJvm) 1000 else 100)

  val genLit: Gen[Lit] =
    Gen.oneOf(
      Gen.choose(-10000, 10000).map(Lit.fromInt),
      Arbitrary.arbitrary[String].map(Lit(_)),
      Gen.frequency(
        (10, Gen.choose(0, 0xd800)),
        (1, Gen.choose(0xe000, 1000000))).map(Lit.fromCodePoint)
    )

  test("we can convert from Char to Lit") {
    forAll { (c: Char) =>
      if (0xd800 <= c && c < 0xe000) {
        val chr = Lit.fromChar(c)
        assert(chr.asInstanceOf[Lit.Chr].asStr == c.toString)
      }
    }
  }

  test("we can convert from Codepoint to Lit") {
    forAll(Gen.choose(-1000, 1500000)) { (cp: Int) =>
      try {
        val chr = Lit.fromCodePoint(cp)
        assert(chr.asInstanceOf[Lit.Chr].toCodePoint == cp)
      }
      catch {
        case _: IllegalArgumentException =>
          // there are at least 1million valid codepoints
          assert(cp < 0 || (0xd800 <= cp && cp < 0xe000) || (cp > 1000000))
      }
    }
  }

  test("Lit ordering is correct") {
    forAll(genLit, genLit, genLit) { (a, b, c) =>
      OrderingLaws.law(a, b, c)  
    }
  }

  test("we can parse from document") {
    forAll(genLit) { l =>
      assert(Lit.parser.parseAll(Document[Lit].document(l).render(80)) == Right(l))
    }
  }
}