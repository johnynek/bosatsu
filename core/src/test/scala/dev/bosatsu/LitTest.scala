package dev.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.typelevel.paiges.Document

class LitTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 1000 else 100
    )

  val genLit: Gen[Lit] =
    Gen.oneOf(
      Gen.choose(-10000, 10000).map(Lit.fromInt),
      Arbitrary.arbitrary[String].map(Lit(_)),
      Gen
        .frequency(
          (10, Gen.choose(0, 0xd800)),
          (1, Gen.choose(0xe000, 1000000))
        )
        .map(Lit.fromCodePoint)
    )

  test("we can convert from Char to Lit") {
    forAll(Gen.choose(Char.MinValue, Char.MaxValue)) { (c: Char) =>
      try {
        val chr = Lit.fromChar(c)
        assertEquals(chr.asInstanceOf[Lit.Chr].asStr, c.toString)
      } catch {
        case _: IllegalArgumentException =>
          // there are at least 1million valid codepoints
          val cp = c.toInt
          assert((0xd800 <= cp && cp < 0xe000))
      }
    }
  }

  test("we can convert from Codepoint to Lit") {
    forAll(Gen.choose(-1000, 1500000)) { (cp: Int) =>
      try {
        val chr = Lit.fromCodePoint(cp)
        assertEquals(chr.asInstanceOf[Lit.Chr].toCodePoint, cp)
      } catch {
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
      assertEquals(
        Lit.parser.parseAll(Document[Lit].document(l).render(80)),
        Right(l)
      )
    }
  }
}
