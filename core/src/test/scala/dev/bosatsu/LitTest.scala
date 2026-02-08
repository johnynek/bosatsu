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

  val genFloatNoNaN: Gen[Lit.Float64] =
    Gen
      .oneOf(
        Arbitrary.arbitrary[Long].map(Lit.Float64.fromRawLongBits),
        Gen.const(Lit.Float64.fromDouble(java.lang.Double.POSITIVE_INFINITY)),
        Gen.const(Lit.Float64.fromDouble(java.lang.Double.NEGATIVE_INFINITY)),
        Gen.const(Lit.Float64.fromDouble(-0.0d)),
        Gen.const(Lit.Float64.fromDouble(0.0d))
      )
      .suchThat(f => !java.lang.Double.isNaN(f.toDouble))

  val genLit: Gen[Lit] =
    Gen.oneOf(
      Gen.choose(-10000, 10000).map(Lit.fromInt),
      Arbitrary.arbitrary[String].map(Lit(_)),
      genFloatNoNaN,
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

  test("Float64 ordering treats all NaNs as equal and sorts NaN first") {
    val genNaN = Arbitrary.arbitrary[Long].map { seed =>
      val frac = (seed & 0x000fffffffffffffL) | 0x1L
      val bits = 0x7ff0000000000000L | frac
      Lit.Float64.fromRawLongBits(bits)
    }

    forAll(genNaN, genNaN) { (a, b) =>
      val cmp = Ordering[Lit].compare(a, b)
      assertEquals(cmp, 0)
      assert(Ordering[Lit].compare(a, Lit.Float64.fromDouble(0.0)) < 0)
    }

    assertEquals(
      Ordering[Lit].compare(
        Lit.Float64.fromDouble(-0.0),
        Lit.Float64.fromDouble(0.0)
      ),
      0
    )
  }

  test(".NaN renders and parses") {
    val nan = Lit.Float64.fromDouble(java.lang.Double.NaN)
    assertEquals(Document[Lit].document(nan).render(80), ".NaN")
    assertEquals(Lit.float64Parser.parseAll(".NaN"), Right(nan))
  }
}
