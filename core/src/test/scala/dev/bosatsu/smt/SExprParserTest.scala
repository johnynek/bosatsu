package dev.bosatsu.smt

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class SExprParserTest extends munit.ScalaCheckSuite {
  import SExpr._

  private val atomChars: Vector[Char] =
    (('a' to 'z') ++
      ('A' to 'Z') ++
      ('0' to '9') ++
      Seq('_', '-', '+', '*', '/', '=', '<', '>', '?', '!', '~', '$', '%', '&', '^', ':', '.'))
      .toVector

  private val atomCharGen: Gen[Char] =
    Gen.oneOf(atomChars)

  private val bareAtomGen: Gen[String] =
    for {
      len <- Gen.choose(1, 16)
      chars <- Gen.listOfN(len, atomCharGen)
    } yield chars.mkString

  private def genExpr(depth: Int): Gen[SExpr] =
    if (depth <= 0) {
      bareAtomGen.map(Atom(_))
    } else {
      val atom = bareAtomGen.map(Atom(_))
      val list = for {
        size <- Gen.choose(0, 6)
        items <- Gen.listOfN(size, genExpr(depth - 1))
      } yield List(items.toVector)
      Gen.frequency(4 -> atom, 2 -> list)
    }

  private val exprVectorGen: Gen[Vector[SExpr]] =
    for {
      size <- Gen.choose(0, 8)
      items <- Gen.listOfN(size, genExpr(4))
    } yield items.toVector

  test("generated bare atoms are valid and parse directly") {
    forAll(bareAtomGen) { atom =>
      assert(SExpr.isValidBareAtom(atom))
      assertEquals(SExprParser.parseAll(atom), Right(Vector(Atom(atom))))
    }
  }

  test("single expression roundtrips through Doc/String/parser") {
    forAll(genExpr(5)) { expr =>
      val text = SExpr.toDoc(expr).render(120)
      assertEquals(SExprParser.parseAll(text), Right(Vector(expr)))
    }
  }

  test("top-level sequences roundtrip through Doc/String/parser") {
    forAll(exprVectorGen) { exprs =>
      val text = SExpr.toDocAll(exprs).render(120)
      assertEquals(SExprParser.parseAll(text), Right(exprs))
    }
  }

  test("whitespace and comments are ignored between forms") {
    val input =
      """; comment before all forms
        |(a ; inner comment
        | b)
        |; trailing comment
        |c
        |""".stripMargin
    assertEquals(
      SExprParser.parseAll(input),
      Right(
        Vector(
          List(Vector(Atom("a"), Atom("b"))),
          Atom("c")
        )
      )
    )
  }

  test("bar-quoted atoms support escaped pipes") {
    val expr = Atom("|a||b c|")
    val text = SExpr.toDoc(expr).render(120)
    assertEquals(text, "|a||b c|")
    assertEquals(SExprParser.parseAll(text), Right(Vector(expr)))
  }
}
