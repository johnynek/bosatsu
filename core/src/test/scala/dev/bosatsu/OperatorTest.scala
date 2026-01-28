package dev.bosatsu

import org.typelevel.paiges.{Doc, Document}

import cats.parse.{Parser => P}

import Parser.Combinators

class OperatorTest extends ParserTestBase {

  import Operators.Formula
  import TestUtils.runBosatsuTest

  sealed abstract class F {
    def toFormula: Formula[String] =
      this match {
        case F.Num(s)                            => Formula.Sym(s)
        case F.Form(Formula.Sym(n))              => n.toFormula
        case F.Form(Formula.Op(left, op, right)) =>
          Formula.Op(F.Form(left).toFormula, op, F.Form(right).toFormula)
      }
  }
  object F {
    case class Num(str: String) extends F
    case class Form(toForm: Formula[F]) extends F
  }

  lazy val formP: P[F] =
    Operators.Formula
      .parser(
        Parser.integerString
          .map(F.Num(_))
          .orElse(P.defer(formP.parensCut))
      )
      .map(F.Form(_))

  implicit val document: Document[Formula[String]] =
    Document.instance[Formula[String]] {
      case Formula.Sym(n)      => Doc.text(n)
      case Formula.Op(l, o, r) =>
        document.document(l) + Doc.text(o) + document.document(r)
    }

  def parseSame(left: String, right: String) =
    assertEquals(Parser.unsafeParse(formP, left).toFormula, Parser
        .unsafeParse(formP, right)
        .toFormula)

  test("we can parse integer formulas") {
    parseSame("1+2", "1 + 2")
    parseSame("1<2", "1 < 2")
    parseSame("1+(2*3)", "1 + 2*3")
    parseSame("1+2+3", "(1 + 2) + 3")
    parseSame("1+2+3+4", "((1 + 2) + 3) + 4")
    parseSame("1*2+3*4", "(1 * 2) + (3 * 4)")
    parseSame("1&2|3&4", "(1 & 2) | (3 & 4)")
    parseSame("1&2^3&4", "(1 & 2) ^ (3 & 4)")
    parseSame("1 < 2 & 3 < 4", "(1 < 2) & (3 < 4)")
    parseSame("1 <= 2 & 3 <= 4", "(1 <= 2) & (3 <= 4)")
    parseSame("1 <= 2 < 3", "(1 <= 2) < 3")
    parseSame("0 < 1 <= 2", "0 < (1 <= 2)")
    parseSame("1 ** 2 * 3", "(1 ** 2) * 3")
    parseSame("3 * 1 ** 2", "3 * (1 ** 2)")
    parseSame("1 + 2 == 2 + 1", "(1 + 2) == (2 + 1)")
    parseSame("1 + 2 - 3", "1 + (2 - 3)")
    parseSame("1 - 2 + 3", "(1 - 2) + 3")
    parseSame("1 + 2 * 3 == 1 + (2 * 3)", "(1 + (2*3)) == (1 + (2 * 3))")
    parseSame("1 +. 2 * 3 == 1 +. (2 * 3)", "(1 +. (2*3)) == (1 +. (2 * 3))")
    parseSame("1 .+ 2 * 3 == (1 .+ 2) * 3", "((1 .+ 2) *3) == ((1 .+ 2) * 3)")
  }

  test("test operator precedence in real programs") {
    runBosatsuTest(
      List("""
package Test

operator + = add
operator * = times
operator == = eq_Int
def operator %(a, b): mod_Int(a, b)

test = TestSuite("precedence",
  [
    Assertion(1 + 2 * 3 == 1 + (2 * 3), "p1"),
    Assertion(1 * 2 * 3 == (1 * 2) * 3, "p1"),
    Assertion(1 + 2 % 3 == 1 + (2 % 3), "p1")
  ])
"""),
      "Test",
      3
    )

    runBosatsuTest(
      List("""
package Test

# this is non-associative so we can test order
operator *> = (x, y) -> (x, y)

operator == = (x, y) ->
  # kind of an interesting style to make local operators
  `=*=` = eq_Int
  `&` = (x, y) -> y if x else False
  (((a, b), c), ((d, e), f)) = (x, y)
  (a =*= d) & (b =*= e) & (c =*= f)

test = TestSuite("precedence",
  [
    Assertion(1 *> 2 *> 3 == (1 *> 2) *> 3, "p1"),
  ])
"""),
      "Test",
      1
    )

    runBosatsuTest(
      List(
        """
package T1

export operator +, operator *, operator ==

operator + = add
operator * = times
operator == = eq_Int
""",
        """
package T2

from T1 import operator + as operator ++, `*`, `==`

`.+` = `++`
`+.` = `++`

test = TestSuite("import export",
  [ Assertion(1 +. (2 * 3) == 1 .+ (2 * 3), "p1"),
    Assertion(1 .+ 2 * 3 == (1 .+ 2) * 3, "p1") ])
"""
      ),
      "T2",
      2
    )
  }

  test("test ternary operator precedence") {
    runBosatsuTest(
      List("""
package Test

operator == = eq_Int
operator + = add

left1 = 1 + 2 if False else 4
# should be 4 not 1 + 4
right1 = 4

left2 = 1 if True else 2 + 3
# above should be 1 not (1 + 3)
right2 = 1

test = TestSuite("precedence",
  [
    Assertion(left1 == right1, "p1"),
    Assertion(left2 == right2, "p2"),
  ])
"""),
      "Test",
      2
    )
  }
}
