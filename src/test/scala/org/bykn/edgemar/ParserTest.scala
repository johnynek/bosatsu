package org.bykn.edgemar

import cats.data.NonEmptyList
import Parser.Combinators
import fastparse.all._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.forAll

class ParserTest extends FunSuite {

  def i(in: Int): Expr = Expr.Literal(Lit.Integer(in))
  def b(bo: Boolean): Expr = Expr.Literal(Lit.Bool(bo))
  def v(str: String): Expr = Expr.Var(str)

  def parseTest[T](p: Parser[T], str: String, expected: T, exidx: Int) =
    p.parse(str) match {
      case Parsed.Success(t, idx) =>
        assert(t == expected)
        assert(idx == exidx)
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
    }

  def parseTestAll[T](p: Parser[T], str: String, expected: T) =
    parseTest(p, str, expected, str.length)

  def expectFail[T](p: Parser[T], str: String, atIdx: Int) =
    p.parse(str) match {
      case Parsed.Success(t, idx) => fail(s"parsed $t to: $idx")
      case Parsed.Failure(_, idx, _) =>
        assert(idx == atIdx)
    }

  test("we can parse integers") {
    forAll { b: BigInt =>
      parseTestAll(Parser.integerString, b.toString, b.toString)
    }
  }

  test("we can parse lists") {
    forAll { (ls: List[Long], spaceCnt0: Int) =>
      val spaceCount = spaceCnt0 & 7
      val str0 = ls.toString
      val str = str0.flatMap {
        case ',' => "," + (" " * spaceCount)
        case c => c.toString
      }
      parseTestAll(Parser.integerString.list.wrappedSpace("List(", ")"),
        str,
        ls.map(_.toString))
    }
  }

  test("we can parse TypeRefs") {
    parseTestAll(TypeRef.parser, "foo", TypeRef.TypeVar("foo"))
    parseTestAll(TypeRef.parser, "Foo", TypeRef.TypeName("Foo"))
    parseTestAll(TypeRef.parser, "Foo -> Bar", TypeRef.TypeArrow(TypeRef.TypeName("Foo"), TypeRef.TypeName("Bar")))
    parseTestAll(TypeRef.parser, "Foo -> Bar -> baz",
      TypeRef.TypeArrow(TypeRef.TypeName("Foo"), TypeRef.TypeArrow(TypeRef.TypeName("Bar"), TypeRef.TypeVar("baz"))))
    parseTestAll(TypeRef.parser, "(Foo -> Bar) -> baz",
      TypeRef.TypeArrow(TypeRef.TypeArrow(TypeRef.TypeName("Foo"), TypeRef.TypeName("Bar")), TypeRef.TypeVar("baz")))
    parseTestAll(TypeRef.parser, "Foo[Bar]", TypeRef.TypeApply(TypeRef.TypeName("Foo"), NonEmptyList.of(TypeRef.TypeName("Bar"))))

    forAll(Generators.typeRefGen) { tref =>
      parseTestAll(TypeRef.parser, tref.toDoc.render(80), tref)
    }
  }

  test("we can parse comments") {
    val gen = Generators.commentGen(Gen.const(Declaration.EndOfFile))
    forAll(gen) { comment =>
      parseTestAll(Declaration.commentP,
        comment.toDoc.render(80),
        comment)
    }
  }

  test("we can parse Declaration.LiteralInt") {
    forAll { bi: BigInt =>
      val litInt = Declaration.LiteralInt(bi.toString)
      parseTestAll(Declaration.literalIntP, litInt.toDoc.render(80), litInt)
    }
  }
  test("we can parse Declaration.LiteralBool") {
    parseTestAll(Declaration.literalBoolP,
      Declaration.LiteralBool(false).toDoc.render(80),
      Declaration.LiteralBool(false))
    parseTestAll(Declaration.literalBoolP,
      Declaration.LiteralBool(true).toDoc.render(80),
      Declaration.LiteralBool(true))
  }

  test("we can parse any Declaration") {
    forAll(Generators.genDeclaration(5)) { decl =>
      parseTestAll(Declaration.parser,
        decl.toDoc.render(80),
        decl)
    }
  }

  test("we can parse Expr.Var") {
    parseTest(Parser.variable, "a", v("a"), 1)
    parseTest(Parser.variable, "ab", v("ab"), 2)
    expectFail(Parser.variable, " ab", 0)
  }

  test("we can parse Expr.App") {
    parseTest(Parser.expr, "a(b)", Expr.App(v("a"), v("b")), 4)
    parseTest(Parser.expr, "ab( ab )", Expr.App(v("ab"), v("ab")), 8)
    expectFail(Parser.expr, " ab()", 0)
    parseTest(Parser.expr, "(lambda x: x)(1)", Expr.App(Expr.Lambda("x", v("x")), i(1)), 16)
  }

  test("we can parse Expr.Literal") {
    parseTest(Parser.expr, "true", b(true), 4)
    parseTest(Parser.expr, "false", b(false), 5)
    parseTest(Parser.expr, "42", i(42), 2)
  }

  test("we can parse operators") {
    parseTest(Parser.expr, "1 + 2", Expr.Op(i(1), Operator.Plus, i(2)), 5)
    parseTest(Parser.expr, "1 * 2", Expr.Op(i(1), Operator.Mul, i(2)), 5)
    parseTest(Parser.expr, "(1 + 1) == 2", Expr.Op(Expr.Op(i(1), Operator.Plus, i(1)), Operator.Eql, i(2)), 12)
  }

  test("we can parse if then") {
    val ifthen =
"""if 1 == 2:
    42
else:
    54"""
    parseTest(Parser.expr, ifthen, Expr.If(Expr.Op(i(1), Operator.Eql, i(2)), i(42), i(54)), 30)
    val ifthen2 =
"""if x == 2:
    42
else:
    54"""
    parseTest(Parser.expr, ifthen2, Expr.If(Expr.Op(v("x"), Operator.Eql, i(2)), i(42), i(54)), 30)
  }

  test("we can parse lambda") {
    parseTest(Parser.expr, "lambda x: x + 1", Expr.Lambda("x", Expr.Op(v("x"), Operator.Plus, i(1))), 15)
    parseTestAll(Parser.expr,
"""def foo(x):
  x + 1
foo(1)""",
      Expr.Let("foo", Expr.Lambda("x", Expr.Op(v("x"), Operator.Plus, i(1))), Expr.App(Expr.Var("foo"), i(1))))
    parseTestAll(Parser.expr, "lambda x:\n  x + 1", Expr.Lambda("x", Expr.Op(v("x"), Operator.Plus, i(1))))
    parseTestAll(Parser.expr, "lambda x, y: x + y", Expr.Lambda("x", Expr.Lambda("y", Expr.Op(v("x"), Operator.Plus, v("y")))))
    parseTestAll(Parser.expr, "lambda x: lambda y: x + y", Expr.Lambda("x", Expr.Lambda("y", Expr.Op(v("x"), Operator.Plus, v("y")))))
  }

  test("we can parse let") {
    parseTest(Parser.expr, "x = 1\nx", Expr.Let("x", i(1), v("x")), 7)
    parseTestAll(Parser.expr, "x = 1\ny = 2\nx * y", Expr.Let("x", i(1), Expr.Let("y", i(2), Expr.Op(v("x"), Operator.Mul, v("y")))))
    parseTestAll(
      Parser.expr,
"""lambda x:
  fn = lambda y: x + y
  fn""", Expr.Lambda("x", Expr.Let("fn", Expr.Lambda("y", Expr.Op(v("x"), Operator.Plus, v("y"))), v("fn"))))
  }

  test("we can parse ffi") {
    parseTestAll(Parser.expr, """ffi "java" "System.identityHashCode" Int -> Int""",
      Expr.Ffi("java", "System.identityHashCode", Scheme(Nil, Type.Arrow(Type.intT, Type.intT))))
  }

}
