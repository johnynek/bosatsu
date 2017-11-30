package org.bykn.bosatsu

import cats.data.NonEmptyList
import Parser.Combinators
import fastparse.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.typelevel.paiges.Document

class ParserTest extends FunSuite {
  // This is so we can make Declarations without the region
  private[this] implicit val emptyRegion: Region = Region(0, 0)

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 500)
    PropertyCheckConfiguration(minSuccessful = 50)
    //PropertyCheckConfiguration(minSuccessful = 5)

  def region(s0: String, idx: Int): String = {
    val s = s0.updated(idx, '*')
    ("...(" + s.drop(idx - 20).take(20) + ")...")
  }

  def firstDiff(s1: String, s2: String): String =
    if (s1 == s2) ""
    else if (s1.isEmpty) s2
    else if (s2.isEmpty) s1
    else if (s1(0) == s2(0)) firstDiff(s1.tail, s2.tail)
    else s"${s1(0).toInt}: ${s1.take(20)}... != ${s2(0).toInt}: ${s2.take(20)}..."

  def parseTest[T](p: Parser[T], str: String, expected: T, exidx: Int) =
    p.parse(str) match {
      case Parsed.Success(t, idx) =>
        assert(t == expected)
        assert(idx == exidx)
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
    }

  def parseTestAll[T](p: Parser[T], str: String, expected: T) =
    parseTest(p, str, expected, str.length)

  def roundTrip[T: Document](p: Parser[T], str: String) =
    p.parse(str) match {
      case Parsed.Success(t, idx) =>
        assert(idx == str.length)
        val tstr = Document[T].document(t).render(80)
        p.parse(tstr) match {
          case Parsed.Success(t1, _) =>
            assert(t1 == t)
          case Parsed.Failure(exp, idx, extra) =>
            val diff = firstDiff(str, tstr)
            fail(s"Diff: $diff.\nfailed to reparse: $tstr: $exp at $idx in region ${region(tstr, idx)} with trace: ${extra.traced.trace}")
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
    }

  def law[T: Document](p: Parser[T])(t: T) =
    parseTestAll(p, Document[T].document(t).render(80), t)

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

  test("we can parse quoted strings") {
    val qstr = for {
      qchar <- Gen.oneOf('\'', '"')
      qstr = qchar.toString
      str <- Arbitrary.arbitrary[String]
    } yield (qstr + Parser.escape(Set(qchar), str) + qstr, str, qchar)

    forAll(qstr) { case (quoted, str, char) =>
      parseTestAll(Parser.escapedString(char), quoted, str)
    }

    parseTestAll(Parser.escapedString('\''), "''", "")
    parseTestAll(Parser.escapedString('"'), "\"\"", "")
    parseTestAll(Parser.escapedString('\''), "'foo\tbar'", "foo\tbar")
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
    val gen = Generators.commentGen(Generators.padding(Generators.genDeclaration(0), 1))
    forAll(gen) { comment =>
      parseTestAll(CommentStatement.parser("", Padding.parser(Declaration.parser(""))),
        Document[CommentStatement[Padding[Declaration]]].document(comment).render(80),
        comment)
    }

    val parensComment = """(#foo
#bar

1)"""
    parseTestAll(
      Declaration.parser(""),
      parensComment,
      Declaration.Parens(Declaration.Comment(
        CommentStatement(NonEmptyList.of("foo", "bar"),
          Padding(1, Declaration.LiteralInt("1"))))))
  }

  test("we can parse Declaration.LiteralInt") {
    forAll { bi: BigInt =>
      val litInt = Declaration.LiteralInt(bi.toString)
      parseTestAll(Declaration.literalIntP, litInt.toDoc.render(80), litInt)
    }
  }

  test("we can parse DefStatement") {
    forAll(Generators.defGen(Generators.indented(Generators.genDeclaration(0)))) { defn =>
      parseTestAll[DefStatement[Indented[Declaration]]](
        DefStatement.parser(Indented.parser(Declaration.parser(_))),
        Document[DefStatement[Indented[Declaration]]].document(defn).render(80),
        defn)
    }

    val defWithComment = """def foo(a):
  # comment here
  a
foo"""
    parseTestAll(
      Declaration.parser(""),
      defWithComment,
      Declaration.DefFn(DefStatement("foo", NonEmptyList.of(("a", None)), None,
        (Padding(0, Indented(2, Declaration.Comment(CommentStatement(NonEmptyList.of(" comment here"),
          Padding(0, Declaration.Var("a")))))),
         Padding(0, Declaration.Var("foo"))))))

  }

  test("we can parse BindingStatement") {
    parseTestAll(Declaration.parser(""),
      """foo = 5

5""",
    Declaration.Binding(BindingStatement("foo", Declaration.LiteralInt("5"),
      Padding(1, Declaration.LiteralInt("5")))))
  }

  test("we can parse any Apply") {
    import Declaration._

    parseTestAll(parser(""),
      "x(f)",
      Apply(Var("x"), NonEmptyList.of(Var("f")), false))

    parseTestAll(parser(""),
      "f.x",
      Apply(Var("x"), NonEmptyList.of(Var("f")), true))

    parseTestAll(parser(""),
      "f(foo).x",
      Apply(Var("x"), NonEmptyList.of(Apply(Var("f"), NonEmptyList.of(Var("foo")), false)), true))

    parseTestAll(parser(""),
      "f.foo(x)", // foo(f, x)
      Apply(Var("foo"), NonEmptyList.of(Var("f"), Var("x")), true))

    parseTestAll(parser(""),
      "(\\x -> x)(f)",
      Apply(Parens(Lambda(NonEmptyList.of("x"), Var("x"))), NonEmptyList.of(Var("f")), false))

    parseTestAll(parser(""),
      "((\\x -> x)(f))",
      Parens(Apply(Parens(Lambda(NonEmptyList.of("x"), Var("x"))), NonEmptyList.of(Var("f")), false)))

    val expected = Apply(Parens(Parens(Lambda(NonEmptyList.of("x"), Var("x")))), NonEmptyList.of(Var("f")), false)
    parseTestAll(parser(""),
      "((\\x -> x))(f)",
      expected)

    parseTestAll(parser(""),
      expected.toDoc.render(80),
      expected)

    import Operator._

    val hard = Lambda(NonEmptyList.of("zl"),Lambda(NonEmptyList.of("mbovroDewsy", "kapdcnqxt", "rwd"),
      Op(Parens(Op(Parens(LiteralInt("-9223372036854775808")),Sub,Parens(Var("anbo7gkpfvj")))),Sub,
        Parens(Apply(Parens(Lambda(NonEmptyList.of("uxu1i"),LiteralInt("35595864962462191423853794339110335496"))),
          NonEmptyList.of(Op(Parens(LiteralInt("1678198301007947346")),Sub,Parens(LiteralInt("-2902090857627552045")))), false)))))

    parseTestAll(parser(""),
      hard.toDoc.render(80),
      hard)

  }

  test("we can parse bind") {
    import Declaration._

    parseTestAll(parser(""),
      """x = 4
x""",
    Binding(BindingStatement("x", LiteralInt("4"), Padding(0, Var("x")))))

    parseTestAll(parser(""),
      """x = foo(4)

x""",
    Binding(BindingStatement("x", Apply(Var("foo"), NonEmptyList.of(LiteralInt("4")), false), Padding(1, Var("x")))))

    parseTestAll(parser(""),
      """x = foo(4)
# x is really great
x""",
    Binding(BindingStatement("x",Apply(Var("foo"),NonEmptyList.of(LiteralInt("4")), false),Padding(0,Comment(CommentStatement(NonEmptyList.of(" x is really great"),Padding(0,Var("x"))))))))

  }

  test("we can parse if") {
    import Declaration._

    parseTestAll(parser(""),
      """if x == 3:
      x
else:
      y""",
      IfElse(NonEmptyList.of((Op(Var("x"),Operator.Eql,LiteralInt("3")),Padding(0,Indented(6,Var("x"))))),Padding(0,Indented(6,Var("y")))))

    parseTestAll(parser(""),
      """if x == 3:
      x
elif foo:
      z
else:
      y""",
      IfElse(NonEmptyList.of((Op(Var("x"),Operator.Eql,LiteralInt("3")),Padding(0,Indented(6,Var("x")))), (Var("foo"),Padding(0,Indented(6,Var("z"))))),Padding(0,Indented(6,Var("y")))))
  }

  test("we can parse a match") {
    roundTrip(Declaration.parser(""),
"""match 1:
  Foo(a, b):
    a + b
  Bar:
    42""")

    roundTrip(Declaration.parser(""),
"""match 1:
  Foo(a, b):
    a + b
  Bar:
    match x:
      True:
        100
      False:
        99""")

    roundTrip(Declaration.parser(""),
"""foo(1, match 2:
  Foo:

    foo
  Bar:

    # this is the bar case
    bar, 100)""")

    roundTrip(Declaration.parser(""),
"""if match 2:
  Foo:

    foo
  Bar:

    # this is the bar case
    bar:
  1
else:
  2""")

    roundTrip(Declaration.parser(""),
"""if True:
  match 1:
    Foo(f):
      1
else:
  100""")
  }

  test("we can parse any Declaration") {
    forAll(Generators.genDeclaration(5))(law(Declaration.parser("")))
  }

  test("we can parse any Statement") {
    forAll(Generators.genStatement)(law(Statement.parser))

    val hardCase0 = {
      import TypeRef._
      import Statement.{Def, EndOfFile}
      import Declaration._

      Def(DefStatement("qxGfrHvom",
        NonEmptyList.of(("xvgv",Some(TypeName("IObfma8"))), ("uvajpvmKfI",Some(TypeName("Li9bou4io")))),None,
        (Padding(6,Indented(12,Var("jbbytnfws"))),
          Padding(0,Def(DefStatement("ik5xfx",NonEmptyList.of(("f8u0vVcix",Some(TypeVar("caf"))), ("nu",None), ("uv",Some(TypeName("R1"))),
            ("gp",Some(TypeName("ANxKclqu"))), ("asszzmvJE",None)),Some(TypeVar("fxp")),
          (Padding(2,Indented(4,LiteralInt("-9223372036854775809"))),
            Padding(6,Statement.Comment(CommentStatement(
              NonEmptyList.of("foo", "bar"),
              Padding[Statement](1,EndOfFile)))))))))))
    }
    law(Statement.parser)(hardCase0)

    roundTrip(Statement.parser,
"""# header
y = if x == 2:
  True
else:
  False

def foo(x: Integer, y: String) -> String:
  toString(x) + y

# here is a lambda
fn = \x, y -> x + y

x = ( foo )

""")
  }

  test("parse external defs") {
    roundTrip(Statement.parser,
"""# header
external def foo -> String
""")
    roundTrip(Statement.parser,
"""# header
external def foo(i: Integer) -> String
""")
    roundTrip(Statement.parser,
"""# header
external def foo(i: Integer, b: a) -> String

external def foo2(i: Integer, b: a) -> String
""")
  }

  test("we can parse any package") {
    roundTrip(Package.parser,
"""
package Foo/Bar
import Baz [Bippy]
export [foo]

foo = 1
""")

    forAll(Generators.packageGen)(law(Package.parser))
  }

  test("we can parse Externals") {
    parseTestAll(Externals.parser,
"""
Foo/Bar flatMap scala org.bykn.bosatsu.Std.flatMap
Foo/Bar fold scala org.bykn.bosatsu.Std.fold
""",
   Externals.empty
     .add(PackageName.parse("Foo/Bar").get, "flatMap", FfiCall.ScalaCall("org.bykn.bosatsu.Std.flatMap"))
     .add(PackageName.parse("Foo/Bar").get, "fold", FfiCall.ScalaCall("org.bykn.bosatsu.Std.fold")))
  }
}
