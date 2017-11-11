package org.bykn.edgemar

import cats.data.NonEmptyList
import Parser.Combinators
import fastparse.all._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.typelevel.paiges.Document

class ParserTest extends FunSuite {
  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)
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
  test("we can parse Declaration.LiteralBool") {
    parseTestAll(Declaration.literalBoolP,
      Declaration.LiteralBool(false).toDoc.render(80),
      Declaration.LiteralBool(false))
    parseTestAll(Declaration.literalBoolP,
      Declaration.LiteralBool(true).toDoc.render(80),
      Declaration.LiteralBool(true))
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
    def law(decl: Declaration) =
      parseTestAll(Declaration.parser(""),
        decl.toDoc.render(80),
        decl)

    forAll(Generators.genDeclaration(5))(law _)

    import Declaration._
    import Operator._
    import TypeRef._
    // here are some previous failures:
    val hard0 =
      Op(Parens(LiteralBool(false)),Sub,
        Parens(DefFn(
          DefStatement("hKlsdabavsw",
            NonEmptyList.of(("wlqc3hcyppP",None), ("qzbCqgrbe",Option(TypeVar("lvg")))),
            None,
            (Padding(6,Indented(12,LiteralInt("-8393308213904846225"))),
              Padding(1,
                DefFn(DefStatement("j",
                  NonEmptyList.of(("kdco9d",None), ("ilmZf8modds",Option.empty[TypeRef])),
                  None,
                  (Padding(0,Indented(5,Var("whmloukl"))),
                    Padding(9,Op(Parens(
                      DefFn(
                        DefStatement("vef",
                          NonEmptyList.of(("o",Some(TypeArrow(TypeVar("gpjgctxq"),TypeName("G"))))),
                          None,
                          (Padding(3,Indented(5,LiteralBool(false))),
                            Padding(3,LiteralBool(false)))))),Plus,Parens(LiteralBool(true))))))))
              )))))
    val hard1 =
      Op(Parens(DefFn(
        DefStatement("qvn9nctvpe",NonEmptyList.of(("ccetywawql",None)),None,
          (Padding(8,Indented(3,
            DefFn(DefStatement("iMpmafg",NonEmptyList.of(("btblmdnhnij",Some(TypeName("I0UdK")))),Some(TypeName("Pjwrd")),
              (Padding(1,Indented(3,DefFn(
                DefStatement("rcvWloarpN",
                  NonEmptyList.of(("gicncD5v",Some(TypeVar("hlspaxy4c4")))),Some(TypeVar("xn")),
                  (Padding(2,Indented(11,LiteralBool(true))),
                    Padding(5,Comment(CommentStatement(NonEmptyList.of(""),Padding(10,LiteralInt("8784604272431575973")))))))))),
                Padding(4,
                  DefFn(DefStatement("e5g",NonEmptyList.of(("vjb0",Some(TypeArrow(TypeName("X3oaxhok"),TypeVar("bplm0x3"))))),None,
                    (Padding(4,Indented(8,Op(Parens(Var("uw")),Plus,Parens(LiteralBool(false))))),
                      Padding(9,Op(Parens(LiteralInt("4189753091618908546561077944446038042")),Plus,Parens(LiteralBool(true))))))))))))),
            Padding(4,Comment(CommentStatement(NonEmptyList.of(""),Padding(5,Comment(CommentStatement(NonEmptyList.of(""),Padding(4,LiteralBool(false))))))))))
      )),Mul,Parens(LiteralBool(false)))

    // def showPos(i: Int, str: String): String =
    //   str.substring(0, i) + "*" + str.substring(i + 1)

    // println("-----------------------")
    // println(hard1.toDoc.render(80))
    // println("-----------------------")
    // println(showPos(207, hard1.toDoc.render(80)))
    // println("-----------------------")

    val hardCases = List(hard0, hard1)
    hardCases.foreach(law _)
  }

  test("we can parse any Statement") {
    def law(statement: Statement) =
      parseTestAll(Statement.parser,
        Statement.document.document(statement).render(80),
        statement)

    //forAll(Generators.genStatement)(law _)

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
    law(hardCase0)

    val hardCase1 = {
      import TypeRef.TypeVar
      import Statement.{Def, EndOfFile, Bind, Comment}
      import Declaration._

  Bind(BindingStatement("gwvbRq", LiteralInt("3"),
  Padding(10,Comment(CommentStatement(NonEmptyList.of(""),
    Padding(2,Comment(CommentStatement(NonEmptyList.of(""),
      Padding[Statement](10,Def(DefStatement("hf4yfc7B0gg",NonEmptyList.of(("xHZgqebwy",Some(TypeVar("fhlhxenBm")))),None,
        (Padding(7,Indented(3,Declaration.Comment(CommentStatement(NonEmptyList.of(""),
          Padding(10,Var("kyowgc")))))),
  Padding(8,Def(DefStatement("xbvfkk",NonEmptyList.of(("wxtp",None)),None,
    (Padding(10,Indented(11,Op(Parens(LiteralBool(true)),Operator.Mul,
      Parens(LiteralInt("-30284323492203273957034407858667371620"))))),Padding(5,EndOfFile)))))))))))))))))
    }
    law(hardCase1)

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
}
