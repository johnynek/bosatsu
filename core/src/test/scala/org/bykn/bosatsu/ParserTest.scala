package org.bykn.bosatsu

import cats.data.NonEmptyList
import Parser.Combinators
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.typelevel.paiges.{Doc, Document}

import cats.implicits._
import fastparse.all._
import Parser.Indy

import Generators.{shrinkDecl, shrinkStmt}

object TestParseUtils {
  def region(s0: String, idx: Int): String =
    if (s0.isEmpty) s"empty string, idx = $idx"
    else if (s0.length == idx) {
      val s = s0 + "*"
      ("...(" + s.drop(idx - 20).take(20) + ")...")
    }
    else {
      val s = s0.updated(idx, '*')
      ("...(" + s.drop(idx - 20).take(20) + ")...")
    }

  def firstDiff(s1: String, s2: String): String =
    if (s1 == s2) ""
    else if (s1.isEmpty) s2
    else if (s2.isEmpty) s1
    else if (s1(0) == s2(0)) firstDiff(s1.tail, s2.tail)
    else s"${s1(0).toInt}: ${s1.take(20)}... != ${s2(0).toInt}: ${s2.take(20)}..."

}

abstract class ParserTestBase extends FunSuite {
  import TestParseUtils._

  // This is so we can make Declarations without the region
  protected implicit val emptyRegion: Region = Region(0, 0)

  def parseUnsafe[A](p: Parser[A], str: String): A =
    p.parse(str) match {
      case Parsed.Success(a, idx) =>
        assert(idx == str.length)
        a
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
        sys.error("nope")
    }
  def parseOpt[A](p: Parser[A], str: String): Option[A] =
    p.parse(str) match {
      case Parsed.Success(a, idx) if idx == str.length =>
        Some(a)
      case _ =>
        None
    }


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

  def roundTrip[T: Document](p: Parser[T], str: String, lax: Boolean = false) =
    p.parse(str) match {
      case Parsed.Success(t, idx) =>
        if (!lax) {
          assert(idx == str.length, s"parsed: $t from: $str")
        }
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

  def config: PropertyCheckConfiguration = {
    if (System.getenv("PLATFORM") == "js")
      PropertyCheckConfiguration(minSuccessful = 10)
    else
      PropertyCheckConfiguration(minSuccessful = 300)
  }
}

class ParserTest extends ParserTestBase {
  import TestParseUtils._

  implicit val generatorDrivenConfig = config

  test("we can parse integers") {
    forAll { b: BigInt =>
      val bstr = b.toString
      parseTestAll(Parser.integerString, bstr, bstr)
    }

    // we can also add _ in between any two digits
    case class Opaque(toS: String) // no shrinking
    val strGen: Gen[Opaque] =
      Arbitrary.arbitrary[BigInt].flatMap { b =>
        val bstr = b.abs.toString.take(5) // don't go nuts in size
        // randomly put some _ in between
        val sep = Gen.frequency((10, Gen.const("")), (1, Gen.const("_")))

        def loop(b: String): Gen[String] =
          if (b.length <= 1) Gen.const(b)
          else for {
            s <- sep
            tail <- loop(b.tail)
          } yield s"${b.charAt(0)}$s$tail"

        loop(bstr).map(Opaque(_))
      }

    parseTestAll(Parser.integerString, "1_000", "1_000")
    forAll(strGen) { case Opaque(s) =>
      parseTestAll(Parser.integerString, s, s)
    }
  }

  test("string escape/unescape round trips") {
    forAll(Gen.oneOf('\'', '"'), Arbitrary.arbitrary[String]) { (c, str) =>
      val str1 = Parser.escape(c, str)
      try {
        Parser.unescape(str1) match {
          case Right(str2) => assert(str2 == str)
          case Left(idx) => fail(s"failed at idx: $idx in $str: ${region(str, idx)}")
        }
      }
      catch {
        case t: Throwable => fail(s"failed to decode: $str1 from $str, exception: $t")
      }
    }

    assert(Parser.escape('"', "\t") == "\\t")
    assert(Parser.escape('"', "\n") == "\\n")

    // unescape never throws:
    assert(Parser.unescape("\\x0").isLeft)
    assert(Parser.unescape("\\o0").isLeft)
    assert(Parser.unescape("\\u0").isLeft)
    assert(Parser.unescape("\\u00").isLeft)
    assert(Parser.unescape("\\u000").isLeft)
    assert(Parser.unescape("\\U0000").isLeft)
    forAll { s: String => Parser.unescape(s); succeed }
    // more brutal tests
    forAll { s: String =>
      val prefixes = List('x', 'o', 'u', 'U').map { c => s"\\$c" }
      prefixes.foreach { p =>
        Parser.unescape(s"$p$s")
        succeed
      }
    }

    assert(Parser.unescape("\\u0020") == Right(" "))
  }

  test("we can parse quoted strings") {
    val qstr = for {
      qchar <- Gen.oneOf('\'', '"')
      str <- Arbitrary.arbitrary[String]
    } yield (str, qchar)

    def law(str: String, qchar: Char) = {
      // we have to do this here, or otherwise broken scalacheck shrinking gets us
      val qstr = qchar.toString
      val quoted = qstr + Parser.escape(qchar, str) + qstr
      parseTestAll(Parser.escapedString(qchar), quoted, str)
    }

    forAll(qstr) { case (s, c) => law(s, c) }

    parseTestAll(Parser.escapedString('\''), "''", "")
    parseTestAll(Parser.escapedString('\''), "''", "")
    parseTestAll(Parser.escapedString('"'), "\"\"", "")
    parseTestAll(Parser.escapedString('\''), "'foo\tbar'", "foo\tbar")

    val regressions = List(("'", '\''))


    regressions.foreach { case (s, c) => law(s, c) }
  }

  test("Identifier round trips") {
    forAll(Generators.identifierGen)(law(Identifier.parser))

    val examples = List("foo", "`bar`", "`bar foo`",
      "`with \\`internal`", "operator +")

    examples.foreach(roundTrip(Identifier.parser, _))
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

  test("we can parse dicts") {
    val strDict = Parser.dictLikeParser(Parser.escapedString('\''), Parser.escapedString('\''))
    parseTestAll(strDict, "{}", Nil)
    parseTestAll(strDict, "{'a': 'b'}", List(("a", "b")))
    parseTestAll(strDict, "{ 'a' : 'b' }", List(("a", "b")))
    parseTestAll(strDict, "{'a' : 'b', 'c': 'd'}", List(("a", "b"), ("c", "d")))
    parseTestAll(strDict, "{'a' : 'b',\n'c': 'd'}", List(("a", "b"), ("c", "d")))
    parseTestAll(strDict, "{'a' : 'b',\n\t'c': 'd'}", List(("a", "b"), ("c", "d")))
    parseTestAll(strDict, "{'a' : 'b',\n  'c': 'd'}", List(("a", "b"), ("c", "d")))

    case class WildDict(stringRepNoCurlies: List[String], original: List[(String, String)]) {
      def stringRep: String = stringRepNoCurlies.mkString("{", "", "}")

      def addEntry(strings: List[String], k: String, v: String): WildDict =
        if (stringRepNoCurlies.isEmpty) WildDict(strings, (k, v) :: original)
        else WildDict(strings ::: ("," :: stringRepNoCurlies), (k, v) :: original)
    }

    val genString = Arbitrary.arbitrary[String]
    val wsGen = Gen.listOf(Gen.oneOf(' ', '\t', '\n')).map(_.mkString)
    def q(s: String): String = "'" + Parser.escape('\'', s) + "'"

    val genItem: Gen[(List[String], (String, String))] =
      for {
        preK <- wsGen
        k <- genString
        postK <- wsGen
        preV <- wsGen
        v <- genString
        postV <- wsGen
      } yield (List(preK, q(k), postK, ":", preV, q(v), postV), (k, v))

    val genWild: Gen[WildDict] =
      Gen.listOf(genItem).map { items =>
        items.foldLeft(WildDict(Nil, Nil)) { case (wd, (s, (k, v))) =>
          wd.addEntry(s, k, v)
        }
      }

    forAll(genWild) { wd =>
      parseTestAll(strDict, wd.stringRep, wd.original)
    }
  }

  test("we can parse tuples") {
    forAll { (ls: List[Long], spaceCnt0: Int) =>
      val spaceCount = spaceCnt0 & 7
      val pad = " " * spaceCount
      val str =
        ls match {
          case h :: Nil => s"($h,$pad)"
          case _ =>
            ls.mkString("(", "," + pad, ")")
        }
      parseTestAll(Parser.integerString.tupleOrParens,
        str,
        Right(ls.map(_.toString)))
    }

    // a single item is parsed as parens
    forAll { (it: Long, spaceCnt0: Int) =>
      val spaceCount = spaceCnt0 & 7
      val pad = " " * spaceCount
      val str = s"($it$pad)"
      parseTestAll(Parser.integerString.tupleOrParens,
        str,
        Left(it.toString))
    }
  }

  test("we can parse blocks") {
    val indy = Indy.block(Indy.lift(P("if foo")), Indy.lift(P("bar")))
    val p = indy.run("")
    parseTestAll(p, "if foo: bar", ((), OptIndent.same(())))
    parseTestAll(p, "if foo:\n\tbar", ((), OptIndent.paddedIndented(1, 4, ())))
    parseTestAll(p, "if foo:\n    bar", ((), OptIndent.paddedIndented(1, 4, ())))
    parseTestAll(p, "if foo:\n  bar", ((), OptIndent.paddedIndented(1, 2, ())))

    import Indy.IndyMethods
    val repeated = indy.nonEmptyList(Indy.lift(Parser.toEOL))

    val single = ((), OptIndent.notSame(Padding(1, Indented(2, ()))))
    parseTestAll(repeated.run(""), "if foo:\n  bar\nif foo:\n  bar",
      NonEmptyList.of(single, single))

    // we can nest blocks
    parseTestAll(Indy.block(Indy.lift(P("nest")), indy)(""), "nest: if foo: bar",
      ((), OptIndent.same(((), OptIndent.same(())))))
    parseTestAll(Indy.block(Indy.lift(P("nest")), indy)(""), "nest:\n  if foo: bar",
      ((), OptIndent.paddedIndented(1, 2, ((), OptIndent.same(())))))
    parseTestAll(Indy.block(Indy.lift(P("nest")), indy)(""), "nest:\n  if foo:\n    bar",
      ((), OptIndent.paddedIndented(1, 2, ((), OptIndent.paddedIndented(1, 2, ())))))

    val simpleBlock = Indy.block(Indy.lift(Parser.lowerIdent ~ Parser.maybeSpace), Indy.lift(Parser.lowerIdent))
      .nonEmptyList(Indy.toEOLIndent)

    val sbRes = NonEmptyList.of(("x1", OptIndent.paddedIndented(1, 2, "x2")),
        ("y1", OptIndent.paddedIndented(1, 3, "y2")))
    parseTestAll(simpleBlock(""), "x1:\n  x2\ny1:\n   y2", sbRes)

    parseTestAll(Indy.block(Indy.lift(Parser.lowerIdent), simpleBlock)(""),
      "block:\n  x1:\n    x2\n  y1:\n     y2",
      ("block", OptIndent.paddedIndented(1, 2, sbRes)))
  }

  def trName(s: String): TypeRef.TypeName =
    TypeRef.TypeName(TypeName(Identifier.Constructor(s)))

  test("we can parse TypeRefs") {
    parseTestAll(TypeRef.parser, "foo", TypeRef.TypeVar("foo"))
    parseTestAll(TypeRef.parser, "Foo", trName("Foo"))

    parseTestAll(TypeRef.parser, "forall a. a", TypeRef.TypeLambda(NonEmptyList.of(TypeRef.TypeVar("a")), TypeRef.TypeVar("a")))
    parseTestAll(TypeRef.parser, "forall a, b. f[a] -> f[b]",
      TypeRef.TypeLambda(NonEmptyList.of(TypeRef.TypeVar("a"), TypeRef.TypeVar("b")),
        TypeRef.TypeArrow(
          TypeRef.TypeApply(TypeRef.TypeVar("f"), NonEmptyList.of(TypeRef.TypeVar("a"))),
          TypeRef.TypeApply(TypeRef.TypeVar("f"), NonEmptyList.of(TypeRef.TypeVar("b"))))))
    roundTrip(TypeRef.parser, "forall a, b. f[a] -> f[b]")
    roundTrip(TypeRef.parser, "(forall a, b. f[a]) -> f[b]")
    roundTrip(TypeRef.parser, "(forall a, b. f[a])[Int]") // apply a type

    parseTestAll(TypeRef.parser, "Foo -> Bar", TypeRef.TypeArrow(trName("Foo"), trName("Bar")))
    parseTestAll(TypeRef.parser, "Foo -> Bar -> baz",
      TypeRef.TypeArrow(trName("Foo"), TypeRef.TypeArrow(trName("Bar"), TypeRef.TypeVar("baz"))))
    parseTestAll(TypeRef.parser, "(Foo -> Bar) -> baz",
      TypeRef.TypeArrow(TypeRef.TypeArrow(trName("Foo"), trName("Bar")), TypeRef.TypeVar("baz")))
    parseTestAll(TypeRef.parser, "Foo[Bar]", TypeRef.TypeApply(trName("Foo"), NonEmptyList.of(trName("Bar"))))

    forAll(Generators.typeRefGen) { tref =>
      parseTestAll(TypeRef.parser, tref.toDoc.render(80), tref)
    }
  }

  test("we can parse python style list expressions") {
    val pident = Parser.lowerIdent
    implicit val stringDoc: Document[String] = Document.instance[String](Doc.text(_))

    val llp = ListLang.parser(pident, pident)
    roundTrip(llp, "[a]")
    roundTrip(llp, "[]")
    roundTrip(llp, "[  ]")
    roundTrip(llp, "[a , b]")
    roundTrip(llp, "[a , b]")
    roundTrip(llp, "[a , *b]")
    roundTrip(llp, "[a ,\n*b,\n c]")
    roundTrip(llp, "[x for y in z]")
    roundTrip(llp, "[x for y in z if w]")
    roundTrip(ListLang.SpliceOrItem.parser(pident), "a")
    roundTrip(ListLang.SpliceOrItem.parser(pident), "*a")
  }

  test("we can parse operators") {
    val singleToks = List(
      "+", "-", "*", "!", "$", "%",
      "^", "&", "*", "|", "?", "/", "<",
      ">", "~")
    val withEq = "=" :: singleToks

    val allLen2 = (withEq, withEq).mapN(_ + _)
    val allLen3 = (allLen2, withEq).mapN(_ + _)

    (singleToks ::: allLen2 ::: allLen3).foreach { opStr =>
      roundTrip(Operators.operatorToken, opStr)
    }

    expectFail(Operators.operatorToken, "=", 0)
  }
}

/**
 * This is a separate class since some of these are very slow
 */
class SyntaxParseTest extends ParserTestBase {

  implicit val generatorDrivenConfig = config

  def mkVar(n: String): Declaration.Var =
    Declaration.Var(Identifier.Name(n))

  test("we can parse comments") {
    val gen = Generators.commentGen(Generators.padding(Generators.genDeclaration(0), 1))
    forAll(gen) { comment =>
      parseTestAll(CommentStatement.parser(Parser.Indy.lift(Padding.parser(Declaration.parser("")))).run(""),
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
          Padding(1, Declaration.Literal(Lit.fromInt(1)))))))
  }

  test("we can parse Lit.Integer") {
    forAll { bi: BigInt =>
      roundTrip(Lit.parser, bi.toString)
    }
  }

  test("we can parse DefStatement") {
    val indDoc = Document[Indented[Declaration]]

    forAll(Generators.defGen(Generators.optIndent(Generators.genDeclaration(0)))) { defn =>
      parseTestAll[DefStatement[Pattern.Parsed, OptIndent[Declaration]]](
        DefStatement.parser(Pattern.bindParser, Parser.maybeSpace ~ OptIndent.indy(Declaration.parser).run("")),
        Document[DefStatement[Pattern.Parsed, OptIndent[Declaration]]].document(defn).render(80),
        defn)
    }

    val defWithComment = """def foo(a):
  # comment here
  a
foo"""
    parseTestAll(
      Declaration.parser(""),
      defWithComment,
      Declaration.DefFn(DefStatement(Identifier.Name("foo"), List(Pattern.Var(Identifier.Name("a"))), None,
        (OptIndent.paddedIndented(1, 2, Declaration.Comment(CommentStatement(NonEmptyList.of(" comment here"),
          Padding(0, mkVar("a"))))),
         Padding(0, mkVar("foo"))))))

    roundTrip(Declaration.parser(""), defWithComment)

    // Here is a pretty brutal randomly generated case
    roundTrip(Declaration.parser(""),
"""def uwr(dw: h6lmZhgg) -> forall lnNR. Z5syis -> Mhgm:
  -349743008

foo""")

  }

  test("we can parse BindingStatement") {
    val dp = Declaration.parser("")
    parseTestAll(dp,
      """foo = 5

5""",
    Declaration.Binding(BindingStatement(Pattern.Var(Identifier.Name("foo")), Declaration.Literal(Lit.fromInt(5)),
      Padding(1, Declaration.Literal(Lit.fromInt(5))))))


    roundTrip(dp,
"""#
Pair(_, x) = z
x""")
  }

  test("we can parse any Apply") {
    import Declaration._

    import ApplyKind.{Dot => ADot, Parens => AParens }

    parseTestAll(parser(""),
      "x(f)",
      Apply(mkVar("x"), NonEmptyList.of(mkVar("f")), AParens))

    parseTestAll(parser(""),
      "f.x",
      Apply(mkVar("x"), NonEmptyList.of(mkVar("f")), ADot))

    parseTestAll(parser(""),
      "f(foo).x",
      Apply(mkVar("x"), NonEmptyList.of(Apply(mkVar("f"), NonEmptyList.of(mkVar("foo")), AParens)), ADot))

    parseTestAll(parser(""),
      "f.foo(x)", // foo(f, x)
      Apply(mkVar("foo"), NonEmptyList.of(mkVar("f"), mkVar("x")), ADot))

    parseTestAll(parser(""),
      "(\\x -> x)(f)",
      Apply(Parens(Lambda(NonEmptyList.of(Pattern.Var(Identifier.Name("x"))), mkVar("x"))), NonEmptyList.of(mkVar("f")), AParens))

    parseTestAll(parser(""),
      "((\\x -> x)(f))",
      Parens(Apply(Parens(Lambda(NonEmptyList.of(Pattern.Var(Identifier.Name("x"))), mkVar("x"))), NonEmptyList.of(mkVar("f")), AParens)))

    val expected = Apply(Parens(Parens(Lambda(NonEmptyList.of(Pattern.Var(Identifier.Name("x"))), mkVar("x")))), NonEmptyList.of(mkVar("f")), AParens)
    parseTestAll(parser(""),
      "((\\x -> x))(f)",
      expected)

    parseTestAll(parser(""),
      expected.toDoc.render(80),
      expected)

  }

  test("we can parse patterns") {
    roundTrip(Pattern.matchParser, "Foo([])")
    roundTrip(Pattern.matchParser, "Foo([], bar)")
    roundTrip(Pattern.matchParser, "x")
    roundTrip(Pattern.matchParser, "_")
    roundTrip(Pattern.matchParser, "(a, b)")
    roundTrip(Pattern.matchParser, "(a, b) | _")
    roundTrip(Pattern.matchParser, "foo @ _")
    roundTrip(Pattern.matchParser, "foo @ Some(_) | None")
    roundTrip(Pattern.matchParser, "Bar | foo @ Some(_) | None")
    roundTrip(Pattern.bindParser, "x: Int")

    implicit def docList[A: Document]: Document[NonEmptyList[A]] =
      Document.instance[NonEmptyList[A]] { nes =>
        Doc.intercalate(Doc.text(", "), nes.toList.map(Document[A].document(_)))
      }
    roundTrip(Pattern.bindParser.nonEmptyList, "x: Int")
    roundTrip(Pattern.bindParser.nonEmptyList, "x: Int, y, Foo(z: Int)")
  }

  test("Declaration.toPattern works for all Pattern-like declarations") {
    def law1(dec: Declaration) = {
      Declaration.toPattern(dec) match {
        case None => fail("expected to convert to pattern")
        case Some(pat) =>
          // if we convert to string this parses the same as a pattern:
          val decStr = dec.toDoc.render(80)
          val parsePat = parseUnsafe(Pattern.matchParser, decStr)
          assert(pat == parsePat)
      }
    }
    forAll(Generators.patternDecl(4))(law1(_))

    {
      import Declaration._
      import Identifier.{Name, Operator}
      // this operator application can be a pattern
      val regression = ApplyOp(Var(Name("q")),Operator("|"),Var(Name("npzma")))
      law1(regression)
    }

    // for all Declarations, either it parses like a pattern or toPattern is None
    forAll(Generators.genDeclaration(5)) { dec =>
      val decStr = dec.toDoc.render(80)
      val parsePat = parseOpt(Pattern.matchParser, decStr)
      (Declaration.toPattern(dec), parsePat) match {
        case (None, None) => succeed
        case (Some(p0), Some(p1)) => assert(p0 == p1)
        case (None, Some(_)) => fail(s"toPattern failed, but parsed $decStr to: $parsePat")
        case (Some(p), None) => fail(s"toPattern succeeded: $p but pattern parse failed")
      }
    }


    def testEqual(decl: String) = {
      val dec = parseUnsafe(Declaration.parser(""), decl)
      val patt = parseUnsafe(Pattern.matchParser, decl)
      Declaration.toPattern(dec) match {
        case Some(p2) => assert(p2 == patt)
        case None => fail(s"could not convert $decl to pattern")
      }
    }

    testEqual("a")
    testEqual("Foo(a)")
    testEqual("[1, Foo, a]")
    testEqual("[*a, Foo([]), bar]")
  }

  test("we can parse bind") {
    import Declaration._

    parseTestAll(parser(""),
      """x = 4
x""",
    Binding(BindingStatement(Pattern.Var(Identifier.Name("x")), Literal(Lit.fromInt(4)), Padding(0, mkVar("x")))))

    parseTestAll(parser(""),
      """x = foo(4)

x""",
    Binding(BindingStatement(Pattern.Var(Identifier.Name("x")), Apply(mkVar("foo"), NonEmptyList.of(Literal(Lit.fromInt(4))), ApplyKind.Parens), Padding(1, mkVar("x")))))

    parseTestAll(parser(""),
      """x = foo(4)
# x is really great
x""",
    Binding(BindingStatement(Pattern.Var(Identifier.Name("x")),Apply(mkVar("foo"),NonEmptyList.of(Literal(Lit.fromInt(4))), ApplyKind.Parens),Padding(0,Comment(CommentStatement(NonEmptyList.of(" x is really great"),Padding(0,mkVar("x"))))))))

  }

  test("we can parse if") {
    import Declaration._

    roundTrip[Declaration](ifElseP(Parser.Indy.lift(varP))(""),
      """if w:
      x
else:
      y""")
    roundTrip(parser(""),
      """if eq_Int(x, 3):
      x
else:
      y""")

    roundTrip(parser(""),
      """if eq_Int(x, 3):
      x
elif foo:
   z
else:
      y""")

    roundTrip[Declaration](ifElseP(Parser.Indy.lift(varP))(""),
      """if w: x
else: y""")
    roundTrip(parser(""),
      """if eq_Int(x, 3): x
else: y""")

    roundTrip(parser(""),
      """if eq_Int(x, 3): x
elif foo:
      z
else: y""")
  }

  test("we can parse a match") {
    roundTrip[Declaration](Declaration.matchP(Parser.Indy.lift(Declaration.varP))(""),
"""match x:
  y:
    z
  w:
    r""")
    roundTrip(Declaration.parser(""),
"""match 1:
  Foo(a, b):
    a.plus(b)
  Bar:
    42""")
    roundTrip(Declaration.parser(""),

"""match 1:
  (a, b):
    a.plus(b)
  ():
    42""")

    roundTrip(Declaration.parser(""),

"""match 1:
  (a, (b, c)):
    a.plus(b).plus(e)
  (1,):
    42""")

    roundTrip(Declaration.parser(""),
"""match 1:
  Foo(a, b):
    a.plus(b)
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

    roundTrip(Declaration.parser(""),
"""match x:
  Bar(_, _):
    10""")

    roundTrip(Declaration.parser(""),
"""match x:
  Bar(_, _):
      if True: 0
      else: 10""")

    roundTrip(Declaration.parser(""),
"""match x:
  Bar(_, _):
      if True: 0
      else: 10""")

    roundTrip(Declaration.parser(""),
"""match x:
  []: 0
  [x]: 1
  _: 2""")

    roundTrip(Declaration.parser(""),
"""Foo(x) = bar
x""")

    roundTrip(Declaration.parser(""),
"""match x:
  Some(_) | None: 1""")

    roundTrip(Declaration.parser(""),
"""match x:
  Some(_) | None: 1
  y: y
  [x | y, _]: z""")


    roundTrip(Declaration.parser(""),
"""Foo(x) | Bar(x) = bar
x""")
  }

  test("we can parse declaration lists") {
    roundTrip(Declaration.parser(""), "[]")
    roundTrip(Declaration.parser(""), "[1]")
    roundTrip(Declaration.parser(""), "[1, 2, 3]")
    roundTrip(Declaration.parser(""), "[1, *x, 3]")
    roundTrip(Declaration.parser(""), "[Foo(a, b), *acc]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser), "[foo(a, b)]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser), "[x for y in z]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser), "[x for (y, z) in w]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser), "[x for (y, z) in w if w1]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser), "[*x for y in z]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser), "[*x for (y, z) in w]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser), "[*x for (y, z) in w if w1]")
    roundTrip(ListLang.parser(Declaration.parser(""), Pattern.matchParser),
      "[x for x in range(4) if x.eq_Int(2)]")
    roundTrip(ListLang.SpliceOrItem.parser(Declaration.parser("")), "a")
    roundTrip(ListLang.SpliceOrItem.parser(Declaration.parser("")), "foo(a, b)")
    roundTrip(ListLang.SpliceOrItem.parser(Declaration.parser("")), "*foo(a, b)")
    roundTrip(Declaration.parser(""), "[x for y in [1, 2]]")
    roundTrip(Declaration.parser(""), "[x for y in [1, 2] if foo]")
  }

  test("we can parse any Declaration") {
    forAll(Generators.genDeclaration(5))(law(Declaration.parser("")))
  }

  test("we can parse any Statement") {
    forAll(Generators.genStatement(5))(law(Statement.parser))

    roundTrip(Statement.parser,
"""#
def foo(x): x""")

    roundTrip(Statement.parser,
"""#
def foo(x):
  x""")

    roundTrip(Statement.parser,
"""#
operator + = plus

x = 1+2
""")

    roundTrip(Statement.parser,
"""# header
y = if eq_Int(x, 2):
  True
else:
  False

def foo(x: Integer, y: String) -> String:
  toString(x).append_str(y)

# here is a lambda
fn = \x, y -> x.plus(y)

x = ( foo )

""")

    roundTrip(Statement.parser,
"""# header
def foo(x: forall f. f[a] -> f[b], y: a) -> b:
  x(y)

# here is a lambda
fn = \x, y -> x.plus(y)

x = ( foo )

""")

    roundTrip(Statement.parser,
"""#

x = Pair([], b)
""")

    roundTrip(Statement.parser,
"""#

Pair(x, _) = Pair([], b)
""")

    roundTrip(Statement.parser,
"""# MONADS!!!!
struct Monad(pure: forall a. a -> f[a], flatMap: forall a, b. f[a] -> (a -> f[b]) -> f[b])
""")

    // we can put new-lines in structs
    roundTrip(Statement.parser,
"""# MONADS!!!!
struct Monad(
  pure: forall a. a -> f[a],
  flatMap: forall a, b. f[a] -> (a -> f[b]) -> f[b])
""")

    // we can put type params in
    roundTrip(Statement.parser,
"""# MONADS!!!!
struct Monad[f](
  pure: forall a. a -> f[a],
  flatMap: forall a, b. f[a] -> (a -> f[b]) -> f[b])
""")

    // we can put new-lines in defs
    roundTrip(Statement.parser,
"""#
def foo(
  x,
  y: Int): x.add(y)
""")

    roundTrip(Statement.parser, """enum Option: None, Some(a)""")

    roundTrip(Statement.parser, """enum Option[a]: None, Some(a: a)""")

    roundTrip(Statement.parser,
"""enum Option:
  None
  Some(a)""")

    roundTrip(Statement.parser,
"""enum Option[a]:
  None
  Some(a: a)""")

    roundTrip(Statement.parser,
"""enum Option:
  None, Some(a)""")
  }

  test("Any statement may append a newline and continue to parse") {
    forAll(Generators.genStatement(5)) {
      case Statement.EndOfFile => ()
      case s =>
        val str = Document[Statement].document(s).render(80) + "\n"
        roundTrip(Statement.parser, str)
    }
  }

  test("Any statement ending in a newline may have it removed and continue to parse") {
    forAll(Generators.genStatement(5)) { s =>
      val str = Document[Statement].document(s).render(80)

      roundTrip(Statement.parser, str.reverse.dropWhile(_ == '\n').reverse)
    }
  }

  test("Any declaration may append any whitespace and optionally a comma and parse") {
    forAll(Generators.genDeclaration(5), Gen.listOf(Gen.oneOf(' ', '\t')).map(_.mkString), Gen.oneOf(true, false)) {
      case (s, ws, comma) =>
        val str = Document[Declaration].document(s).render(80) + ws + (if (comma) "," else "")
        roundTrip(Declaration.parser(""), str, lax = true)
    }
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

    forAll(Generators.packageGen(5))(law(Package.parser))
  }

  test("parse errors point near where they occur") {
    expectFail(Statement.parser,
      """x = 1
z = 3
z = 4
y = {'x': 'x' : 'y'}
""", 18)
  }

}
