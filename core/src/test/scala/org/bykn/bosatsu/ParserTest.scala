package org.bykn.bosatsu

import cats.data.NonEmptyList
import Parser.Combinators
import java.math.BigInteger
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.typelevel.paiges.{Doc, Document}

import cats.implicits._
import cats.parse.{Parser0 => P0, Parser => P}
import Parser.{optionParse, unsafeParse, Indy}

import Generators.{shrinkDecl, shrinkStmt, genCodePoints}
import org.scalatest.funsuite.AnyFunSuite

trait ParseFns {
  def region(s0: String, idx: Int): String =
    if (s0.isEmpty) s"empty string, idx = $idx"
    else if (s0.length == idx) {
      val s = s0 + "*"
      ("...(" + s.drop(idx - 20).take(20) + ")...")
    } else {
      val s = s0.updated(idx, '*')
      ("...(" + s.drop(idx - 20).take(30) + ")...")
    }

  @annotation.tailrec
  final def firstDiff(s1: String, s2: String): String =
    if (s1 == s2) ""
    else if (s1.isEmpty) s2
    else if (s2.isEmpty) s1
    else if (s1(0) == s2(0)) firstDiff(s1.tail, s2.tail)
    else
      s"${s1(0).toInt}: ${s1.take(20)}... != ${s2(0).toInt}: ${s2.take(20)}..."

}

object TestParseUtils extends ParseFns

abstract class ParserTestBase extends AnyFunSuite with ParseFns {

  // This is so we can make Declarations without the region
  implicit protected val emptyRegion: Region = Region(0, 0)

  def parseTest[T](p: P0[T], str: String, expected: T, exidx: Int) =
    p.parse(str) match {
      case Right((rest, t)) =>
        val idx = if (rest == "") str.length else str.indexOf(rest)
        lazy val message = firstDiff(t.toString, expected.toString)
        assert(
          t == expected,
          s"difference: $message, input syntax:\n\n\n$str\n\n"
        )
        assert(idx == exidx)
      case Left(err) =>
        val idx = err.failedAtOffset
        fail(
          s"failed to parse: $str: at $idx in region ${region(str, idx)} with err: ${err}"
        )
    }

  def parseTestAll[T](p: P0[T], str: String, expected: T) =
    parseTest(p, str, expected, str.length)

  def roundTrip[T: Document](p: P0[T], str: String, lax: Boolean = false) =
    p.parse(str) match {
      case Right((rest, t)) =>
        val idx = if (rest == "") str.length else str.indexOf(rest)
        if (!lax) {
          assert(idx == str.length, s"parsed: $t from: $str")
        }
        val tstr = Document[T].document(t).render(80)
        p.parse(tstr) match {
          case Right((_, t1)) =>
            assert(t1 == t)
          case Left(err) =>
            val idx = err.failedAtOffset
            val diff = firstDiff(str, tstr)
            fail(
              s"Diff: $diff.\nfailed to reparse: $tstr: $idx in region ${region(tstr, idx)} with err: ${err}"
            )
        }
      case Left(err) =>
        val idx = err.failedAtOffset
        fail(
          s"failed to parse: $str: $idx in region ${region(str, idx)} with err: ${err}"
        )
    }

  def roundTripExact[T: Document](p: P0[T], str: String) =
    p.parse(str) match {
      case Right((rest, t)) =>
        val idx = if (rest == "") str.length else str.indexOf(rest)
        assert(idx == str.length, s"parsed: $t from: $str")
        val tstr = Document[T].document(t).render(80)
        assert(tstr == str)
      case Left(err) =>
        val idx = err.failedAtOffset
        fail(
          s"failed to parse: $str: $idx in region ${region(str, idx)} with err: ${err}"
        )
    }

  def law[T: Document](p: P0[T])(t: T) = {
    val syntax = Document[T].document(t).render(80)
    parseTestAll(p, syntax, t)
  }

  def expectFail[T](p: P0[T], str: String, atIdx: Int) =
    p.parse(str) match {
      case Right((rest, t)) =>
        val idx = if (rest == "") str.length else str.indexOf(rest)
        fail(s"parsed $t to: $idx: ${region(str, idx)}")
      case Left(err) =>
        val idx = err.failedAtOffset
        def msg =
          s"failed to parse: $str: at $idx in region ${region(str, idx)} with err: ${err}"
        assert(idx == atIdx, msg)
    }

  def config: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful =
      if (Platform.isScalaJvm) 300 else 10
    )
}

class ParserTest extends ParserTestBase {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration = config

  test("we can parse integers") {
    forAll { (b: BigInt) =>
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
          else
            for {
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
          case Left(idx)   =>
            fail(s"failed at idx: $idx in $str: ${region(str, idx)}")
        }
      } catch {
        case t: Throwable =>
          fail(s"failed to decode: $str1 from $str, exception: $t")
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
    forAll { (s: String) => Parser.unescape(s); succeed }
    // more brutal tests
    forAll { (s: String) =>
      val prefixes = List('x', 'o', 'u', 'U').map(c => s"\\$c")
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
    parseTestAll(Parser.escapedString('\''), "'\\o44'", "$")
    parseTestAll(Parser.escapedString('\''), "'\\x24'", "$")
    parseTestAll(Parser.escapedString('\''), "'\\u0024'", "$")
    parseTestAll(Parser.escapedString('\''), "'\\U00000024'", "$")
    parseTestAll(Parser.escapedString('"'), "\"\"", "")
    parseTestAll(Parser.escapedString('\''), "'foo\\tbar'", "foo\tbar")
    parseTestAll(Parser.escapedString('\''), "'foo\tbar'", "foo\tbar")

    val regressions = List(("'", '\''))

    regressions.foreach { case (s, c) => law(s, c) }
  }

  test("we can parse interpolated strings") {
    def singleq(str1: String, res: List[Either[Json, String]]) =
      parseTestAll(
        StringUtil
          .interpolatedString(
            '\'',
            P.string("${").as((j: Json) => j),
            Json.parser,
            P.char('}')
          )
          .map(_.map {
            case Right((_, str)) => Right(str)
            case Left(l)         => Left(l)
          }),
        str1,
        res
      )

    // scala complains about things that look like interpolation strings that aren't interpolated
    val dollar = '$'.toString
    singleq("''", List())
    singleq("'foo\\\\qbar'", List(Right("foo\\qbar")))
    singleq("'foo\tbar'", List(Right("foo\tbar")))
    singleq(s"'foo\\$dollar{bar}'", List(Right(s"foo$dollar{bar}")))
    // foo$bar is okay, it is only foo${bar} that needs to be escaped
    singleq(s"'foo${dollar}bar'", List(Right(s"foo${dollar}bar")))
    singleq(
      s"'foo$dollar{42}'",
      List(Right("foo"), Left(Json.JNumberStr("42")))
    )
    singleq(s"'$dollar{42}'", List(Left(Json.JNumberStr("42"))))
    singleq(
      s"'$dollar{42}bar'",
      List(Left(Json.JNumberStr("42")), Right("bar"))
    )
  }

  test("we can decode any utf16") {
    val p =
      StringUtil.utf16Codepoint.repAs(StringUtil.codePointAccumulator) | P.pure(
        ""
      )

    forAll(Gen.listOf(genCodePoints)) { cps =>
      val strbuilder = new java.lang.StringBuilder
      cps.foreach(strbuilder.appendCodePoint(_))
      val str = strbuilder.toString
      val hex = cps.map(_.toHexString)

      val parsed = p.parseAll(str)
      assert(parsed == Right(str))

      assert(
        parsed.map(StringUtil.codePoints) == Right(cps),
        s"hex = $hex, str = ${StringUtil.codePoints(str)} utf16 = ${str.toCharArray().toList.map(_.toInt.toHexString)}"
      )
    }
  }

  test("Identifier round trips") {
    forAll(Generators.identifierGen)(law(Identifier.parser))

    val examples =
      List("foo", "`bar`", "`bar foo`", "`with \\`internal`", "operator +")

    examples.foreach(roundTrip(Identifier.parser, _))
  }

  test("we can append to an identifier and parse it") {
    forAll(Generators.bindIdentGen, Arbitrary.arbitrary[String]) { (b, str) =>
      val i1 = Identifier.appendToName(b, str)

      val src = i1.sourceCodeRepr
      assert(Identifier.unsafe(src).sourceCodeRepr == src)
    }
  }

  test("we can parse lists") {
    forAll { (ls: List[Long], spaceCnt0: Int) =>
      val spaceCount = spaceCnt0 & 7
      val str0 = ls.toString
      val str = str0.flatMap {
        case ',' => "," + (" " * spaceCount)
        case c   => c.toString
      }

      val listOfStr: P[List[String]] =
        P.string("List(") *>
          Parser.integerString.nonEmptyList
            .map(_.toList)
            .orElse(P.pure(Nil)) <*
          P.char(')')

      parseTestAll(listOfStr, str, ls.map(_.toString))
    }
  }

  test("we can parse dicts") {
    val strDict = Parser.dictLikeParser(
      Parser.escapedString('\''),
      Parser.escapedString('\'')
    )
    parseTestAll(strDict, "{}", Nil)
    parseTestAll(strDict, "{'a': 'b'}", List(("a", "b")))
    parseTestAll(strDict, "{ 'a' : 'b' }", List(("a", "b")))
    parseTestAll(strDict, "{'a' : 'b', 'c': 'd'}", List(("a", "b"), ("c", "d")))
    parseTestAll(
      strDict,
      "{'a' : 'b',\n'c': 'd'}",
      List(("a", "b"), ("c", "d"))
    )
    parseTestAll(
      strDict,
      "{'a' : 'b',\n\t'c': 'd'}",
      List(("a", "b"), ("c", "d"))
    )
    parseTestAll(
      strDict,
      "{'a' : 'b',\n  'c': 'd'}",
      List(("a", "b"), ("c", "d"))
    )

    case class WildDict(
        stringRepNoCurlies: List[String],
        original: List[(String, String)]
    ) {
      def stringRep: String = stringRepNoCurlies.mkString("{", "", "}")

      def addEntry(strings: List[String], k: String, v: String): WildDict =
        if (stringRepNoCurlies.isEmpty) WildDict(strings, (k, v) :: original)
        else
          WildDict(strings ::: ("," :: stringRepNoCurlies), (k, v) :: original)
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

  test("we can parse RecordConstructors") {
    def check(str: String) =
      roundTrip[Declaration](
        Declaration.recordConstructorP(
          "",
          Declaration.varP,
          Declaration.varP.orElse(Declaration.lits)
        ),
        str
      )

    check("Foo { bar }")
    check("Foo{bar}")
    check("Foo(bar)")
    check("Foo(bar\n)")
    check("Foo {   bar   }")
    check("Foo {\nbar\n}")

    check("Foo { bar: baz }")
    check("Foo{bar:baz}")
    check("Foo {   bar : baz   }")
    check("Foo {\nbar:\n\tbaz}")
    check("Foo {\nbar:\n\n\tbaz}")

    check("Foo { bar, baz }")
    check("Foo{bar,baz}")
    check("Foo(bar,baz)")
    check("Foo {   bar , baz  }")
    check("Foo {\nbar,\n baz}")
    check("Foo {\nbar\n, baz}")

    check("Foo { bar, baz: 42 }")
    check("Foo{bar,baz:42}")
    check("Foo {   bar , baz : 42  }")
    check("Foo {\nbar,\n baz:\n 42}")
    check("Foo {\nbar\n, baz\n:\t42}")

    check("Foo { bar: baz, quux: 42 }")
    check("Foo{bar:baz,quux:42}")
    check("Foo {   bar : baz , quux : 42  }")
    check("Foo {\nbar:\n\tbaz, quux:\n\t42\n\t}")
    check("Foo {\nbar:\n\n\tbaz,\nquux\n:\n42\n}")

    check("Foo{x:1}")
    // from scalacheck
    // check("Ze8lujlrbo {wlqOvp: {}}")
  }

  test("we can parse tuples") {
    forAll { (ls: List[Long], spaceCnt0: Int) =>
      val spaceCount = spaceCnt0 & 7
      val pad = " " * spaceCount
      val str =
        ls match {
          case h :: Nil => s"($h,$pad)"
          case _        =>
            ls.mkString("(", "," + pad, ")")
        }
      parseTestAll(
        Parser.integerString.tupleOrParens,
        str,
        Right(ls.map(_.toString))
      )
    }

    // a single item is parsed as parens
    forAll { (it: Long, spaceCnt0: Int) =>
      val spaceCount = spaceCnt0 & 7
      val pad = " " * spaceCount
      val str = s"($it$pad)"
      parseTestAll(Parser.integerString.tupleOrParens, str, Left(it.toString))
    }
  }

  test("we can parse blocks") {
    val indy =
      OptIndent.block(Indy.lift(P.string("if foo")), Indy.lift(P.string("bar")))
    val p = indy.run("")
    parseTestAll(p, "if foo: bar", ((), OptIndent.same(())))
    parseTestAll(p, "if foo:\n\tbar", ((), OptIndent.paddedIndented(1, 4, ())))
    parseTestAll(
      p,
      "if foo:\n    bar",
      ((), OptIndent.paddedIndented(1, 4, ()))
    )
    parseTestAll(p, "if foo:\n  bar", ((), OptIndent.paddedIndented(1, 2, ())))

    import Indy.IndyMethods
    val repeated = indy.nonEmptyList(Indy.lift(Parser.toEOL1))

    val single = ((), OptIndent.notSame(Padding(1, Indented(2, ()))))
    parseTestAll(
      repeated.run(""),
      "if foo:\n  bar\nif foo:\n  bar",
      NonEmptyList.of(single, single)
    )

    // we can nest blocks
    parseTestAll(
      OptIndent.block(Indy.lift(P.string("nest")), indy)(""),
      "nest: if foo: bar",
      ((), OptIndent.same(((), OptIndent.same(()))))
    )
    parseTestAll(
      OptIndent.block(Indy.lift(P.string("nest")), indy)(""),
      "nest:\n  if foo: bar",
      ((), OptIndent.paddedIndented(1, 2, ((), OptIndent.same(()))))
    )
    parseTestAll(
      OptIndent.block(Indy.lift(P.string("nest")), indy)(""),
      "nest:\n  if foo:\n    bar",
      (
        (),
        OptIndent.paddedIndented(1, 2, ((), OptIndent.paddedIndented(1, 2, ())))
      )
    )

    val simpleBlock = OptIndent
      .block(
        Indy.lift(Parser.lowerIdent <* Parser.maybeSpace),
        Indy.lift(Parser.lowerIdent)
      )
      .nonEmptyList(Indy.toEOLIndent)

    val sbRes = NonEmptyList.of(
      ("x1", OptIndent.paddedIndented(1, 2, "x2")),
      ("y1", OptIndent.paddedIndented(1, 3, "y2"))
    )
    parseTestAll(simpleBlock(""), "x1:\n  x2\ny1:\n   y2", sbRes)

    parseTestAll(
      OptIndent.block(Indy.lift(Parser.lowerIdent), simpleBlock)(""),
      "block:\n  x1:\n    x2\n  y1:\n     y2",
      ("block", OptIndent.paddedIndented(1, 2, sbRes))
    )
  }

  def trName(s: String): TypeRef.TypeName =
    TypeRef.TypeName(TypeName(Identifier.Constructor(s)))

  test("we can parse TypeRefs") {
    parseTestAll(TypeRef.parser, "foo", TypeRef.TypeVar("foo"))
    parseTestAll(TypeRef.parser, "Foo", trName("Foo"))

    parseTestAll(
      TypeRef.parser,
      "forall a. a",
      TypeRef.TypeForAll(
        NonEmptyList.of((TypeRef.TypeVar("a"), None)),
        TypeRef.TypeVar("a")
      )
    )
    parseTestAll(
      TypeRef.parser,
      "forall a, b. f[a] -> f[b]",
      TypeRef.TypeForAll(
        NonEmptyList
          .of((TypeRef.TypeVar("a"), None), (TypeRef.TypeVar("b"), None)),
        TypeRef.TypeArrow(
          TypeRef.TypeApply(
            TypeRef.TypeVar("f"),
            NonEmptyList.of(TypeRef.TypeVar("a"))
          ),
          TypeRef.TypeApply(
            TypeRef.TypeVar("f"),
            NonEmptyList.of(TypeRef.TypeVar("b"))
          )
        )
      )
    )
    roundTrip(TypeRef.parser, "forall a, b. f[a] -> f[b]")
    roundTrip(TypeRef.parser, "(forall a, b. f[a]) -> f[b]")
    roundTrip(TypeRef.parser, "(forall a, b. f[a])[Int]") // apply a type

    parseTestAll(
      TypeRef.parser,
      "Foo -> Bar",
      TypeRef.TypeArrow(trName("Foo"), trName("Bar"))
    )
    parseTestAll(
      TypeRef.parser,
      "Foo -> Bar -> baz",
      TypeRef.TypeArrow(
        trName("Foo"),
        TypeRef.TypeArrow(trName("Bar"), TypeRef.TypeVar("baz"))
      )
    )
    parseTestAll(
      TypeRef.parser,
      "(Foo -> Bar) -> baz",
      TypeRef.TypeArrow(
        TypeRef.TypeArrow(trName("Foo"), trName("Bar")),
        TypeRef.TypeVar("baz")
      )
    )
    parseTestAll(
      TypeRef.parser,
      "Foo[Bar]",
      TypeRef.TypeApply(trName("Foo"), NonEmptyList.of(trName("Bar")))
    )

    forAll(Generators.typeRefGen) { tref =>
      parseTestAll(TypeRef.parser, tref.toDoc.render(80), tref)
    }
  }

  test("we can parse fully pathed types") {
    import rankn.Type
    import Type._

    def check(s: String, t: Type) = parseTestAll(Type.fullyResolvedParser, s, t)

    val varA = TyVar(Var.Bound("a"))
    val varB = TyVar(Var.Bound("b"))
    val FooBarBar = TyConst(
      Const.Defined(
        PackageName.parts("Foo", "Bar"),
        TypeName(Identifier.Constructor("Bar"))
      )
    )
    check("a", varA)
    check("Foo/Bar::Bar", FooBarBar)
    check("a -> Foo/Bar::Bar", Fun(varA, FooBarBar))
    check(
      "forall a, b. Foo/Bar::Bar[a, b]",
      Type.forAll(
        List((Var.Bound("a"), Kind.Type), (Var.Bound("b"), Kind.Type)),
        TyApply(TyApply(FooBarBar, varA), varB)
      )
    )
    check(
      "forall a. forall b. Foo/Bar::Bar[a, b]",
      Type.forAll(
        List((Var.Bound("a"), Kind.Type), (Var.Bound("b"), Kind.Type)),
        TyApply(TyApply(FooBarBar, varA), varB)
      )
    )
    check("(a)", varA)
    check("(a, b)", Tuple(List(varA, varB)))
  }

  test("we can parse python style list expressions") {
    val pident = Parser.lowerIdent
    implicit val stringDoc: Document[String] =
      Document.instance[String](Doc.text(_))

    val llp = ListLang.parser(pident, pident, pident)
    roundTrip(llp, "[a]")
    roundTrip(llp, "[\n    a\n  ]")
    roundTrip(llp, "[]")
    roundTrip(llp, "[  ]")
    roundTrip(llp, "[a , b]")
    roundTrip(llp, "[a , b]")
    roundTrip(llp, "[a , *b]")
    roundTrip(llp, "[a ,\n*b,\n c]")
    roundTrip(llp, "[  a ,\n*b ,  \n      c]")
    roundTrip(llp, "[x for y in z]")
    roundTrip(llp, "[x for y in z if w]")
    roundTrip(ListLang.SpliceOrItem.parser(pident), "a")
    roundTrip(ListLang.SpliceOrItem.parser(pident), "*a")
  }

  test("we can parse operators") {
    val singleToks = List(
      "+",
      "-",
      "*",
      "!",
      "$",
      "%",
      "^",
      "&",
      "*",
      "|",
      "?",
      "/",
      "<",
      ">",
      "~"
    )
    val withEq = "=" :: singleToks

    val allLen2 = (withEq, withEq).mapN(_ + _)
    val allLen3 = (allLen2, withEq).mapN(_ + _)

    (singleToks ::: allLen2 ::: allLen3).foreach { opStr =>
      if ((opStr != "<-") && (opStr != "->")) {
        roundTrip(Operators.operatorToken, opStr)
      }
    }

    expectFail(Operators.operatorToken, "=", 1)
    expectFail(Operators.operatorToken, "<-", 2)
  }

  test("test import statements") {
    roundTrip(Import.parser, "from Foo import bar, baz")
    roundTrip(Import.parser, "from Foo import bar as quux, baz")
    roundTrip(Import.parser, "from Foo import bar as quux, baz")
    roundTrip(Import.parser, "from Foo import (\nbar as quux,\nbaz)")
  }
}

/** This is a separate class since some of these are very slow
  */
class SyntaxParseTest extends ParserTestBase {

  implicit val generatorDrivenConfig: PropertyCheckConfiguration = config

  def mkVar(n: String): Declaration.Var =
    Declaration.Var(Identifier.Name(n))

  test("we can parse comments") {
    val gen =
      Generators.commentGen(Generators.padding(Generators.genDeclaration(0), 1))
    forAll(gen) { comment =>
      parseTestAll(
        CommentStatement
          .parser(i => Padding.parser(Declaration.parser(i)))
          .run(""),
        Document[CommentStatement[Padding[Declaration]]]
          .document(comment)
          .render(80),
        comment
      )
    }

    val commentLit = """#foo
#bar

1"""
    parseTestAll(
      Declaration.parser(""),
      commentLit,
      Declaration.CommentNB(
        CommentStatement(
          NonEmptyList.of("foo", "bar"),
          Padding(1, Declaration.Literal(Lit.fromInt(1)))
        )
      )
    )

    val parensComment = """(#foo
#bar

1)"""
    parseTestAll(
      Declaration.parser(""),
      parensComment,
      Declaration.Parens(
        Declaration.CommentNB(
          CommentStatement(
            NonEmptyList.of("foo", "bar"),
            Padding(1, Declaration.Literal(Lit.fromInt(1)))
          )
        )
      )
    )
  }

  test("we can parse Lit.Integer") {
    forAll { (bi: BigInt) =>
      roundTrip(Lit.parser, bi.toString)
    }
  }

  test("we can parse DefStatement") {
    forAll(
      Generators.defGen(Generators.optIndent(Generators.genDeclaration(0)))
    ) { defn =>
      parseTestAll[DefStatement[Pattern.Parsed, OptIndent[Declaration]]](
        DefStatement.parser(
          Pattern.bindParser,
          Parser.maybeSpace.with1 *> OptIndent.indy(Declaration.parser).run("")
        ),
        Document[DefStatement[Pattern.Parsed, OptIndent[Declaration]]]
          .document(defn)
          .render(80),
        defn
      )
    }

    val defWithComment = """def foo(a):
  # comment here
  a
foo"""
    parseTestAll(
      Declaration.parser(""),
      defWithComment,
      Declaration.DefFn(
        DefStatement(
          Identifier.Name("foo"),
          None,
          NonEmptyList.one(NonEmptyList.one(Pattern.Var(Identifier.Name("a")))),
          None,
          (
            OptIndent.paddedIndented(
              1,
              2,
              Declaration.CommentNB(
                CommentStatement(
                  NonEmptyList.of(" comment here"),
                  Padding(0, mkVar("a"))
                )
              )
            ),
            Padding(0, mkVar("foo"))
          )
        )
      )
    )

    roundTrip(Declaration.parser(""), defWithComment)

    // Here is a pretty brutal randomly generated case
    roundTrip(
      Declaration.parser(""),
      """def uwr(dw: h6lmZhgg) -> forall lnNR. Z5syis -> Mhgm:
  -349743008

foo"""
    )

  }

  test("we can parse BindingStatement") {
    val dp = Declaration.parser("")
    parseTestAll(
      dp,
      """foo = 5

5""",
      Declaration.Binding(
        BindingStatement(
          Pattern.Var(Identifier.Name("foo")),
          Declaration.Literal(Lit.fromInt(5)),
          Padding(1, Declaration.Literal(Lit.fromInt(5)))
        )
      )
    )

    roundTrip(
      dp,
      """#
Pair(_, x) = z
x"""
    )
  }

  test("we can parse any Apply") {
    import Declaration._

    import ApplyKind.{Dot => ADot, Parens => AParens}

    parseTestAll(
      parser(""),
      "x(f)",
      Apply(mkVar("x"), NonEmptyList.of(mkVar("f")), AParens)
    )

    parseTestAll(
      parser(""),
      "f.x()",
      Apply(mkVar("x"), NonEmptyList.of(mkVar("f")), ADot)
    )

    parseTestAll(
      parser(""),
      "f(foo).x()",
      Apply(
        mkVar("x"),
        NonEmptyList.of(
          Apply(mkVar("f"), NonEmptyList.of(mkVar("foo")), AParens)
        ),
        ADot
      )
    )

    parseTestAll(
      parser(""),
      "f.foo(x)", // foo(f, x)
      Apply(mkVar("foo"), NonEmptyList.of(mkVar("f"), mkVar("x")), ADot)
    )

    parseTestAll(
      parser(""),
      "(\\x -> x)(f)",
      Apply(
        Parens(
          Lambda(NonEmptyList.of(Pattern.Var(Identifier.Name("x"))), mkVar("x"))
        ),
        NonEmptyList.of(mkVar("f")),
        AParens
      )
    )

    parseTestAll(
      parser(""),
      "((\\x -> x)(f))",
      Parens(
        Apply(
          Parens(
            Lambda(
              NonEmptyList.of(Pattern.Var(Identifier.Name("x"))),
              mkVar("x")
            )
          ),
          NonEmptyList.of(mkVar("f")),
          AParens
        )
      )
    )

    // bare lambda
    parseTestAll(
      parser(""),
      "((x -> x)(f))",
      Parens(
        Apply(
          Parens(
            Lambda(
              NonEmptyList.of(Pattern.Var(Identifier.Name("x"))),
              mkVar("x")
            )
          ),
          NonEmptyList.of(mkVar("f")),
          AParens
        )
      )
    )

    val expected = Apply(
      Parens(
        Parens(
          Lambda(NonEmptyList.of(Pattern.Var(Identifier.Name("x"))), mkVar("x"))
        )
      ),
      NonEmptyList.of(mkVar("f")),
      AParens
    )
    parseTestAll(parser(""), "((\\x -> x))(f)", expected)

    parseTestAll(parser(""), expected.toDoc.render(80), expected)

  }

  test("bare style lambdas work") {
    roundTrip(Declaration.parser(""), "() -> 1")
    roundTrip(Declaration.parser(""), "(x, y) -> x.add(y)")
    roundTrip(Declaration.parser(""), "Foo(x, y) -> x.add(y)")
    roundTrip(Declaration.parser(""), "((x: Int, y: Int)) -> x.add(y)")
  }

  test("we can parse patterns") {
    roundTrip(Pattern.matchParser, "Foo([])")
    roundTrip(Pattern.matchParser, "Foo([], bar)")
    roundTrip(Pattern.matchParser, "Foo(...)")
    roundTrip(Pattern.matchParser, "Foo(a, ...)")
    roundTrip(Pattern.matchParser, "Foo { a: 12, b: 3, c }")
    roundTrip(Pattern.matchParser, "Foo { a: 12, b: 3, ... }")
    roundTrip(Pattern.matchParser, "Foo{a: 12,b: 3,c}")
    roundTrip(Pattern.matchParser, "Foo{a: 12,b: 3,...}")
    roundTrip(Pattern.matchParser, "Foo{a}")
    roundTrip(Pattern.matchParser, "Foo { a }")
    roundTrip(Pattern.matchParser, "x")
    roundTrip(Pattern.matchParser, "_")
    roundTrip(Pattern.matchParser, "(a, b)")
    roundTrip(Pattern.matchParser, "(a, b) | _")
    roundTrip(Pattern.matchParser, "_ as foo")
    roundTrip(Pattern.matchParser, "Some(_) as foo | None")
    roundTrip(Pattern.matchParser, "Bar | Some(_) as foo | None")
    roundTrip(Pattern.matchParser, """"foo${bar}$.{codepoint}"""")
    roundTrip(Pattern.bindParser, "x: Int")

    implicit def docList[A: Document]: Document[NonEmptyList[A]] =
      Document.instance[NonEmptyList[A]] { nes =>
        Doc.intercalate(Doc.text(", "), nes.toList.map(Document[A].document(_)))
      }
    roundTrip(Pattern.bindParser.nonEmptyList, "x: Int")
    roundTrip(Pattern.bindParser.nonEmptyList, "x: Int, y, Foo(z: Int)")
  }

  test("Declaration.toPattern works for all Pattern-like declarations") {
    def law1(dec: Declaration.NonBinding) =
      Declaration.toPattern(dec) match {
        case None      => fail("expected to convert to pattern")
        case Some(pat) =>
          // if we convert to string this parses the same as a pattern:
          val decStr = dec.toDoc.render(80)
          val parsePat = unsafeParse(Pattern.bindParser, decStr)
          assert(pat == parsePat)
      }
    forAll(Generators.patternDecl(4))(law1(_))

    val regressions = {
      import Declaration._
      import Identifier.{Name, Operator, Constructor}
      // this operator application can be a pattern
      List(
        ApplyOp(Var(Name("q")), Operator("|"), Var(Name("npzma"))),
        ApplyOp(
          Parens(
            ApplyOp(
              Parens(Literal(Lit.Str("igyimc"))),
              Operator("|"),
              Var(Name("ncf5Eo9"))
            )
          ),
          Operator("|"),
          Var(Constructor("K"))
        )
      )
    }

    regressions.foreach(law1(_))

    def law2(dec: Declaration.NonBinding) = {
      val decStr = dec.toDoc.render(80)
      val parsePat = optionParse(Pattern.matchParser, decStr)
      (Declaration.toPattern(dec), parsePat) match {
        case (None, None)         => succeed
        case (Some(p0), Some(p1)) => assert(p0 == p1)
        case (None, Some(_))      =>
          fail(s"toPattern failed, but parsed $decStr to: $parsePat")
        case (Some(p), None) =>
          fail(s"toPattern succeeded: $p but pattern parse failed")
      }
    }

    // for all Declarations, either it parses like a pattern or toPattern is None
    forAll(Generators.genNonBinding(5))(law2(_))
    regressions.foreach(law2(_))

    def testEqual(decl: String) = {
      val dec = unsafeParse(Declaration.parser(""), decl)
        .asInstanceOf[Declaration.NonBinding]
      val patt = unsafeParse(Pattern.matchParser, decl)
      Declaration.toPattern(dec) match {
        case Some(p2) => assert(p2 == patt)
        case None     => fail(s"could not convert $decl to pattern")
      }
    }

    testEqual("a")
    testEqual("Foo(a)")
    testEqual("[1, Foo, a]")
    testEqual("[*a, Foo([]), bar]")
  }

  test("we can parse bind") {
    import Declaration._

    parseTestAll(
      parser(""),
      """x = 4
x""",
      Binding(
        BindingStatement(
          Pattern.Var(Identifier.Name("x")),
          Literal(Lit.fromInt(4)),
          Padding(0, mkVar("x"))
        )
      )
    )

    parseTestAll(
      parser(""),
      """x = foo(4)

x""",
      Binding(
        BindingStatement(
          Pattern.Var(Identifier.Name("x")),
          Apply(
            mkVar("foo"),
            NonEmptyList.of(Literal(Lit.fromInt(4))),
            ApplyKind.Parens
          ),
          Padding(1, mkVar("x"))
        )
      )
    )

    parseTestAll(
      parser(""),
      """x = foo(4)
# x is really great
x""",
      Binding(
        BindingStatement(
          Pattern.Var(Identifier.Name("x")),
          Apply(
            mkVar("foo"),
            NonEmptyList.of(Literal(Lit.fromInt(4))),
            ApplyKind.Parens
          ),
          Padding(
            0,
            CommentNB(
              CommentStatement(
                NonEmptyList.of(" x is really great"),
                Padding(0, mkVar("x"))
              )
            )
          )
        )
      )
    )

    // allow indentation after =
    roundTrip(
      parser(""),
      """x =
        |  foo
        |x""".stripMargin
    )
  }

  test("we can parse if") {
    import Declaration._

    val liftVar = Parser.Indy.lift(varP: P[Declaration])
    val liftVar0 = Parser.Indy.lift(varP: P[NonBinding])
    val parser0 = ifElseP(liftVar0, liftVar)("")

    roundTrip[Declaration](
      parser0,
      """if w:
      x
else:
      y"""
    )

    roundTrip[Declaration](
      parser0,
      """if w:
        |     x
        |else:
        |     y""".stripMargin
    )

    roundTrip(
      parser(""),
      """if eq_Int(x, 3):
      x
else:
      y"""
    )

    expectFail(
      parser0,
      """if x:
      x
else
      y""",
      18
    )

    expectFail(
      parser0,
      """if x: x
else y""",
      13
    )

    expectFail(
      parser(""),
      """if x:
      x
else
      y""",
      18
    )

    expectFail(
      parser(""),
      """if x: x
else y""",
      13
    )

    expectFail(
      parser(""),
      """if f: 0
else 1""",
      13
    )

    roundTrip(
      parser(""),
      """if eq_Int(x, 3):
      x
elif foo:
   z
else:
      y"""
    )

    roundTrip[Declaration](
      parser0,
      """if w: x
else: y"""
    )
    roundTrip(
      parser(""),
      """if eq_Int(x, 3): x
else: y"""
    )

    roundTrip(
      parser(""),
      """if eq_Int(x, 3): x
elif foo:
      z
else: y"""
    )
  }

  test("we can parse a match") {
    val liftVar = Parser.Indy.lift(Declaration.varP: P[Declaration])
    val liftVar0 = Parser.Indy.lift(Declaration.varP: P[Declaration.NonBinding])
    roundTrip[Declaration](
      Declaration.matchP(liftVar0, liftVar)(""),
      """match x:
  case y:
    z
  case w:
    r"""
    )
    roundTrip(
      Declaration.parser(""),
      """match 1:
  case Foo(a, b):
    a.plus(b)
  case Bar:
    42"""
    )
    roundTrip(
      Declaration.parser(""),
      """match 1:
  case (a, b):
    a.plus(b)
  case ():
    42"""
    )

    roundTrip(
      Declaration.parser(""),
      """match 1:
  case (a, (b, c)):
    a.plus(b).plus(e)
  case (1,):
    42"""
    )

    roundTrip(
      Declaration.parser(""),
      """match 1:
  case Foo(a, b):
    a.plus(b)
  case Bar:
    match x:
      case True:
        100
      case False:
        99"""
    )

    roundTrip(
      Declaration.parser(""),
      """foo(1, match 2:
  case Foo:

    foo
  case Bar:

    # this is the bar case
    bar, 100)"""
    )

    roundTrip(
      Declaration.parser(""),
      """if (match 2:
  case Foo:

    foo
  case Bar:

    # this is the bar case
    bar):
  1
else:
  2"""
    )

    roundTrip(
      Declaration.parser(""),
      """if True:
  match 1:
    case Foo(f):
      1
else:
  100"""
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
  case Bar(_, _):
    10"""
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
  case Bar(_, _):
      if True: 0
      else: 10"""
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
  case Bar(_, _):
      if True: 0
      else: 10"""
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
  case []: 0
  case [x]: 1
  case _: 2"""
    )

    roundTrip(
      Declaration.parser(""),
      """Foo(x) = bar
x"""
    )

    roundTrip(
      Declaration.parser(""),
      """Foo { x } = bar
x"""
    )

    roundTrip(
      Declaration.parser(""),
      """Foo { x } = Foo{x:1}
x"""
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
  case Some(_) | None: 1"""
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
  case Some(_) | None: 1
  case y: y
  case [x | y, _]: z"""
    )

    roundTrip(
      Declaration.parser(""),
      """Foo(x) | Bar(x) = bar
x"""
    )

    roundTrip(
      Declaration.parser(""),
      """(x: Int) = bar
x"""
    )
    roundTrip(
      Declaration.parser(""),
      """x: Int = bar
x"""
    )
  }

  test("we allow extra indentation on elif and else for better alignment") {

    roundTrip(
      Declaration.parser(""),
      """z = if w:
        |      x
        |    else:
        |      y
        |z""".stripMargin
    )

    roundTrip(
      Declaration.parser(""),
      """z = if w: x
        |    elif y: z
        |    else: quux
        |z""".stripMargin
    )
  }

  test("we can parse declaration lists") {

    val ll = ListLang.parser(
      Declaration.parser(""),
      Declaration.nonBindingParserNoTern(""),
      Pattern.matchParser
    )

    roundTrip(Declaration.parser(""), "[]")
    roundTrip(Declaration.parser(""), "[1]")
    roundTrip(Declaration.parser(""), "[1, 2, 3]")
    roundTrip(Declaration.parser(""), "[1, *x, 3]")
    roundTrip(Declaration.parser(""), "[Foo(a, b), *acc]")
    roundTrip(ll, "[foo(a, b)]")
    roundTrip(ll, "[x for y in z]")
    roundTrip(ll, "[x for (y, z) in w]")
    roundTrip(ll, "[x for (y, z) in w if w1]")
    roundTrip(ll, "[*x for y in z]")
    roundTrip(ll, "[*x for (y, z) in w]")
    roundTrip(ll, "[*x for (y, z) in w if w1]")
    roundTrip(ll, "[x for x in range(4) if x.eq_Int(2)]")
    roundTrip(ListLang.SpliceOrItem.parser(Declaration.parser("")), "a")
    roundTrip(ListLang.SpliceOrItem.parser(Declaration.parser("")), "foo(a, b)")
    roundTrip(
      ListLang.SpliceOrItem.parser(Declaration.parser("")),
      "*foo(a, b)"
    )
    roundTrip(Declaration.parser(""), "[x for y in [1, 2]]")
    roundTrip(Declaration.parser(""), "[x for y in [1, 2] if foo]")
  }

  test("we can parse any Declaration") {
    forAll(Generators.genDeclaration(5))(
      law(Declaration.parser("").map(_.replaceRegions(emptyRegion)))
    )

    def decl(s: String) = roundTrip(Declaration.parser(""), s)

    decl("x: Bar")
    decl("x :Bar")
    decl("x : Bar")
    decl("(x: Bar)")
    decl("(x :Bar)")
    decl("(x : Bar)")
    decl("y if y < z else q")
    decl("[x for x in xs if x < y ]")
    decl("[x for x in (xs if x < y else xy) ]")
    decl("y = [x for x in xs if x < y ]\ny")

    decl("x.f(y).g(z)")
    decl("""|x.f(y) \
            | .g(z)""".stripMargin)
    decl("""|x.f(y) \
            | .g(z) \
            | .h(w)""".stripMargin)
    decl("""|x \
            | .g(z) \
            | .h(w)""".stripMargin)
    decl("""|(y = 1
            |_ = y
            |2)""".stripMargin)

    decl("""|(y = 1
            |# just ignore y
            |_ = y
            |2)""".stripMargin)

    decl("""|[1,
            |  (# comment in a list
            |2)]""".stripMargin)

    decl("""|[1,
            |  # comment in a list
            | 2]""".stripMargin)

    decl("""|[1,
            |  # comment in a list
            |    # 1.5
            |  2]""".stripMargin)
  }

  test("we can parse any Statement") {
    forAll(Generators.genStatements(4, 10))(
      law(Statement.parser.map(_.map(_.replaceRegions(emptyRegion))))
    )

    roundTrip(
      Statement.parser,
      """#
def foo(x): x"""
    )

    roundTrip(
      Statement.parser,
      """#
def foo(x):
  x"""
    )

    roundTrip(
      Statement.parser,
      """#
operator + = plus

x = 1+2
"""
    )

    roundTrip(
      Statement.parser,
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

"""
    )

    roundTrip(
      Statement.parser,
      """# header
def foo(x: forall f. f[a] -> f[b], y: a) -> b:
  x(y)

# here is a lambda
fn = \x, y -> x.plus(y)

x = ( foo )

"""
    )

    // we can add spaces at the end of the file
    roundTrip(
      Statement.parser,
      """# header
def foo(x: forall f. f[a] -> f[b], y: a) -> b:
  x(y)

# here is a lambda
fn = \x, y -> x.plus(y)

x = ( foo )
      """
    )

    roundTrip(
      Statement.parser,
      """#

x = Pair([], b)
"""
    )

    roundTrip(
      Statement.parser,
      """#

Pair(x, _) = Pair([], b)
"""
    )

    roundTrip(
      Statement.parser,
      """# MONADS!!!!
struct Monad(pure: forall a. a -> f[a], flatMap: forall a, b. f[a] -> (a -> f[b]) -> f[b])
"""
    )

    // we can put new-lines in structs
    roundTrip(
      Statement.parser,
      """# MONADS!!!!
struct Monad(
  pure: forall a. a -> f[a],
  flatMap: forall a, b. f[a] -> (a -> f[b]) -> f[b])
"""
    )

    // we can put type params in
    roundTrip(
      Statement.parser,
      """# MONADS!!!!
struct Monad[f](
  pure: forall a. a -> f[a],
  flatMap: forall a, b. f[a] -> (a -> f[b]) -> f[b])
"""
    )

    // we can put new-lines in defs
    roundTrip(
      Statement.parser,
      """#
def foo(
  x,
  y: Int): x.add(y)
"""
    )

    roundTrip(Statement.parser, """enum Option: None, Some(a)""")

    roundTrip(Statement.parser, """enum Option[a]: None, Some(a: a)""")

    roundTrip(
      Statement.parser,
      """enum Option:
  None
  Some(a)"""
    )

    roundTrip(
      Statement.parser,
      """enum Option[a]:
  None
  Some(a: a)"""
    )

    roundTrip(
      Statement.parser,
      """enum Option:
  None, Some(a)"""
    )

    roundTripExact(
      Statement.parser,
      """def run(z):
  Err(y) | Good(y) = z
  y
"""
    )
  }

  def dropTrailingPadding(s: List[Statement]): List[Statement] =
    s.reverse.dropWhile {
      case Statement.PaddingStatement(_) => true
      case _                             => false
    }.reverse

  test("Any statement may append trailing whitespace and continue to parse") {
    forAll(Generators.genStatement(5), Generators.whiteSpace) { (s, ws) =>
      val str = Document[Statement].document(s).render(80) + ws
      roundTrip(Statement.parser.map(dropTrailingPadding(_)), str)
    }
  }

  test(
    "Any statement ending in a newline may have it removed and continue to parse"
  ) {
    forAll(Generators.genStatement(5)) { s =>
      val str = Document[Statement].document(s).render(80)

      roundTrip(
        Statement.parser.map(dropTrailingPadding(_)),
        str.reverse.dropWhile(_ == '\n').reverse
      )
    }
  }

  test(
    "Any declaration may append any whitespace and optionally a comma and parse"
  ) {
    forAll(
      Generators.genDeclaration(4),
      Gen.listOf(Gen.oneOf(' ', '\t')).map(_.mkString),
      Gen.oneOf(true, false)
    ) { case (s, ws, comma) =>
      val str =
        Document[Declaration].document(s).render(80) + ws + (if (comma) ","
                                                             else "")
      roundTrip(Declaration.parser(""), str, lax = true)
    }
  }

  test("parse external defs") {
    roundTrip(
      Statement.parser,
      """# header
external foo: String
"""
    )
    roundTrip(
      Statement.parser,
      """# header
external def foo(i: Integer) -> String
"""
    )
    roundTrip(
      Statement.parser,
      """# header
external def foo(i: Integer, b: a) -> String

external def foo2(i: Integer, b: a) -> String
"""
    )
    roundTrip(
      Statement.parser,
      """# header
external def foo[a](i: Integer, b: a) -> String
external def foo_co[a: +* -> *](i: Integer, b: a) -> String
"""
    )

  }

  test("we can parse any package") {
    roundTrip(
      Package.parser(None),
      """
# we can comment the package
package Foo/Bar
# comments are allowed
from Baz import Bippy
# even here
export foo # or here

foo = 1
"""
    )

    val pp = Package.parser(None).map { pack =>
      pack.copy(program = pack.program.map(_.replaceRegions(emptyRegion)))
    }
    forAll(Generators.packageGen(4))(law(pp))

    roundTripExact(
      Package.parser(None),
      """package Foo

enum Res[a, b]: Err(a: a), Good(a: a, b: b)

x = Err('good')

def run(z):
  Err(y) | Good(y, _) = z
  y

main = run(x)
"""
    )
  }

  test("parse errors point near where they occur") {
    expectFail(
      Statement.parser,
      """x = 1
z = 3
z = 4
y = {'x': 'x' : 'y'}
""",
      32
    )

    expectFail(
      Statement.parser,
      """x = 1
z = (
  x = 1
  x x)
""",
      24
    )

    expectFail(
      Statement.parser,
      """x = 1
z = (
  x = 1
  y = [1, 2, 3]
  x x)
""",
      40
    )

    expectFail(
      Statement.parser,
      """z = (
  if f: 0
  else 1)
""",
      23
    )

    expectFail(
      Package.parser(None),
      """package Foo
from Baz import a, , b

x = 1
""",
      31
    )

    expectFail(
      Package.parser(None),
      """package Foo
export x, , y

x = 1
""",
      22
    )

    expectFail(
      Package.parser(None),
      """package Foo
export x, ,

x = 1
""",
      22
    )
    expectFail(
      Package.parser(None),
      """package Foo

x = Foo(bar if bar)
""",
      31
    )

    expectFail(
      Package.parser(None),
      """package Foo

z = [x for x in xs if x < y else ]
""",
      41
    )
  }

  test("using parens to make blocks") {
    roundTrip(
      Package.parser(None),
      """package Foo

x = (
  y = 3
  y
)
""",
      lax = true
    )

    roundTrip(
      Package.parser(None),
      """package Foo

x = (
  # some pattern matching
  Foo(y, _) = foo
  y
)
""",
      lax = true
    )

    roundTrip(
      Package.parser(None),
      """package Foo

x = (
  # an if/else block
  if True: 1
  else: 0
)
""",
      lax = true
    )

    roundTrip(
      Package.parser(None),
      """package Foo

x = (
  def foo(x): x
  (
    foo(1)
  )
)
""",
      lax = true
    )

    roundTrip(
      Package.parser(None),
      """package Foo

x = (
  # here is foo
  def foo(x): x
  (
    # comment is okay
    foo(1)
  )
)
""",
      lax = true
    )

    roundTrip(
      Package.parser(None),
      """package Foo

x = ( y = 3
y
)
""",
      lax = true
    )
  }

  test("lambdas can have new lines") {

    roundTrip(
      Package.parser(None),
      """package Foo

x = z ->
  z
""",
      lax = true
    )

    roundTrip(
      Package.parser(None),
      """package Foo

x = z ->
  # we can comment here
  z
""",
      lax = true
    )
  }

  test("Parser.integerWithBase works") {
    case class Args(bi: BigInteger, asString: String, base: Int)
    def intersperse(str: String): Gen[String] =
      if (str.length <= 1) Gen.const(str)
      else {
        // 2 or longer:
        for {
          middle <- Gen.oneOf("", "_")
          tail <- intersperse(str.tail)
        } yield s"${str.head}$middle$tail"
      }
    val gen = for {
      (base, prefix0) <- Gen.oneOf((2, "0B"), (8, "0O"), (10, ""), (16, "0X"))
      prefix <- Gen.oneOf(prefix0, prefix0.toLowerCase)
      bi <- Gen.long.map(BigInteger.valueOf(_))
      biStr = bi.toString(base)
      withPrefix <-
        if (biStr(0) == '-') {
          intersperse(biStr.tail).map { t =>
            s"-$prefix$t"
          }
        } else intersperse(biStr).map(prefix + _)
    } yield Args(bi, withPrefix, base)

    forAll(gen) { case Args(bi, inBase, base) =>
      Parser.integerWithBase.parseAll(inBase) match {
        case Right((biP, b)) =>
          assert(biP == bi)
          assert(b == base)
        case Left(err) => fail(err.toString)
      }
    }
  }
}
