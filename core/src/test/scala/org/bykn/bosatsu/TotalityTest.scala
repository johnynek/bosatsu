package org.bykn.bosatsu

import cats.Eq
import cats.data.NonEmptyList
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalacheck.Gen

import org.bykn.bosatsu.pattern.{SetOps, SetOpsLaws}

import rankn._

import Parser.Combinators

import org.typelevel.paiges.Document

import Identifier.Constructor

import cats.implicits._

class TotalityTest
    extends SetOpsLaws[Pattern[(PackageName, Constructor), Type]] {
  type Pat = Pattern[(PackageName, Constructor), Type]

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful =
      if (Platform.isScalaJvm) 50000 else 100
    )
  // PropertyCheckConfiguration(minSuccessful = 50)

  val genPattern: Gen[Pattern[(PackageName, Constructor), Type]] =
    Generators.genCompiledPattern(5, useAnnotation = false)

  val genPatternNoUnion: Gen[Pattern[(PackageName, Constructor), Type]] =
    Generators.genCompiledPattern(5, useUnion = false, useAnnotation = false)

  def showPat(pat: Pattern[(PackageName, Constructor), Type]): String = {
    val pat0 = pat.mapName { case (_, n) =>
      Pattern.StructKind.Named(n, Pattern.StructKind.Style.TupleLike)
    }

    implicit val tdoc = Type.fullyResolvedDocument
    Document[Pattern[Pattern.StructKind, Type]].document(pat0).render(80)
  }

  def showPats(pats: List[Pattern[(PackageName, Constructor), Type]]): String =
    pats.map(showPat).toString

  def showPatU(pat: Pattern[(PackageName, Constructor), Type]): String =
    showPat(pat.unbind)

  def showPatsU(pats: List[Pattern[(PackageName, Constructor), Type]]): String =
    showPats(pats.map(_.unbind))

  def typeEnvOf(str: String): TypeEnv[Option[Kind.Arg]] =
    TestUtils.typeEnvOf(PackageName.PredefName, str)

  val predefTE = typeEnvOf("""#
struct Unit
struct TupleCons(fst, snd)
enum Bool: False, True
""")

  val setOps: SetOps[Pattern[(PackageName, Constructor), Type]] =
    TotalityCheck(predefTE).patternSetOps

  def genItem: Gen[Pattern[(PackageName, Constructor), Type]] =
    // TODO would be nice to pass with unions, they are hard
    genPatternNoUnion

  val eqPatterns: Eq[List[Pattern[(PackageName, Constructor), Type]]] =
    new Eq[List[Pattern[(PackageName, Constructor), Type]]] {
      val e1 = TotalityCheck(predefTE).eqPat

      def eqv(
          a: List[Pattern[(PackageName, Constructor), Type]],
          b: List[Pattern[(PackageName, Constructor), Type]]
      ) =
        (NonEmptyList.fromList(a), NonEmptyList.fromList(b)) match {
          case (oa, ob) if oa == ob => true
          case (Some(a), Some(b)) =>
            e1.eqv(Pattern.union(a.head, a.tail), Pattern.union(b.head, b.tail))
          case _ => false
        }
    }

  def eqUnion: Gen[Eq[List[Pattern[(PackageName, Constructor), Type]]]] =
    Gen.const(eqPatterns)

  def patterns(str: String): List[Pattern[(PackageName, Constructor), Type]] = {
    val nameToCons: Constructor => (PackageName, Constructor) = { cons =>
      (PackageName.PredefName, cons)
    }

    /** This is sufficient for these tests, but is not a full features pattern
      * compiler.
      */
    def parsedToExpr(
        pat: Pattern.Parsed
    ): Pattern[(PackageName, Constructor), rankn.Type] =
      pat
        .mapStruct[(PackageName, Constructor)] {
          case (Pattern.StructKind.Tuple, args) =>
            // this is a tuple pattern
            def loop(
                args: List[Pattern[(PackageName, Constructor), TypeRef]]
            ): Pattern[(PackageName, Constructor), TypeRef] =
              args match {
                case Nil =>
                  // ()
                  Pattern.PositionalStruct(
                    (PackageName.PredefName, Constructor("Unit")),
                    Nil
                  )
                case h :: tail =>
                  val tailP = loop(tail)
                  Pattern.PositionalStruct(
                    (PackageName.PredefName, Constructor("TupleCons")),
                    h :: tailP :: Nil
                  )
              }

            loop(args)
          case (Pattern.StructKind.Named(nm, _), args) =>
            Pattern.PositionalStruct(nameToCons(nm), args)
          case (Pattern.StructKind.NamedPartial(nm, _), args) =>
            Pattern.PositionalStruct(nameToCons(nm), args)
        }
        .mapType { tref =>
          TypeRefConverter[cats.Id](tref) { tpe =>
            Type.Const.Defined(PackageName.PredefName, TypeName(tpe))
          }
        }

    Parser
      .unsafeParse(Pattern.matchParser.listSyntax, str)
      .map(parsedToExpr _)
  }

  def notTotal(
      te: TypeEnv[Any],
      pats: List[Pattern[(PackageName, Constructor), Type]],
      testMissing: Boolean = true
  ): Unit = {
    val res = TotalityCheck(te).isTotal(pats)
    assert(!res, pats.toString)

    if (testMissing) {
      // if we add the missing, it should be total
      val mb = TotalityCheck(te).missingBranches(pats)
      // missing branches can't be tight because
      // for instance:
      // match x:
      //   1: foo
      //
      // is not total, but can only be made total by
      // adding a wildcard match, which by itself is total
      testTotality(te, pats ::: mb, tight = false)
    }
  }

  def testTotality(
      te: TypeEnv[Any],
      pats: List[Pattern[(PackageName, Constructor), Type]],
      tight: Boolean = false
  ) = {
    val res = TotalityCheck(te).missingBranches(pats)
    val asStr = res.map(showPat)
    assert(asStr == Nil, showPats(pats))

    // any missing pattern shouldn't be total:
    def allButOne[A](head: A, tail: List[A]): List[List[A]] =
      tail match {
        case Nil       => Nil
        case h :: rest =>
          // we can either delete the head or one from the tail:
          val keepHead = allButOne(h, rest).map(head :: _)
          tail :: keepHead
      }

    pats match {
      case h :: tail if tight =>
        allButOne(h, tail).foreach(
          notTotal(te, _, testMissing = false)
        ) // don't make an infinite loop here
      case _ => ()
    }
  }

  test("patterns are well ordered") {
    forAll(genPattern, genPattern, genPattern) { (a, b, c) =>
      OrderingLaws.law(a, b, c)
    }
  }

  test("totality test") {
    val te = typeEnvOf("""#
struct Unit
""")
    val pats = patterns("[Unit]")
    testTotality(te, pats)

    val te1 = typeEnvOf("""#
struct TupleCons(a, b)
""")
    testTotality(te1, patterns("[TupleCons(_, _)]"))
    testTotality(te1, patterns("[TupleCons(_, 0), TupleCons(_, _)]"))
    notTotal(te1, patterns("[TupleCons(_, 0)]"))
  }

  test("test Option types") {
    val te = typeEnvOf("""#
enum Option: None, Some(get)
""")
    testTotality(te, patterns("[Some(_), None]"), tight = true)
    testTotality(te, patterns("[Some(_) | None]"), tight = true)
    testTotality(te, patterns("[Some(_), _]"))
    testTotality(te, patterns("[Some(1), Some(x), None]"))
    testTotality(
      te,
      patterns("[Some(Some(_)), Some(None), None]"),
      tight = true
    )
    testTotality(te, patterns("[Some(Some(_) | None), None]"), tight = true)

    notTotal(te, patterns("[Some(_)]"))
    notTotal(te, patterns("[Some(Some(1) | None), None]"))
    notTotal(te, patterns("[Some(Some(_)), None]"))
    notTotal(te, patterns("[None]"))
    notTotal(te, patterns("[]"))
  }

  test("test Either types") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
""")
    testTotality(te, patterns("[Left(_), Right(_)]"))
    testTotality(
      te,
      patterns(
        "[Left(Right(_)), Left(Left(_)), Right(Left(_)), Right(Right(_))]"
      ),
      tight = true
    )

    testTotality(
      te,
      patterns("[Left(Right(_) | Left(_)), Right(Left(_) | Right(_))]"),
      tight = true
    )

    notTotal(te, patterns("[Left(_)]"))
    notTotal(te, patterns("[Right(_)]"))
    notTotal(te, patterns("[Left(Right(_)), Right(_)]"))
    notTotal(te, patterns("[Left(Right(_)) | Right(_)]"))
  }

  test("test List matching") {
    testTotality(predefTE, patterns("[[], [h, *tail]]"), tight = true)
    testTotality(
      predefTE,
      patterns("[[], [h, *tail], [h0, h1, *tail]]"),
      tight = true
    )
    testTotality(predefTE, patterns("[[], [*tail, _]]"), tight = true)
    testTotality(
      predefTE,
      patterns("[[*_, True, *_], [], [False, *_]]"),
      tight = true
    )
    testTotality(
      predefTE,
      patterns("[[*_, True, *_], [] | [False, *_]]"),
      tight = true
    )

    notTotal(predefTE, patterns("[[], [h, *tail, _]]"))
  }

  test("multiple struct compose") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
enum Option: None, Some(get)
struct TupleCons(fst, snd)
""")

    testTotality(
      te,
      patterns("[None, Some(Left(_)), Some(Right(_))]"),
      tight = true
    )
    testTotality(te, patterns("[None, Some(Left(_) | Right(_))]"), tight = true)
    testTotality(
      te,
      patterns(
        "[None, Some(TupleCons(Left(_), _)), Some(TupleCons(_, Right(_))), Some(TupleCons(Right(_), Left(_)))]"
      ),
      tight = true
    )
    testTotality(
      te,
      patterns(
        "[None, Some(TupleCons(Left(_), _) | TupleCons(_, Right(_))), Some(TupleCons(Right(_), Left(_)))]"
      ),
      tight = true
    )
  }

  test("compose List with structs") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
""")
    testTotality(
      te,
      patterns("[[Left(_), *_], [Right(_), *_], [], [_, _, *_]]"),
      tight = true
    )
    testTotality(
      te,
      patterns("[Left([]), Left([h, *_]), Right([]), Right([h, *_])]"),
      tight = true
    )
  }

  test("test intersection") {
    val p0 :: p1 :: p1norm :: Nil = patterns("[[*_], [*_, _], [_, *_]]")
    TotalityCheck(predefTE).intersection(p0, p1) match {
      case List(intr) => assert(intr == p1norm)
      case other      => fail(s"expected exactly one intersection: $other")
    }

    val p2 :: p3 :: Nil = patterns("[[*_], [_, _]]")
    TotalityCheck(predefTE).intersection(p2, p3) match {
      case List(intr) => assert(p3 == intr)
      case other      => fail(s"expected exactly one intersection: $other")
    }
  }

  test("test some difference examples") {
    val tc = TotalityCheck(predefTE)
    import tc.eqPat.eqv
    {
      val p0 :: p1 :: Nil = patterns("[[1], [\"foo\", _]]")
      tc.difference(p0, p1) match {
        case diff :: Nil => assert(eqv(p0, diff))
        case many => fail(s"expected exactly one difference: ${showPats(many)}")
      }
    }

    {
      val p0 :: p1 :: Nil = patterns("[[_, _], [[*foo]]]")
      TotalityCheck(predefTE).difference(p1, p0) match {
        case diff :: Nil => assert(eqv(diff, p1))
        case many => fail(s"expected exactly one difference: ${showPats(many)}")
      }
      TotalityCheck(predefTE).difference(p0, p1) match {
        case diff :: Nil => assert(eqv(diff, p0))
        case many => fail(s"expected exactly one difference: ${showPats(many)}")
      }
    }

    {
      val p0 :: p1 :: Nil = patterns("[[*_, _], [_, *_]]")
      TotalityCheck(predefTE).intersection(p0, p1) match {
        case List(res) if res == p0 || res == p1 => succeed
        case Nil                                 => fail("these do overlap")
        case nonUnified => fail(s"didn't unify to one: $nonUnified")
      }
    }
  }

  test("(a - b) n c == (a n c) - (b n c) regressions") {
    import Pattern._
    import StrPart.{LitStr, NamedStr, WildStr}
    import ListPart.{NamedList, Item}
    import Identifier.Name

    val regressions: List[(Pat, Pat, Pat)] =
      (
        Named(
          Name("hTt"),
          StrPat(
            NonEmptyList
              .of(NamedStr(Name("rfb")), LitStr("q"), NamedStr(Name("ngkrx")))
          )
        ),
        WildCard,
        Named(
          Name("hjbmtklh"),
          StrPat(
            NonEmptyList.of(
              NamedStr(Name("qz8lcT")),
              WildStr,
              LitStr("p7"),
              NamedStr(Name("hqxprG"))
            )
          )
        )
      ) ::
        (
          WildCard,
          ListPat(
            List(
              NamedList(Name("nv6")),
              Item(Literal(Lit.fromInt(-17))),
              Item(WildCard)
            )
          ),
          ListPat(
            List(
              Item(StrPat(NonEmptyList.of(WildStr))),
              Item(
                StrPat(
                  NonEmptyList.of(
                    NamedStr(Name("eejhh")),
                    LitStr("jbuzfcwsumP"),
                    WildStr
                  )
                )
              )
            )
          )
        ) ::
        Nil

    regressions.foreach { case (a, b, c) =>
      diffIntersectionLaw(a, b, c)
    }
  }

  test("x - top = 0 regressions") {
    // see: https://github.com/johnynek/bosatsu/issues/475
    def law(x: Pat, y: Pat) = {
      if (setOps.isTop(y)) assert(setOps.difference(x, y).isEmpty)
    }

    val regressions: List[(Pat, Pat)] =
      List({
        val struct = Pattern.PositionalStruct(
          (PackageName(NonEmptyList.of("Pack")), Identifier.Constructor("Foo")),
          Nil
        )
        val lst = Pattern.ListPat(List(Pattern.ListPart.WildList))
        (struct, lst)
      })

    regressions.foreach { case (a, b) => law(a, b) }
  }

  test("subset consistency regressions") {
    val regressions: List[(Pat, Pat)] = {
      val struct = Pattern.PositionalStruct(
        (PackageName(NonEmptyList.of("Pack")), Identifier.Constructor("Foo")),
        Nil
      )
      val lst = Pattern.ListPat(List(Pattern.ListPart.WildList))
      (struct, lst)
    } ::
      Nil

    regressions.foreach { case (a, b) =>
      subsetConsistencyLaw(a, b, eqPatterns)
    }
  }

  test("difference is idempotent regressions") {
    import Pattern._
    import ListPart._
    import Identifier.Name
    import StrPart.WildStr

    val regressions: List[(Pat, Pat)] =
      List({
        val left = ListPat(List(Item(WildCard), WildList))
        val right = ListPat(
          List(
            Item(Var(Name("bey6ct"))),
            Item(Literal(Lit.fromInt(42))),
            Item(StrPat(NonEmptyList.of(WildStr))),
            Item(Literal(Lit("agfn"))),
            Item(WildCard)
          )
        )
        (left, right)
      })

    regressions.foreach { case (a, b) =>
      differenceIsIdempotent(a, b, eqPatterns)
    }
  }

  test("if a n b = 0 then a - b = a regressions") {

    import Pattern._
    import ListPart._
    import Identifier.Name
    import StrPart.WildStr

    val regressions: List[(Pat, Pat)] =
      List(
        {
          val left = ListPat(List(Item(WildCard), WildList))
          val right = ListPat(
            List(
              Item(Var(Name("bey6ct"))),
              Item(Literal(Lit.fromInt(42))),
              Item(StrPat(NonEmptyList.of(WildStr))),
              Item(Literal(Lit("agfn"))),
              Item(WildCard)
            )
          )
          (left, right)
        }, {
          val left = ListPat(
            List(NamedList(Name("a")), Item(WildCard), Item(Var(Name("b"))))
          )
          val right = ListPat(List())
          (left, right)
        }
      )

    regressions.foreach { case (a, b) =>
      emptyIntersectionMeansDiffIdent(a, b, eqPatterns)
    }
  }
}
