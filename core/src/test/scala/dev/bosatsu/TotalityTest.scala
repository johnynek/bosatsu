package dev.bosatsu

import cats.Eq
import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import dev.bosatsu.set.{SetOps, SetOpsLaws, Rel}

import rankn._

import Parser.Combinators

import org.typelevel.paiges.Document

import Identifier.Constructor
import org.scalacheck.Shrink

class TotalityTest
    extends SetOpsLaws[Pattern[(PackageName, Constructor), Type]] {
  import Generators.shrinkPattern

  type Pat = Pattern[(PackageName, Constructor), Type]

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 1000 else 20)
      .withMaxDiscardRatio(10)

  val genPattern: Gen[Pattern[(PackageName, Constructor), Type]] =
    Generators.genCompiledPattern(5, useAnnotation = false)

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

  val PredefTotalityCheck = TotalityCheck(predefTE)
  val setOps: SetOps[Pattern[(PackageName, Constructor), Type]] =
    PredefTotalityCheck.patternSetOps

  override def genItem: Gen[Pattern[(PackageName, Constructor), Type]] =
    genPattern

  override val shrinkItem: Shrink[Pattern[(PackageName, Constructor), Type]] =
    shrinkPattern

  val genPatternNoUnion: Gen[Pattern[(PackageName, Constructor), Type]] =
    Generators.genCompiledPattern(5, useUnion = false, useAnnotation = false)

  override def genUnion =
    Gen.oneOf(
      genPattern.map { p =>
        // this will violate the law if we make List(union) because
        // we can expand that into more things than the input
        Pattern.flatten(PredefTotalityCheck.normalizePattern(p)).toList
      },
      Generators.smallList(genPatternNoUnion)
    )

  val eqPatterns: Eq[List[Pattern[(PackageName, Constructor), Type]]] =
    new Eq[List[Pattern[(PackageName, Constructor), Type]]] {
      val e1 = PredefTotalityCheck.eqPat
      private val optEq =
        Eq.fromUniversalEquals[
          Option[NonEmptyList[Pattern[(PackageName, Constructor), Type]]]
        ]

      def eqv(
          a: List[Pattern[(PackageName, Constructor), Type]],
          b: List[Pattern[(PackageName, Constructor), Type]]
      ) =
        (NonEmptyList.fromList(a), NonEmptyList.fromList(b)) match {
          case (oa, ob) if optEq.eqv(oa, ob) =>
            true
          case (Some(a), Some(b))   =>
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
      .map(parsedToExpr)
  }

  def notTotal(
      te: TypeEnv[Any],
      pats: List[Pattern[(PackageName, Constructor), Type]],
      testMissing: Boolean = true
  ): Unit = {
    val res = TotalityCheck(te).missingBranches(pats).isEmpty
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
  )(implicit loc: munit.Location) = {
    val res = TotalityCheck(te).missingBranches(pats)
    val asStr = res.map(showPat)
    assertEquals(asStr, Nil, showPats(pats))

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

  property("patterns are well ordered") {
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
    val p0 :: p1 :: p1norm :: Nil =
      patterns("[[*_], [*_, _], [_, *_]]").runtimeChecked
    PredefTotalityCheck.intersection(p0, p1) match {
      case List(intr) => assertEquals(intr, p1norm)
      case other      => fail(s"expected exactly one intersection: $other")
    }

    val p2 :: p3 :: Nil = patterns("[[*_], [_, _]]").runtimeChecked
    PredefTotalityCheck.intersection(p2, p3) match {
      case List(intr) => assertEquals(p3, intr)
      case other      => fail(s"expected exactly one intersection: $other")
    }

    // a regression
    {
      val p0 :: p1 :: p2 :: Nil =
        patterns("""["${_}$.{_}$.{_}",
        "$.{foo}",
        "baz"]""").runtimeChecked

      assert(PredefTotalityCheck.intersection(p0, p1).isEmpty)
      assert(PredefTotalityCheck.intersection(p1, p2).isEmpty)

      import pattern.SeqPattern.{stringUnitMatcher, Cat, Empty}
      import pattern.SeqPart.AnyElem
      import pattern.Splitter.stringUnit

      val strPat1 = p1.asInstanceOf[Pattern.StrPat]
      val seqPat = Cat(AnyElem, Empty)
      assertEquals(strPat1.toSeqPattern, seqPat)
      assert(!strPat1.matches("baz"))
      assert(stringUnitMatcher(seqPat)("baz").isEmpty)
      assertEquals(stringUnit.uncons("baz"), Some(('b'.toInt, "az")))
      assert(!stringUnit.isEmpty("az"))
    }
  }

  test("test some difference examples") {
    val tc = PredefTotalityCheck
    import tc.eqPat.eqv
    {
      val p0 :: p1 :: Nil = patterns("[[1], [\"foo\", _]]").runtimeChecked
      tc.difference(p0, p1) match {
        case diff :: Nil => assert(eqv(p0, diff))
        case many => fail(s"expected exactly one difference: ${showPats(many)}")
      }
    }

    {
      val p0 :: p1 :: Nil = patterns("[[_, _], [[*foo]]]").runtimeChecked
      PredefTotalityCheck.difference(p1, p0) match {
        case diff :: Nil => assert(eqv(diff, p1))
        case many => fail(s"expected exactly one difference: ${showPats(many)}")
      }
      PredefTotalityCheck.difference(p0, p1) match {
        case diff :: Nil => assert(eqv(diff, p0))
        case many => fail(s"expected exactly one difference: ${showPats(many)}")
      }
    }

    {
      val p0 :: p1 :: Nil = patterns("[[*_, _], [_, *_]]").runtimeChecked
      PredefTotalityCheck.intersection(p0, p1) match {
        case List(res) if res == p0 || res == p1 => ()
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
    def law(x: Pat, y: Pat)(implicit loc: munit.Location) =
      if (setOps.isTop(y))
        assert(
          setOps.difference(x, y).isEmpty,
          s"x = ${showPat(x)}, y = ${showPat(y)}"
        )

    val regressions: List[(Pat, Pat)] =
      List {
        val struct = Pattern.PositionalStruct(
          (PackageName(NonEmptyList.of("Pack")), Identifier.Constructor("Foo")),
          Nil
        )
        val lst = Pattern.ListPat(List(Pattern.ListPart.WildList))
        (struct, lst)
      }

    regressions.foreach { case (a, b) => law(a, b) }
  }

  private def pair(str: String): (Pat, Pat) = {
    val pats = patterns(str)
    (pats(0), pats(1))
  }
  test("subset consistency regressions") {
    val regressions: List[(Pat, Pat)] = {
      val struct = Pattern.PositionalStruct(
        (PackageName(NonEmptyList.of("Pack")), Identifier.Constructor("Foo")),
        Nil
      )
      val lst = Pattern.ListPat(List(Pattern.ListPart.WildList))
      (struct, lst)
    } :: {
      import Pattern._
      import ListPart._

      val a = WildCard
      /*
        val b = Union(ListPat(List()),NonEmptyList.of(
          ListPat(List(Item(Named(n("tmxb"),Union(Var(n("op")),NonEmptyList.of(Var(n("mjpqdwRbkz")), Literal(Lit.Chr("鱛")))))))),
          ListPat(List(Item(ListPat(List(WildList))), Item(Named(n("e7psNp0ok"),WildCard)), WildList))))
       */

      val b = Union(
        ListPat(List()),
        NonEmptyList.of(
          // ListPat(List(Item(Union(WildCard, NonEmptyList.of(WildCard, Literal(Lit.Chr("鱛"))))))),
          ListPat(List(Item(WildCard))),
          ListPat(List(Item(ListPat(List(WildList))), Item(WildCard), WildList))
        )
      )
      // ListPat(List(Item(WildCard), Item(WildCard), WildList))))

      assert(setOps.isTop(ListPat(List(WildList))))
      assertEquals(
        setOps.unifyUnion(Pattern.flatten(b).toList),
        WildCard :: Nil
      )
      assertEquals(setOps.relate(a, b), Rel.Same)
      (a, b)
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
        },
        pair("""[[] | [_, *_], "$.{_}${_}"]""")
      )

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

  test("difference returns distinct regressions") {
    def check(str: String) = {
      val List(p1, p2) = patterns(str).runtimeChecked
      val tc = PredefTotalityCheck
      val diff = tc.difference(p1, p2)
      assertEquals(diff, diff.distinct)
    }

    check("""["${foo}$.{_}", "$.{bar}$.{_}$.{_}"]""")
  }

  test("intersection is commutative regressions") {
    val tc = PredefTotalityCheck

    {
      val p0 :: p1 :: Nil = patterns("""[
        "${_}$.{_}",
        "${_}$.{_}${_}",
        ]""").runtimeChecked

      assertEquals(tc.intersection(p0, p1), tc.intersection(p1, p0))
    }
  }

  test("string match totality") {
    val tc = PredefTotalityCheck

    val ps = patterns("""["${_}$.{_}", ""]""")
    val diff = tc.missingBranches(ps)
    assertEquals(diff, Nil)

    val ps1 = patterns("""["", "$.{_}${_}"]""")
    val diff1 = tc.missingBranches(ps1)
    assertEquals(diff1, Nil)
  }

  override def missingBranchesIfAddedRegressions: List[List[Pat]] =
    patterns("""[[*foo, "$.{_}", "$.{_}"], [[b, *_]]]""") ::
      Nil

  test("var pattern is super or same") {
    val tc = PredefTotalityCheck

    val p1 :: p2 :: _ = patterns("""[foo, Bar(1)]""").runtimeChecked
    val rel = tc.patternSetOps.relate(p1, p2)
    assertEquals(rel, Rel.Super)
  }

  test("union commutes with type wrappers: Some(1 | 2) == Some(1) | Some(2)") {
    val tc = PredefTotalityCheck

    {
      val p1 :: p2 :: _ =
        patterns("""[Some(1 | 2), Some(1) | Some(2)]""").runtimeChecked
      val rel = tc.patternSetOps.relate(p1, p2)
      assertEquals(rel, Rel.Same)
    }

    {
      val p1 :: p2 :: _ =
        patterns("""[Some(1 | 2 | 3), Some(1) | Some(2)]""").runtimeChecked
      val rel = tc.patternSetOps.relate(p1, p2)
      assertEquals(rel, Rel.Super)
    }
  }

  property("unifyUnion returns no top-level unions") {
    forAll(Gen.listOf(genPattern)) { pats =>
      val unions =
        setOps.unifyUnion(pats).collect { case u @ Pattern.Union(_, _) => u }
      assertEquals(unions, Nil)
    }
  }

  property("unifyUnion(u) <:> u == Same") {
    def law(pat1: Pat, pat2: Pat)(implicit loc: munit.Location) = {
      val unions =
        NonEmptyList.fromListUnsafe(setOps.unifyUnion(pat1 :: pat2 :: Nil))
      val u1 = Pattern.union(unions.head, unions.tail)
      assertEquals(
        setOps.relate(Pattern.union(pat1, pat2 :: Nil), u1),
        Rel.Same,
        s"p1 = ${showPat(pat1)}\np2 = ${showPat(pat2)}\nunified = ${showPat(u1)}"
      )
    }

    val regressions =
      pair("""["$.{_}${_}$.{_}", "$.{_}${_}"]""") ::
        pair("""["$.{_}", "${_}$.{_}$.{_}" as e]""") ::
        pair("""["$.{a}", "${b}$.{c}$.{d}" as e]""") ::
        pair("""["$.{bar}" as baz, "${_}${_}$.{c}${d}"]""") ::
        pair("""["$.{bar}${_}$.{_}", "$.{_}${_}" as foo]""") ::
        Nil

    regressions.foreach { case (a, b) => law(a, b) }
    forAll(genPattern, genPattern)(law(_, _))
  }

  test("x - y where isTop(y) regressions") {
    val regressions =
      (pair("""["foo", ([] | [_, *_])]"""), true) ::
        Nil

    regressions.foreach { case ((x, y), top) =>
      val rel = setOps.relate(x, y)
      val yIsTop = setOps.isTop(y)
      if (top) {
        assert(yIsTop)
      }
      if (yIsTop) {
        assertEquals(
          setOps.difference(x, y),
          Nil,
          s"${showPat(x)} - ${showPat(y)}, rel = $rel"
        )
      }
    }
  }

  property("normalizePattern(p) <:> q == p <:> q") {
    forAll(genPattern, genPattern) { (p, q) =>
      val normp = PredefTotalityCheck.normalizePattern(p)
      assertEquals(
        setOps.relate(normp, q),
        setOps.relate(p, q)
      )

      assertEquals(setOps.relate(normp, p), Rel.Same)
    }
  }
}
