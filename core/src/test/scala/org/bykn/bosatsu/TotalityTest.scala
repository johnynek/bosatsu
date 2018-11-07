package org.bykn.bosatsu

import cats.Eq
import cats.implicits._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalacheck.Gen

import rankn._

import Parser.Combinators
import fastparse.all.Parsed

import org.typelevel.paiges.Document

class TotalityTest extends FunSuite {
  import TestParseUtils._

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 50)
    //PropertyCheckConfiguration(minSuccessful = 5)

  val pack = PackageName.parts("Test")
  def const(t: String): Type =
    Type.TyConst(Type.Const.Defined(pack, t))

  val tpeFn: String => Type.Const =
    { tpe => Type.Const.Defined(pack, tpe) }

  val consFn: String => (PackageName, ConstructorName) =
    { cons => (pack, ConstructorName(cons)) }

  def parsedToExpr(pat: Pattern[String, TypeRef]): Pattern[(PackageName, ConstructorName), Type] =
    pat
      .mapName(consFn)
      .mapType(_.toType(tpeFn))

  val genPattern: Gen[Pattern[(PackageName, ConstructorName), Type]] =
    Generators.genPattern(5)
      .map(parsedToExpr _)

  def showPat(pat: Pattern[(PackageName, ConstructorName), Type]): String = {
    val pat0 = pat.mapName { case (_, ConstructorName(n)) => n }
      .mapType { t => TypeRef.fromType(t).get }
    Document[Pattern[String, TypeRef]].document(pat0).render(80)
  }
  def showPatU(pat: Pattern[(PackageName, ConstructorName), Type]): String =
    showPat(pat.unbind)

  def typeEnvOf(str: String): TypeEnv =
    Statement.parser.parse(str) match {
      case Parsed.Success(stmt, idx) =>
        assert(idx == str.length)
        val prog = Program.fromStatement(
          pack,
          tpeFn,
          consFn,
          stmt)
        prog.types
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
        sys.error("could not produce TypeEnv")
    }

  def patterns(str: String): List[Pattern[(PackageName, ConstructorName), Type]] =
    Pattern.parser.listSyntax.parse(str) match {
      case Parsed.Success(pats, idx) =>
        pats.map(parsedToExpr _)
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
        sys.error("could not produce TypeEnv")
    }

  def notTotal(te: TypeEnv, pats: List[Pattern[(PackageName, ConstructorName), Type]], testMissing: Boolean = true): Unit = {
    TotalityCheck(te).isTotal(pats) match {
      case Right(res) => assert(!res, pats.toString)
      case Left(errs) => fail(errs.toString)
    }

    if (testMissing) {
      // if we add the missing, it should be total
      TotalityCheck(te).missingBranches(pats) match {
        case Left(errs) => fail(errs.toString)
        case Right(mb) =>
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
  }

  def testTotality(te: TypeEnv, pats: List[Pattern[(PackageName, ConstructorName), Type]], tight: Boolean = false) = {
    TotalityCheck(te).missingBranches(pats) match {
      case Right(res) =>
        val asStr = res.map(showPat)
        assert(asStr == Nil)
      case Left(errs) => fail(errs.toString)
    }
    // any missing pattern shouldn't be total:
    def allButOne[A](head: A, tail: List[A]): List[List[A]] =
      tail match {
        case Nil => Nil
        case h :: rest =>
          // we can either delete the head or one from the tail:
          val keepHead = allButOne(h, rest).map(head :: _)
          tail :: keepHead
      }

    pats match {
      case h :: tail if tight =>
        allButOne(h, tail).foreach(notTotal(te, _, testMissing = false)) // don't make an infinite loop here
      case _ => ()
    }
  }



  test("totality test") {
    val te = typeEnvOf("""#
struct Unit
""")
    val pats = patterns("[Unit]")
    testTotality(te, pats)


    val te1 = typeEnvOf("""#
struct Tuple2(a, b)
""")
    testTotality(te1, patterns("[Tuple2(_, _)]"))
    testTotality(te1, patterns("[Tuple2(_, 0), Tuple2(_, _)]"))
    notTotal(te1, patterns("[Tuple2(_, 0)]"))
  }

  test("test Option types") {
    val te = typeEnvOf("""#
enum Option: None, Some(get)
""")
    testTotality(te, patterns("[Some(_), None]"), tight = true)
    testTotality(te, patterns("[Some(_), _]"))
    testTotality(te, patterns("[Some(1), Some(x), None]"))
    testTotality(te, patterns("[Some(Some(_)), Some(None), None]"), tight = true)

    notTotal(te, patterns("[Some(_)]"))
    notTotal(te, patterns("[Some(Some(_)), None]"))
    notTotal(te, patterns("[None]"))
    notTotal(te, patterns("[]"))
  }

  test("test Either types") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
""")
    testTotality(te, patterns("[Left(_), Right(_)]"))
    testTotality(te,
      patterns("[Left(Right(_)), Left(Left(_)), Right(Left(_)), Right(Right(_))]"),
      tight = true)

    notTotal(te, patterns("[Left(_)]"))
    notTotal(te, patterns("[Right(_)]"))
    notTotal(te, patterns("[Left(Right(_)), Right(_)]"))
  }

  test("test List matching") {
    testTotality(TypeEnv.empty, patterns("[[], [h, *tail]]"), tight = true)
    testTotality(TypeEnv.empty, patterns("[[], [h, *tail], [h0, h1, *tail]]"), tight = true)
    testTotality(TypeEnv.empty, patterns("[[], [*tail, _]]"), tight = true)

    notTotal(TypeEnv.empty, patterns("[[], [h, *tail, _]]"))
  }

  test("multiple struct compose") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
enum Option: None, Some(get)
struct Tuple2(fst, snd)
""")

    testTotality(te, patterns("[None, Some(Left(_)), Some(Right(_))]"), tight = true)
    testTotality(te, patterns("[None, Some(Tuple2(Left(_), _)), Some(Tuple2(_, Right(_))), Some(Tuple2(Right(_), Left(_)))]"), tight = true)
  }

  test("compose List with structs") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
""")
    testTotality(te, patterns("[[Left(_), *_], [Right(_), *_], [], [_, _, *_]]"), tight = true)
    testTotality(te, patterns("[Left([]), Left([h, *_]), Right([]), Right([h, *_])]"), tight = true)
  }


  test("test intersection") {
    val p0 :: p1 :: Nil = patterns("[[*_], [*_, _]]")
      TotalityCheck(TypeEnv.empty).intersection(p0, p1) match {
        case Left(err) => fail(err.toString)
        case Right(Some(intr)) => assert(p1 == intr)
        case Right(None) => fail("expected exactly one intersection")
      }

    val p2 :: p3 :: Nil = patterns("[[*_], [_, _]]")
      TotalityCheck(TypeEnv.empty).intersection(p2, p3) match {
        case Left(err) => fail(err.toString)
        case Right(Some(intr)) => assert(p3 == intr)
        case Right(None) => fail("expected exactly one intersection")
      }
  }

  test("test some difference examples") {
    val tc = TotalityCheck(TypeEnv.empty)
    import tc.eqPat.eqv
    {
      val p0 :: p1 :: Nil = patterns("[[1], [\"foo\", _]]")
      tc.difference0(p0, p1) match {
        case Left(err) => fail(err.toString)
        case Right(diff :: Nil) => assert(eqv(p0, diff))
        case Right(many) => fail(s"expected exactly one difference: ${many.map(showPat)}")
      }
    }

    {
      val p0 :: p1 :: Nil = patterns("[[_, _], [[*foo]]]")
      TotalityCheck(TypeEnv.empty).difference0(p1, p0) match {
        case Left(err) => fail(err.toString)
        case Right(diff :: Nil) => assert(eqv(diff, p1))
        case Right(many) => fail(s"expected exactly one difference: ${many.map(showPat)}")
      }
      TotalityCheck(TypeEnv.empty).difference0(p0, p1) match {
        case Left(err) => fail(err.toString)
        case Right(diff :: Nil) => assert(eqv(diff, p0))
        case Right(many) => fail(s"expected exactly one difference: ${many.map(showPat)}")
      }
    }
  }

  test("intersection(a, a) == a") {
    def law(p: Pattern[(PackageName, ConstructorName), Type]) =
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      TotalityCheck(TypeEnv.empty)
        .intersection(p, p) match {
          case Left(_) => () // we can often fail now due to bad patterns
          case Right(Some(h)) => assert(h == p)
          case Right(None) => fail(s"expected one intersection, found none")
        }

    forAll(genPattern)(law)

    def manualTest(str: String) = {
      val a :: Nil = patterns(str)
      law(a)
    }

    manualTest("[[_, _]]")
    manualTest("[[*foo]]")
    manualTest("[[*_]]")
  }

  test("intersection is associative") {
    def law(
      a: Pattern[(PackageName, ConstructorName), Type],
      b: Pattern[(PackageName, ConstructorName), Type],
      c: Pattern[(PackageName, ConstructorName), Type]
    ) = {
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(TypeEnv.empty)
      import tc.intersection

      val left = for {
        abO <- intersection(a, b)
        abc <- abO match {
          case None => Right(None)
          case Some(ab) => intersection(ab, c)
        }
      } yield abc

      val right = for {
        bcO <- intersection(b, c)
        abc <- bcO match {
          case None => Right(None)
          case Some(bc) => intersection(a, bc)
        }
      } yield abc

      (left, right) match {
        case (Right(rl), Right(rr)) => assert(rl == rr)
        case (_, _) =>
          // since our random patterns are ill-typed, there
          // is no guarantee we get errors in both directions
          ()
      }
    }

    forAll(genPattern, genPattern, genPattern)(law)

    def manualTest(str: String) = {
      val a :: b :: c :: Nil = patterns(str)
      law(a, b, c)
    }
  }

  test("intersection(a, b) == intersection(b, a)") {
    def law(
      a: Pattern[(PackageName, ConstructorName), Type],
      b: Pattern[(PackageName, ConstructorName), Type]) = {
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val ab = TotalityCheck(TypeEnv.empty)
        .intersection(a, b)
      val ba = TotalityCheck(TypeEnv.empty)
        .intersection(b, a)
      (ab, ba) match {
        case (Left(_), Left(_)) => ()
        case (Left(err), Right(_)) =>
          fail(s"a = ${showPat(a)} b = ${showPat(b)} ab fails, but ba succeeds: $err")
        case (Right(_), Left(err)) =>
          fail(s"a = ${showPat(a)} b = ${showPat(b)} ba fails, but ab succeeds: $err")
        case (Right(ab), Right(ba)) =>
          assert(ab.map(showPatU) == ba.map(showPatU), s"a = ${showPat(a)} b = ${showPat(b)}")
      }
    }

    forAll(genPattern, genPattern)(law)

    def manualTest(str: String) = {
      val a :: b :: Nil = patterns(str)
      law(a, b)
    }

    manualTest("[[_, _], [*foo]]")
    manualTest("[[*foo], [1, 2, *_]]")
  }

  test("if intersection(a, b) = 0, then a - b == a") {
    def law(
      a: Pattern[(PackageName, ConstructorName), Type],
      b: Pattern[(PackageName, ConstructorName), Type]) = {

      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(TypeEnv.empty)
      tc.intersection(a, b) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(None) =>
          tc.difference0(a, b) match {
            case Left(err) => () // due to our generators, we fail a lot
            case Right(h :: Nil) =>
              assert(tc.eqPat.eqv(h, a), s"${showPat(h)} != ${showPat(a)}")
            case Right(newDiff) =>
              // our tests are not well typed, in well typed
              // code, this shouldn't happen, but it can if the
              // right side is total:
              tc.isTotal(b::Nil) match {
                case Left(_) => ()
                case Right(true) => ()
                case Right(false) =>
                  fail(s"a = ${showPat(a)} b = ${showPat(b)}, a n b == 0. expected no diff, found ${newDiff.map(showPat)}")
              }
          }
        case Right(_) => ()
      }
    }

    def manualTest(str: String) = {
      val a :: b :: Nil = patterns(str)
      law(a, b)
    }

    /**
     * These are some harder regressions that have caught us in the past
     */
    manualTest("[[_, _], [*foo]]")
    /*
     * the following is trick:
     * [_, _] n [[*foo]] == 0 because the left matches 2 item lists and the right only 1
     *
     * if we consider it as a product:
     * _ x [_] - [*foo] x []
     *
     * the product difference formula:
     * (a0 x a1) - (b0 x b1) = (a0 - b0) x a1 + (a0 n b0) x (a1 - b1)
     *
     * suggests: _ - [*foo] = 0 for well typed expressions
     * and (_ n [*foo]) = [*foo] or _ depending on the way to write it
     *
     * then [_] - [] = [_] since they don't overlap
     *
     * if we write `(_ n [*foo]) as _ we get the right answer
     * if we write it as [*foo] we get [[*foo], _]
     */
    manualTest("[[_, _], [[*foo]]]")
    manualTest("[['foo'], [*foo, [*_]]]")
    manualTest("[[*_, [_]], [1]]")
    /*
     * This is hard because they are orthogonal, one is list of 2, one is a list of three,
     * but the first element has a difference
     */
    manualTest("[[[cvspypdahs, *_], ['jnC']], [[*_, 5921457613766301145, 'j'], p, bmhvhs]]")

    forAll(genPattern, genPattern)(law(_, _))
  }

  test("difference returns distinct values") {
    forAll(genPattern, genPattern) { (a, b) =>
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(TypeEnv.empty)
      tc.difference0(a, b) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(c) => assert(c == c.distinct)
      }
    }
  }

  test("a - b = c then c - b == c, because we have already removed all of b") {
    def law(
      a: Pattern[(PackageName, ConstructorName), Type],
      b: Pattern[(PackageName, ConstructorName), Type]) = {
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(TypeEnv.empty)
      tc.difference0(a, b) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(c) =>
          tc.difference(c, b) match {
            case Left(err) => () // due to our generators, we fail a lot
            case Right(c1) =>
              // this quadradic, but order independent
              val eqList = new Eq[TotalityCheck.Patterns] {
                def eqv(a: TotalityCheck.Patterns, b: TotalityCheck.Patterns) = {
                  (a, b) match {
                    case (ah :: taila, _) if taila.exists(tc.eqPat.eqv(ah, _)) =>
                      // duplicate, skip it
                      eqv(taila, b)
                    case (_, bh :: tailb) if tailb.exists(tc.eqPat.eqv(bh, _)) =>
                      // duplicate, skip it
                      eqv(a, tailb)
                    case (ah :: taila, Nil) => false
                    case (ah :: taila, bh :: tailb) =>
                      b.exists(tc.eqPat.eqv(_, ah)) &&
                        a.exists(tc.eqPat.eqv(_, bh)) &&
                        eqv(taila, tailb)
                    case (Nil, Nil) => true
                    case (Nil, _) => false
                    case (_, Nil) => false
                  }
                }
              }

              assert(eqList.eqv(c1, c),
                s"${showPat(a)} - (b=${showPat(b)}) = ${c.map(showPat)} - b = ${c1.map(showPat)} diff = ${
                  c.map(showPatU).diff(c1.map(showPatU))}" )
          }
      }
    }
    def manualTest(str: String) = {
      val a :: b :: Nil = patterns(str)
      law(a, b)
    }

    manualTest("[[*foo, bar], [baz]]")
    forAll(genPattern, genPattern)(law)
  }

  test("a - a = 0") {
    forAll(genPattern) { a =>
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(TypeEnv.empty)
      tc.difference0(a, a) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(Nil) => succeed
        case Right(many) => fail(s"expected empty difference: $many")
      }
    }
  }

  test("(a - b) n c = (a n c) - (b n c)") {
    def law(
      a: Pattern[(PackageName, ConstructorName), Type],
      b: Pattern[(PackageName, ConstructorName), Type],
      c: Pattern[(PackageName, ConstructorName), Type]) = {
      val tc = TotalityCheck(TypeEnv.empty)
      val left = tc.difference0(a, b)
        .flatMap(_.traverse(tc.intersection(_, c)).map(_.flatten))
      val right = (tc.intersection(a, c), tc.intersection(b, c))
        .mapN {
          case (Some(a), Some(b)) => tc.difference0(a, b)
          case _ => Right(Nil)
        }
        .flatMap(e => e)

      (left, right) match {
        case (Right(lr), Right(rr)) => assert(lr == rr)
        case _ =>()
      }
    }

  }

  test("missing branches, if added is total") {
    val pats = Gen.choose(0, 10).flatMap(Gen.listOfN(_, genPattern))

    forAll(pats) { pats =>
      val tc = TotalityCheck(TypeEnv.empty)
      tc.missingBranches(pats) match {
        case Left(_) => ()
        case Right(rest) =>
          tc.missingBranches(pats ::: rest) match {
            case Left(err) => fail(err.toString)
            case Right(Nil) => succeed
            case Right(rest1) =>
              fail(s"after adding $rest we still need $rest1")
          }
      }
    }
  }

  test("missing branches are distinct") {
    val pats = Gen.choose(0, 10).flatMap(Gen.listOfN(_, genPattern))

    forAll(pats) { pats =>
      val tc = TotalityCheck(TypeEnv.empty)
      tc.missingBranches(pats) match {
        case Left(_) => ()
        case Right(rest) =>
          assert(rest == rest.distinct)
      }
    }
  }
}
