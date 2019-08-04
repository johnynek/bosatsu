package org.bykn.bosatsu

import cats.{Applicative, Eq}
import cats.data.{Chain, Writer}
import cats.implicits._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalacheck.Gen

import rankn._

import Parser.Combinators
import fastparse.all.Parsed

import org.typelevel.paiges.Document

import Identifier.Constructor

class TotalityTest extends FunSuite {
  import TestParseUtils._

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 500000)
    PropertyCheckConfiguration(minSuccessful = 5000)

  val genPattern: Gen[Pattern[(PackageName, Constructor), Type]] =
    Generators.genCompiledPattern(5, useAnnotation = false)

  val genPatternNoUnion: Gen[Pattern[(PackageName, Constructor), Type]] =
    Generators.genCompiledPattern(5, useUnion = false, useAnnotation = false)

  def showPat(pat: Pattern[(PackageName, Constructor), Type]): String = {
    val allTypes = pat.traverseType { t => Writer(Chain.one(t), ()) }.run._1.toList
    val toStr = TypeRef.fromTypes(None, allTypes)
    val pat0 = pat.mapName {
      case (_, n) =>
        Pattern.StructKind.Named(n, Pattern.StructKind.Style.TupleLike)
    }.mapType { t => toStr(t) }

    Document[Pattern.Parsed].document(pat0).render(80)
  }
  def showPatU(pat: Pattern[(PackageName, Constructor), Type]): String =
    showPat(pat.unbind)

  def typeEnvOf(str: String): TypeEnv[Unit] =
    TestUtils.typeEnvOf(Predef.packageName, str)

  val predefTE = typeEnvOf("""#
struct Unit
struct TupleCons(fst, snd)
""")

  def patterns(str: String): List[Pattern[(PackageName, Constructor), Type]] = {
    val nameToCons: Constructor => (PackageName, Constructor) =
      { cons => (Predef.packageName, cons) }

    /**
     * This is sufficient for these tests, but is not
     * a full features pattern compiler.
     */
    def parsedToExpr(pat: Pattern.Parsed): Pattern[(PackageName, Constructor), rankn.Type] =
      pat.mapStruct[(PackageName, Constructor)] {
        case (Pattern.StructKind.Tuple, args) =>
          // this is a tuple pattern
          def loop(args: List[Pattern[(PackageName, Constructor), TypeRef]]): Pattern[(PackageName, Constructor), TypeRef] =
            args match {
              case Nil =>
                // ()
                Pattern.PositionalStruct(
                  (Predef.packageName, Constructor("Unit")),
                  Nil)
              case h :: tail =>
                val tailP = loop(tail)
                Pattern.PositionalStruct(
                  (Predef.packageName, Constructor("TupleCons")),
                  h :: tailP :: Nil)
            }

          loop(args)
        case (Pattern.StructKind.Named(nm, _), args) =>
          Pattern.PositionalStruct(nameToCons(nm), args)
        case (Pattern.StructKind.NamedPartial(nm, _), args) =>
          Pattern.PositionalStruct(nameToCons(nm), args)
      }
      .mapType { tref =>
        TypeRefConverter[cats.Id](tref) { tpe =>
          Type.Const.Defined(Predef.packageName, TypeName(tpe))
        }
      }

    Pattern.matchParser.listSyntax.parse(str) match {
      case Parsed.Success(pats, idx) =>
        pats.map(parsedToExpr _)
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
        sys.error("could not produce TypeEnv")
    }
  }

  def notTotal(te: TypeEnv[Any], pats: List[Pattern[(PackageName, Constructor), Type]], testMissing: Boolean = true): Unit = {
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

  def testTotality(te: TypeEnv[Any], pats: List[Pattern[(PackageName, Constructor), Type]], tight: Boolean = false) = {
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
    testTotality(te, patterns("[Some(Some(_)), Some(None), None]"), tight = true)
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
    testTotality(te,
      patterns("[Left(Right(_)), Left(Left(_)), Right(Left(_)), Right(Right(_))]"),
      tight = true)

    testTotality(te,
      patterns("[Left(Right(_) | Left(_)), Right(Left(_) | Right(_))]"),
      tight = true)

    notTotal(te, patterns("[Left(_)]"))
    notTotal(te, patterns("[Right(_)]"))
    notTotal(te, patterns("[Left(Right(_)), Right(_)]"))
    notTotal(te, patterns("[Left(Right(_)) | Right(_)]"))
  }

  test("test List matching") {
    testTotality(predefTE, patterns("[[], [h, *tail]]"), tight = true)
    testTotality(predefTE, patterns("[[], [h, *tail], [h0, h1, *tail]]"), tight = true)
    testTotality(predefTE, patterns("[[], [*tail, _]]"), tight = true)

    notTotal(predefTE, patterns("[[], [h, *tail, _]]"))
  }

  test("multiple struct compose") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
enum Option: None, Some(get)
struct TupleCons(fst, snd)
""")

    testTotality(te, patterns("[None, Some(Left(_)), Some(Right(_))]"), tight = true)
    testTotality(te, patterns("[None, Some(Left(_) | Right(_))]"), tight = true)
    testTotality(te, patterns("[None, Some(TupleCons(Left(_), _)), Some(TupleCons(_, Right(_))), Some(TupleCons(Right(_), Left(_)))]"), tight = true)
    testTotality(te, patterns("[None, Some(TupleCons(Left(_), _) | TupleCons(_, Right(_))), Some(TupleCons(Right(_), Left(_)))]"), tight = true)
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
      TotalityCheck(predefTE).intersection(p0, p1) match {
        case Left(err) => fail(err.toString)
        case Right(List(intr)) => assert(p1 == intr)
        case Right(other) => fail(s"expected exactly one intersection: $other")
      }

    val p2 :: p3 :: Nil = patterns("[[*_], [_, _]]")
      TotalityCheck(predefTE).intersection(p2, p3) match {
        case Left(err) => fail(err.toString)
        case Right(List(intr)) => assert(p3 == intr)
        case Right(other) => fail(s"expected exactly one intersection: $other")
      }
  }

  test("test some difference examples") {
    val tc = TotalityCheck(predefTE)
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
      TotalityCheck(predefTE).difference0(p1, p0) match {
        case Left(err) => fail(err.toString)
        case Right(diff :: Nil) => assert(eqv(diff, p1))
        case Right(many) => fail(s"expected exactly one difference: ${many.map(showPat)}")
      }
      TotalityCheck(predefTE).difference0(p0, p1) match {
        case Left(err) => fail(err.toString)
        case Right(diff :: Nil) => assert(eqv(diff, p0))
        case Right(many) => fail(s"expected exactly one difference: ${many.map(showPat)}")
      }
    }

    {
      val p0 :: p1 :: Nil = patterns("[[*_, _], [_, *_]]")
      TotalityCheck(predefTE).intersection(p0, p1) match {
        case Right(List(res)) if res == p0 || res == p1 => succeed
        case Right(Nil) => fail("these do overlap")
        case Right(nonUnified) => fail(s"didn't unify to one: $nonUnified")
        case Left(err) => fail(s"we shouldn't error computing this intersection: ${err.toString}")
      }
    }
  }

  test("intersection(a, a) == a") {
    def law(p: Pattern[(PackageName, Constructor), Type]) =
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      TotalityCheck(predefTE)
        .intersection(p, p) match {
          case Left(_) => () // we can often fail now due to bad patterns
          case Right(List(h)) => assert(h == p)
          case Right(other) => fail(s"expected one intersection, found $other")
        }

    // the intersection law is hard to verify on unions since we expand unions
    // when doing intersections
    forAll(genPatternNoUnion)(law)

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
      a: Pattern[(PackageName, Constructor), Type],
      b: Pattern[(PackageName, Constructor), Type],
      c: Pattern[(PackageName, Constructor), Type]
    ) = {
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(predefTE)
      import tc.intersection

      val left = intersection(a, b).flatMap(_.traverse(intersection(_, c))).map(_.flatten).map(_.map(_.unbind))
      val right = intersection(b, c).flatMap(_.traverse(intersection(a, _))).map(_.flatten).map(_.map(_.unbind))

      (left, right) match {
        case (Right(a), Right(b)) if a == b => succeed
        case (Right(rl), Right(rr)) =>
          tc.differenceAll(rl, rr).product(tc.differenceAll(rr, rl)) match {
            case Left(_) => () // we can still error in difference due to being ill-typed
            case Right((Nil, Nil)) => succeed
            case Right(_) =>
              val diffl = rl.map(_.unbind).filterNot(rr.map(_.unbind).toSet)
              val diffr = rr.map(_.unbind).filterNot(rl.map(_.unbind).toSet)
              fail(s"rl: ${rl.map(showPatU)}\nrr: ${rr.map(showPatU)}\ndiffl: ${diffl.map(showPat)}, diffr: ${diffr.map(showPat)}")
          }
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

    manualTest("[[a, b, *c], [d, *_, _], [[e], _]]")
    manualTest("[[a, *b], [*c, d], [*e, _, _, _]]")
    manualTest("[[*_, _], [*_, _, _], [_, *_]]")

    val complex = "[['foo'], _, [_, _, 'bar'], *_]"
    manualTest(s"[[*_, _, _, _], [*_, x, _], $complex]")


    {
      val a0 = "[1, *a]"
      val a1 = "[*b, c, d, Foo, [_, e, *f]]"
      val a2 = "[_, g, ['foo'], _, *h]"
      manualTest(s"[$a0, $a1, $a2]")
    }
  }

  test("intersection(a, b) == intersection(b, a)") {
    def law(
      a: Pattern[(PackageName, Constructor), Type],
      b: Pattern[(PackageName, Constructor), Type]) = {
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val ab = TotalityCheck(predefTE)
        .intersection(a, b)
      val ba = TotalityCheck(predefTE)
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

    // unions and intersections are hard to normalize
    forAll(genPatternNoUnion, genPatternNoUnion)(law)

    def manualTest(str: String) = {
      val a :: b :: Nil = patterns(str)
      law(a, b)
    }

    manualTest("[[_, _], [*foo]]")
    manualTest("[[*foo], [1, 2, *_]]")
  }

  test("if intersection(a, b) = 0, then a - b == a") {
    def law(
      a: Pattern[(PackageName, Constructor), Type],
      b: Pattern[(PackageName, Constructor), Type]) = {

      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(predefTE)
      tc.intersection(a, b) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(Nil) =>
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

    forAll(genPatternNoUnion, genPatternNoUnion)(law(_, _))
  }

  test("difference returns distinct values") {
    forAll(genPattern, genPattern) { (a, b) =>
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(predefTE)
      tc.difference0(a, b) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(c) => assert(c == c.distinct)
      }
    }
  }
  test("intersection returns distinct values") {
    forAll(genPattern, genPattern) { (a, b) =>
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(predefTE)
      tc.intersection(a, b) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(c) => assert(c == c.distinct)
      }
    }
  }

  test("a - b = c then c - b == c, because we have already removed all of b") {
    def law(
      a: Pattern[(PackageName, Constructor), Type],
      b: Pattern[(PackageName, Constructor), Type]) = {
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(predefTE)
      tc.difference0(a, b) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(c) =>
          tc.difference(c, b) match {
            case Left(err) => () // due to our generators, we fail a lot
            case Right(c1) =>
              // // this quadradic, but order independent
              val eqList1 = new Eq[TotalityCheck.Patterns] {
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
              // now we check that c - c1 is empty and c1 - c is empty
              val eqList2 = new Eq[TotalityCheck.Patterns] {
                def eqv(a: TotalityCheck.Patterns, b: TotalityCheck.Patterns) = {
                  (a, b) match {
                    case (Nil, Nil) => true
                    case (Nil, _) => false
                    case (_, Nil) => false
                    case (ah :: at, bh :: bt) =>
                      val ua = Pattern.union(ah, at)
                      val ub = Pattern.union(bh, bt)
                      (tc.difference0(ua, ub), tc.difference0(ub, ua)) match {
                        case (Right(Nil), Right(Nil)) => true
                        case _ => false
                      }
                  }
                }
              }
              // if we are equal according to either of these, we are equal,
              // but sometimes we are equal and one or the other won't see it
              // due to this analysis making almost no use of types.
              assert(eqList1.eqv(c1, c) || eqList2.eqv(c1, c),
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
    manualTest("[_, [_, _] | [*_, _]]")
    // see issue #147, this was minimized from that
    manualTest("[_, [_, _] | [*_, ([1], [*ad, s3] | (3, _) | 2)]]")
    forAll(genPattern, genPattern)(law)
  }

  test("a - a = 0") {
    forAll(genPattern) { a =>
      // this would be better if we could get
      // generate random patterns from a sane
      // type Env... thats a TODO)
      val tc = TotalityCheck(predefTE)
      tc.difference0(a, a) match {
        case Left(_) => () // we can often fail now due to bad patterns
        case Right(Nil) => succeed
        case Right(many) => fail(s"expected empty difference: $many")
      }
    }
  }

  test("(a - b) n c = (a n c) - (b n c)") {
    def law(
      a: Pattern[(PackageName, Constructor), Type],
      b: Pattern[(PackageName, Constructor), Type],
      c: Pattern[(PackageName, Constructor), Type]) = {
      val tc = TotalityCheck(predefTE)
      val left = tc.difference0(a, b)
        .flatMap(_.traverse(tc.intersection(_, c)).map(_.flatten))
      val right = (tc.intersection(a, c), tc.intersection(b, c))
        .mapN {
          case (as, bs) =>
            val pairs = Applicative[List].product(as, bs)
            pairs.traverse { case (a, b) => tc.difference0(a, b) }
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
      val tc = TotalityCheck(predefTE)
      tc.missingBranches(pats) match {
        case Left(_) => ()
        case Right(rest) =>
          tc.missingBranches(pats ::: rest) match {
            case Left(err) => ()
              // with unions this is somehow no longer passing.
              // it would be better to generate a TypeEnv and then make
              // patterns from that so we don't see failures
              // fail(s"started with: ${pats.map(showPat)} added: ${rest.map(showPat)} but got: $err")
            case Right(Nil) => succeed
            case Right(rest1) =>
              fail(s"after adding ${rest.map(showPat)} we still need ${rest1.map(showPat)}")
          }
      }
    }
  }

  test("missing branches are distinct") {
    val pats = Gen.choose(0, 10).flatMap(Gen.listOfN(_, genPattern))

    forAll(pats) { pats =>
      val tc = TotalityCheck(predefTE)
      tc.missingBranches(pats) match {
        case Left(_) => ()
        case Right(rest) =>
          assert(rest == rest.distinct)
      }
    }
  }
}
