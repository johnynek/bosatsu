package org.bykn.bosatsu.parser

import cats.{Eq, Id, FlatMap, Functor, Defer, MonoidK, Monad}
import cats.arrow.FunctionK
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Cogen}

import cats.implicits._

sealed abstract class GenT[F[_]] { self =>
  type A
  val cogen: Cogen[A]
  val fa: F[A]

  def transform[G[_]](fk: FunctionK[F, G]): GenT[G] =
    new GenT[G] {
      type A = self.A
      val cogen = self.cogen
      val fa: G[A] = fk(self.fa)
    }

  def toId(implicit F: Functor[F]): F[GenT[Id]] =
    F.map(fa) { a =>
      new GenT[Id] {
        type A = self.A
        val cogen = self.cogen
        val fa = a
      }
    }

  override def toString: String = s"GenT($fa)"
}

object GenT {
  def apply[F[_], A0: Cogen](pa: F[A0]): GenT[F] =
    new GenT[F] {
      type A = A0
      val cogen = implicitly[Cogen[A0]]
      val fa = pa
    }
}

object ParserGen {
  implicit val functorGen: Functor[Gen] =
    new Functor[Gen] {
      def map[A, B](ga: Gen[A])(fn: A => B) = ga.map(fn)
    }

  def arbGen[A: Arbitrary: Cogen]: GenT[Gen] =
    GenT(Arbitrary.arbitrary[A])

  val pures: Gen[GenT[Gen]] =
    Gen.oneOf(arbGen[Int], arbGen[Boolean], arbGen[String], arbGen[(Int, Int)])

  val expect0: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser.fail: Parser[Unit])
      else GenT(Parser.expect(str))
    }

  val charIn: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[List[Char]].map { cs =>
      GenT(Parser.charIn(cs))
    }

  val charIn1: Gen[GenT[Parser1]] =
    Gen.zip(Arbitrary.arbitrary[Char],
      Arbitrary.arbitrary[List[Char]])
        .map { case (c0, cs) =>
          GenT(Parser.charIn1(c0 :: cs))
        }

  val expect1: Gen[GenT[Parser1]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser.fail: Parser1[Unit])
      else GenT(Parser.expect(str))
    }

  val fail: Gen[GenT[Parser]] =
    Gen.const(GenT(Parser.fail: Parser[Unit]))

  def void(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.void(g.fa))

  def void1(g: GenT[Parser1]): GenT[Parser1] =
    GenT(Parser.void1(g.fa))

  def string(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.string(g.fa))

  def string1(g: GenT[Parser1]): GenT[Parser1] =
    GenT(Parser.string1(g.fa))

  def backtrack(g: GenT[Parser]): GenT[Parser] =
    GenT(g.fa.backtrack)(g.cogen)

  def backtrack1(g: GenT[Parser1]): GenT[Parser1] =
    GenT(g.fa.backtrack)(g.cogen)

  def product(ga: GenT[Parser], gb: GenT[Parser]): GenT[Parser] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].product(ga.fa, gb.fa))
  }

  def product1(ga: GenT[Parser1], gb: GenT[Parser1]): GenT[Parser1] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    GenT[Parser1, (ga.A, gb.A)](FlatMap[Parser1].product(ga.fa, gb.fa))
  }

  def mapped(ga: GenT[Parser]): Gen[GenT[Parser]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.map { fn =>
        GenT(ga.fa.map(fn))
      }
    }
  }

  def mapped1(ga: GenT[Parser1]): Gen[GenT[Parser1]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.flatMap { fn =>
        Gen.oneOf(
          GenT(ga.fa.map(fn)),
          GenT(FlatMap[Parser1].map(ga.fa)(fn))
        )
      }
    }
  }

  def flatMapped(ga: Gen[GenT[Parser]]): Gen[GenT[Parser]] =
    Gen.zip(ga, pures).flatMap { case (parser, genRes) =>
      // TODO we need a Gen[A] for Parser
      val gfn0: Gen[parser.A => genRes.A] =
        Gen.function1(genRes.fa)(parser.cogen)

      val gfn: Gen[parser.A => Parser[genRes.A]] =
        gfn0.map { fn => fn.andThen { (out: genRes.A) => Parser.pure(out) } }

      gfn.flatMap { fn =>
        Gen.oneOf(
          GenT(parser.fa.flatMap(fn))(genRes.cogen),
          GenT(FlatMap[Parser].flatMap(parser.fa)(fn))(genRes.cogen)
        )
      }
    }

  def orElse(ga: GenT[Parser], gb: GenT[Parser], res: GenT[Gen]): Gen[GenT[Parser]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).map { case (f1, f2) =>
      GenT(ga.fa.map(f1).orElse(gb.fa.map(f2)))
    }
  }

  def orElse1(ga: GenT[Parser1], gb: GenT[Parser1], res: GenT[Gen]): Gen[GenT[Parser1]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).map { case (f1, f2) =>
      GenT(ga.fa.map(f1).orElse1(gb.fa.map(f2)))
    }
  }

  // Generate a random parser
  lazy val gen: Gen[GenT[Parser]] = {
    val rec = Gen.lzy(gen)

    Gen.frequency(
      (3, pures.flatMap(_.toId).map(_.transform(new FunctionK[Id, Parser] {
        def apply[A](g: Id[A]): Parser[A] = Parser.pure(g)
      }))),
     (5, expect0),
     (5, charIn),
     (1, rec.map(void(_))),
     (1, rec.map(string(_))),
     (1, rec.map(backtrack(_))),
     (1, rec.flatMap(mapped(_))),
     (1, flatMapped(rec)),
     (1, Gen.zip(rec, rec).map { case (g1, g2) => product(g1, g2) }),
     (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse(g1, g2, p) })
    )
  }

  // Generate a random parser
  lazy val gen1: Gen[GenT[Parser1]] = {
    val rec = Gen.lzy(gen1)

    Gen.frequency(
     (8, expect1),
     (8, charIn1),
     (2, rec.map(void1(_))),
     (2, rec.map(string1(_))),
     (2, rec.map(backtrack1(_))),
     (1, rec.flatMap(mapped1(_))),
     (1, Gen.zip(rec, rec).map { case (g1, g2) => product1(g1, g2) }),
     (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse1(g1, g2, p) })
    )
  }

}

class ParserTest extends munit.ScalaCheckSuite {

  val tests: Int = if (BitSetUtil.isScalaJs) 50 else 500

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(tests)
      .withMaxDiscardRatio(10)

  def parseTest[A: Eq](p: Parser[A], str: String, a: A) =
    p.parse(str) match {
      case Right((_, res)) =>
        assert(Eq[A].eqv(a, res), s"expected: $a got $res")
      case Left(errs) =>
        assert(false, errs.toString)
    }

  def parseFail[A: Eq](p: Parser[A], str: String) =
    p.parse(str) match {
      case Right(res) =>
        assert(false, s"expected to not parse, but found: $res")
      case Left(errs) =>
        assert(true)
    }

  test("pure works") {
    parseTest(Parser.pure(42), "anything", 42)
  }

  val fooP = Parser.expect("foo")
  val barP = Parser.expect("bar")

  test("expect tests") {
    parseTest(fooP, "foobar", ())
    parseFail(fooP, "bar")

    parseTest(Parser.oneOf1(fooP :: barP :: Nil), "bar", ())
    parseTest(Parser.oneOf1(fooP :: barP :: Nil), "foo", ())
  }

  test("product tests") {
    parseTest(Parser.product01(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product10(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product(fooP, barP), "foobar", ((), ()))
  }

  val digit = Parser.charIn1('0' to '9')
  val digit1 = Parser.charIn1('1' to '9')
  def maybeNeg[A](p1: Parser1[A]): Parser1[String] =
     (Parser.expect("-").?.with1 ~ p1).string

  val bigIntP =
    maybeNeg(
      ((digit1 ~ Parser.rep(digit)).void)
        .orElse1(Parser.expect("0"))
    )
    .map(BigInt(_))

  property("test an example with BigInt") {
    forAll { bi: BigInt =>
      parseTest(bigIntP, bi.toString, bi)
    }
  }

  property("Parser.start and end work") {
    forAll { (s: String) =>
      if (s.isEmpty) {
        intercept[IllegalArgumentException] {
          Parser.expect(s)
        }
      }
      else {
        val pa = Parser.expect(s)
        assertEquals((Parser.start ~ pa ~ Parser.end).void.parse(s), Right(("", ())))
        assert((pa ~ Parser.start).parse(s).isLeft)
        assert((Parser.end ~ pa).parse(s).isLeft)
        assertEquals((Parser.index ~ pa ~ Parser.index).map { case ((s, _), e) => e - s }.parse(s), Right(("", s.length)))
      }

      true
    }
  }

  property("Parser.length succeeds when the string is long enough") {
    forAll { (s: String, len: Int) =>
      if (len < 1) {
        intercept[IllegalArgumentException] {
          Parser.length(len)
        }
      }
      else {
        val pa = Parser.length(len)

        pa.parse(s) match {
          case Right((rest, first)) =>
            if (s.length >= len) {
              assertEquals(s.take(len), first)
              assertEquals(s.drop(len), rest)
            }
            else fail(s"expected to not parse: $rest, $first")
          case Left(Parser.Error.MissedExpectation(Parser.Expectation.Length(off, l, a))) =>
            assertEquals(off, 0)
            assertEquals(l, len)
            assertEquals(a, s.length)
          case Left(other) =>
            fail(s"unexpected error: $other")
        }
      }

      true
    }
  }

  property("voided only changes the result") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.parse(str)
      val r2 = genP.fa.void.parse(str)

      assertEquals(r2, r1.map { case (off, _) => (off, ()) })
    }
  }

  property("expected in errors gives valid offsets") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      genP.fa.parse(str) match {
        case Left(err) =>
          err.offsets.forall { off =>
            (0 <= off) && (off <= str.length)
          }
        case Right(_) => true
      }

    }
  }

  property("oneOf nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen), Gen.listOf(ParserGen.gen), Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      val oneOf1 = Parser.oneOf((genP1 ::: genP2).map(_.fa))
      val oneOf2 = Parser.oneOf(genP1.map(_.fa)).orElse(
        Parser.oneOf(genP2.map(_.fa)))

      assertEquals(oneOf1.parse(str), oneOf2.parse(str))
    }
  }

  property("oneOf1 nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen1), Gen.listOf(ParserGen.gen1), Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      val oneOf1 = Parser.oneOf1((genP1 ::: genP2).map(_.fa))
      val oneOf2 = Parser.oneOf1(genP1.map(_.fa)).orElse1(
        Parser.oneOf1(genP2.map(_.fa))
      )

      assertEquals(oneOf1.parse(str), oneOf2.parse(str))
    }
  }

  property("string can be recovered with index") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.string.parse(str)
      val r2 = (genP.fa ~ Parser.index).map { case (_, end) => str.substring(0, end) }.parse(str)

      assertEquals(r1, r2)
    }
  }

  property("backtrack orElse pure always succeeds") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.orElse(Parser.pure(())): Parser[Any]

      assert(p1.parse(str).isRight)
    }
  }

  property("backtrack.? pure always succeeds") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.?

      assert(p1.parse(str).isRight)
    }
  }

  test("range messages seem to work") {
    val pa = Parser.charIn1('0' to '9')
    assertEquals(pa.parse("z").toString, "Left(MissedExpectation(InRange(0,0,9)))")
  }

  test("partial parse fails in rep") {
    val partial = Parser.length(1) ~ Parser.fail
    // we can't return empty list here
    assert(partial.rep.parse("foo").isLeft)

    val p2 = Parser.expect("f").orElse1((Parser.expect("boo") ~ Parser.expect("p")).void)
    assert(p2.rep1.parse("fboop").isRight)
    assert(p2.rep1(2).parse("fboop").isRight)
    assert(p2.rep1(3).parse("fboop").isLeft)
    assert(p2.rep1.parse("fboof").isLeft)
  }

  test("defer does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser].defer {
      cnt += 1
      Parser.expect("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }

  test("defer1 does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser1].defer {
      cnt += 1
      Parser.expect("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }

  property("charIn matches charWhere") {
    forAll { (cs: List[Char], str: String) =>
      val cset = cs.toSet
      val p1 = Parser.charIn(cs)
      val p2 = Parser.charWhere(cset)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("charIn1 matches charWhere1") {
    forAll { (c0: Char, cs0: List[Char], str: String) =>
      val cs = c0 :: cs0
      val cset = cs.toSet
      val p1 = Parser.charIn1(cs)
      val p2 = Parser.charWhere1(cset)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("charIn1 matches charIn varargs") {
    forAll { (c0: Char, cs0: List[Char], str: String) =>
      val cs = c0 :: cs0
      val p1 = Parser.charIn1(cs)
      val p2 = Parser.charIn(c0, cs0: _*)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("Parser.end gives the right error") {
    forAll { str: String =>
      Parser.end.parse(str) match {
        case Right((rest, _)) =>
          assertEquals(str, "")
          assertEquals(rest, "")
        case Left(Parser.Error.MissedExpectation(Parser.Expectation.EndOfString(off, len))) =>
          assertEquals(off, 0)
          assertEquals(len, str.length)
        case other =>
          fail(s"unexpected failure: $other")
      }
    }
  }

  property("rep can be reimplemented with oneOf and defer") {
    forAll(ParserGen.gen1, Arbitrary.arbitrary[String]) { (genP, str) =>
      def rep[A](pa: Parser1[A]): Parser[List[A]] =
        Defer[Parser].fix[List[A]] { tail =>
          (pa ~ tail)
            .map { case (h, t) => h :: t }
            .orElse(Parser.pure(Nil))
        }

      val lst1 = rep(genP.fa)
      val lst2 = genP.fa.rep

      assertEquals(lst1.parse(str), lst2.parse(str))
    }
  }

  property("MonoidK[Parser].empty never succeeds") {
    forAll { str: String =>
      assert(MonoidK[Parser].empty.parse(str).isLeft)
      assert(MonoidK[Parser1].empty.parse(str).isLeft)
    }
  }

  property("Monad.pure is an identity function") {
    forAll { (i: Int, str: String) =>
      assertEquals(Monad[Parser].pure(i).parse(str), Right((str, i)))
    }
  }
}
