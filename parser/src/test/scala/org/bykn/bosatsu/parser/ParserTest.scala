package org.bykn.bosatsu.parser

import cats.{Eq, Id, Functor}
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

  val expects: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser.fail: Parser[Unit])
      else GenT(Parser.expect(str))
    }

  def void(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.void(g.fa))

  def string(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser.string(g.fa))

  def backtrack(g: GenT[Parser]): GenT[Parser] =
    GenT(g.fa.backtrack)(g.cogen)

  def product(ga: GenT[Parser], gb: GenT[Parser]): GenT[Parser] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    GenT[Parser, (ga.A, gb.A)](ga.fa ~ gb.fa)
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

  def orElse(ga: GenT[Parser], gb: GenT[Parser], res: GenT[Gen]): Gen[GenT[Parser]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).map { case (f1, f2) =>
      GenT(ga.fa.map(f1).orElse(gb.fa.map(f2)))
    }
  }

  // Generate a random parser
  lazy val gen: Gen[GenT[Parser]] = {
    val rec = Gen.lzy(gen)

    Gen.frequency(
      (3, pures.flatMap(_.toId).map(_.transform(new FunctionK[Id, Parser] {
        def apply[A](g: Id[A]): Parser[A] = Parser.pure(g)
      }))),
     (3, expects),
     (1, rec.map(void(_))),
     (1, rec.map(string(_))),
     (1, rec.map(backtrack(_))),
     (1, rec.flatMap(mapped(_))),
     (1, Gen.zip(rec, rec).map { case (g1, g2) => product(g1, g2) }),
     (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse(g1, g2, p) })
    )
  }

}

class ParserTest extends munit.ScalaCheckSuite {

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
    Parser.oneOf1(
      (Parser.expect("-") ~ p1) ::
      p1 ::
      Nil).string

  val bigIntP =
    maybeNeg(Parser.oneOf1(
      (digit1 ~ Parser.rep(digit)) ::
      Parser.expect("0") ::
      Nil
    ))
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

  test("range messages seem to work") {
    val pa = Parser.charIn1('0' to '9')
    assertEquals(pa.parse("z").toString, "Left(MissedExpectation(InRange(0,0,9)))")
  }
}
