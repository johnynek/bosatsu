package org.bykn.bosatsu

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.{Gen, Arbitrary}
import org.typelevel.paiges.Document

import cats.parse.{Parser => P, Parser1 => P1, Numbers}

object GenSExpr {
  import Arbitrary.arbitrary

  val genInt: Gen[SExpr[Unit]] =
    arbitrary[BigInt].map(SExpr.Integer(_, ()))

  val genFloat: Gen[SExpr[Unit]] =
    arbitrary[BigDecimal].map(SExpr.Floating(_, ()))

  val genString: Gen[String] = {
    val ascii = Gen.choose(' ', '~')
    val asciiStr = Gen.stringOf(ascii)

    Gen.oneOf(
      asciiStr,
      arbitrary[String]
    )
  }

  val genSym: Gen[SExpr[Unit]] =
    genString
      .map {
        // valid symbols can't start with a number
        // or be empty
        case "" => "_"
        case s if s.charAt(0) == '-' => "_" + s
        case num if Numbers.digit.orElse(Numbers.jsonNumber).parse(num).isRight => "_" + num
        case good => good
      }
      .map(SExpr.Sym(_, ()))

  val genStr: Gen[SExpr[Unit]] =
    genString.map(SExpr.Str(_, ()))

  val genBracket: Gen[SExpr.BracketKind] =
    Gen.oneOf(
      SExpr.BracketKind.Parens,
      SExpr.BracketKind.Square,
      SExpr.BracketKind.Curly)

  def genRepeated(ga: Gen[SExpr[Unit]]): Gen[SExpr.Repeated[Unit]] =
    for {
      cnt <- Gen.choose(0, 4)
      bracket <- genBracket
      lst <- Gen.listOfN(cnt, ga)
    } yield SExpr.Repeated(bracket, lst.toVector, ())

  lazy val genExpr: Gen[SExpr[Unit]] = {
    val rec = Gen.lzy(genExpr)
    Gen.frequency(
      (6, genInt),
      (6, genFloat),
      (6, genSym),
      (5, genStr),
      (1, genRepeated(rec)))
  }
}

class SExprTest extends AnyFunSuite {
  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50000)
    //PropertyCheckConfiguration(minSuccessful = 500)

  val unitTagger: SExpr.Tagger[Unit] =
    new SExpr.Tagger[Unit] {
      def apply[A](pa: P1[A]): P1[(A, Unit)] =
        pa.map((_, ()))
    }

  def roundTripLaw[A: Document](gen: Gen[A], pa: P[A]) =
    forAll(gen, Gen.choose(0, 400)) { (a, width) =>
      val str = Document[A].document(a).render(width)

      pa.parseAll(str) match {
        case Right(a1) => assert(a1 == a)
        case Left(err) => fail(err.toString + " when parsing: " + str)
      }
    }

  def rt(str: String) =
    SExpr.parser(unitTagger).parseAll(str) match {
      case Right(sexpr) =>
        assert(Document[SExpr[Unit]].document(sexpr).render(80) == str)
      case Left(err) =>
        fail(s"$err when parsing: $str")
    }

  test("SExpr round trips") {
    roundTripLaw(GenSExpr.genInt, SExpr.parser(unitTagger))
    roundTripLaw(GenSExpr.genFloat, SExpr.parser(unitTagger))
    roundTripLaw(GenSExpr.genRepeated(GenSExpr.genInt), SExpr.parser(unitTagger))
    roundTripLaw(GenSExpr.genExpr, SExpr.parser(unitTagger))
  }

  test("test some hand written examples") {
    val examples: List[String] =
      "12" ::
      "(foo bar baz)" ::
      "12.4" ::
      "(_- 2 1)" ::
      "[_- 2 1]" ::
      "{_- 2 1}" ::
      "(\\\" 2 1)" ::
      "(\\; 2 1)" ::
      "\"foo\"" ::
      Nil

    examples.foreach(rt)
  }
}
