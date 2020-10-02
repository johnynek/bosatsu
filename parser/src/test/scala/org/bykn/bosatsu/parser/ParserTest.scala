package org.bykn.bosatsu.parser

import cats.Eq

import cats.implicits._

class ParserTest extends munit.FunSuite {

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

    parseTest(Parser.oneOf1(fooP, barP :: Nil), "bar", ())
    parseTest(Parser.oneOf1(fooP, barP :: Nil), "foo", ())
  }

  test("product tests") {
    parseTest(Parser.product01(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product10(fooP, barP), "foobar", ((), ()))
    parseTest(Parser.product(fooP, barP), "foobar", ((), ()))
  }
}
