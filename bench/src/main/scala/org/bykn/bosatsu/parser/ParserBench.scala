package org.bykn.bosatsu.parser

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class ParserBench {

  val rng: java.util.Random = new java.util.Random(42)

  val digits: IndexedSeq[String] =
    (0 until 1000000).map { _ => BigInt(rng.nextInt()) * BigInt(rng.nextInt()) * (if (rng.nextBoolean()) -1 else 1) }.map(_.toString)

  object FP {
    import fastparse.all._

    val digit = CharIn('0' to '9')
    val digit1 = CharIn('1' to '9')
    def maybeNeg[A](p1: P[A]): P[String] =
      P(("-" ~ p1) | p1).!

    val bigIntP =
      maybeNeg(
        (digit1 ~ digit.rep()) | P("0")
      )
      .map(BigInt(_))
  }

  object Local {
    val digit = Parser.charIn('0' to '9')
    val digit1 = Parser.charIn('1' to '9')
    def maybeNeg[A](p1: Parser1[A]): Parser1[String] =
      Parser.oneOf1(
        (Parser.string1("-") ~ p1) ::
        p1 ::
        Nil).string

    val bigIntP =
      maybeNeg(Parser.oneOf1(
        (digit1 ~ Parser.rep(digit)) ::
        Parser.char('0') ::
        Nil
      ))
      .map(BigInt(_))
  }

  @Benchmark def benchBigIntsFP(): Unit = {
    digits.foreach { d =>
      FP.bigIntP.parse(d)
    }
  }

  @Benchmark def benchBigIntsLocal(): Unit = {
    digits.foreach { d =>
      Local.bigIntP.parse(d)
    }
  }
}
