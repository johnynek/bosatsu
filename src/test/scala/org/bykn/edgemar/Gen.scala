package org.bykn.edgemar

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}

object Generators {
  val lower: Gen[Char] = Gen.oneOf('a' to 'z')
  val upper: Gen[Char] = Gen.oneOf('A' to 'Z')
  val num: Gen[Char] = Gen.oneOf('0' to '9')
  val identC: Gen[Char] = Gen.frequency((10, lower), (1, upper), (1, num))

  val lowerIdent: Gen[String] =
    for {
      c <- lower
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString

  val upperIdent: Gen[String] =
    for {
      c <- upper
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString

  val typeRefGen: Gen[TypeRef] = {
    import TypeRef._

    val tvar = lowerIdent.map(TypeVar(_))
    val tname = upperIdent.map(TypeName(_))

    val tApply =
      for {
        e <- Gen.oneOf(tvar, tname)
        cnt <- Gen.choose(1, 10)
        args <- Gen.listOfN(cnt, Gen.lzy(typeRefGen))
        nel = NonEmptyList.fromListUnsafe(args)
      } yield TypeApply(e, nel)

    Gen.frequency(
      (4, tvar),
      (4, tname),
      (1, Gen.zip(Gen.lzy(typeRefGen), Gen.lzy(typeRefGen)).map { case (a, b) => TypeArrow(a, b) }),
      (1, tApply))
  }

  def commentGen(dec: Gen[Declaration]): Gen[Declaration.Comment] = {
    import Declaration._
    def cleanNewLine(s: String): String = s.map { c => if (c == '\n') ' ' else c }
    for {
      c1 <- Arbitrary.arbitrary[String]
      cs <- Gen.listOf(Arbitrary.arbitrary[String])
      n <- Gen.choose(0, 10)
      rest0 <- dec
      rest = rest0 match { case Comment(_, _, _) => EndOfFile; case o => o }
    } yield Comment(NonEmptyList(cleanNewLine(c1), cs.map(cleanNewLine _)), n, rest)
  }

  def genDeclaration(depth: Int): Gen[Declaration] = {
    import Declaration._

    val unnested = Gen.oneOf(
      lowerIdent.map(Var(_)),
      Arbitrary.arbitrary[BigInt].map { bi => LiteralInt(bi.toString) },
      Gen.oneOf(true, false).map { b => LiteralBool(b) },
      Gen.const(EndOfFile))

    if (depth <= 0) unnested
    else Gen.frequency((4, unnested), (1, commentGen(Gen.lzy(genDeclaration(depth - 1)))))
  }
}
