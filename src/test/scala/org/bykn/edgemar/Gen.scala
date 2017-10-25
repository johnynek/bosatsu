package org.bykn.edgemar

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}

object Generators {
  val lower: Gen[Char] = Gen.oneOf('a' to 'z')
  val upper: Gen[Char] = Gen.oneOf('A' to 'Z')
  val num: Gen[Char] = Gen.oneOf('0' to '9')
  val identC: Gen[Char] = Gen.frequency((10, lower), (1, upper), (1, num))

  def nonEmpty[T](t: Gen[T]): Gen[NonEmptyList[T]] =
    for {
      h <- t
      tail <- Gen.listOf(t)
    } yield NonEmptyList(h, tail)

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

  def commentGen[T](dec: Gen[T]): Gen[CommentStatement[T]] = {
    def cleanNewLine(s: String): String = s.map { c => if (c == '\n') ' ' else c }
    for {
      cs <- nonEmpty(Arbitrary.arbitrary[String])
      t <- dec
    } yield CommentStatement(cs.map(cleanNewLine _), t)
  }

  def defGen[T](dec: Gen[T]): Gen[DefStatement[T]] = {
    val argGen =
      for {
        arg <- lowerIdent
        t <- Gen.option(typeRefGen)
      } yield (arg, t)

    for {
      name <- lowerIdent
      args <- nonEmpty(argGen)
      retType <- Gen.option(typeRefGen)
      body <- dec
    } yield DefStatement(name, args, retType, body)
  }

  def opGen(dec: Gen[Declaration]): Gen[Declaration] = {
    import Declaration._
    for {
      op <- Gen.oneOf(Operator.allOps)
      left <- dec
      right <- dec
    } yield Op(Parens(left), op, Parens(right))
  }

  def applyGen(dec: Gen[Declaration]): Gen[Declaration] = {
    import Declaration._
    Gen.lzy(for {
      fn <- dec
      args <- nonEmpty(dec)
    } yield Apply(fn, args))
  }

  def bindGen[T](dec: Gen[Declaration], tgen: Gen[T]): Gen[BindingStatement[T]] =
    for {
      b <- lowerIdent
      value <- dec
      in <- tgen
    } yield BindingStatement(b, value, in)

  def padding[T](tgen: Gen[T], min: Int = 0): Gen[Padding[T]] =
    Gen.zip(Gen.choose(min, 10), tgen)
      .map { case (e, t) => Padding(e, t) }

  def indented[T](tgen: Gen[T]): Gen[Indented[T]] =
    for {
      cnt <- Gen.choose(1, 16)
      t <- tgen
    } yield Indented(cnt, t)

  def lambdaGen(bodyGen: Gen[Declaration]): Gen[Declaration.Lambda] =
    for {
      args <- nonEmpty(lowerIdent)
      body <- bodyGen
    } yield Declaration.Lambda(args, body)

  def genDeclaration(depth: Int): Gen[Declaration] = {
    import Declaration._

    val unnested = Gen.oneOf(
      lowerIdent.map(Var(_)),
      Arbitrary.arbitrary[BigInt].map { bi => LiteralInt(bi.toString) },
      Gen.oneOf(true, false).map { b => LiteralBool(b) })

    val recur = Gen.lzy(genDeclaration(depth - 1))
    if (depth <= 0) unnested
    else Gen.frequency(
      (3, unnested),
      (1, commentGen(padding(recur, 1)).map(Comment(_))), // make sure we have 1 space to prevent comments following each other
      (1, defGen(Gen.zip(padding(indented(recur)), padding(recur))).map(DefFn(_))),
      (1, opGen(recur)),
      (1, lambdaGen(recur))
      //(1, applyGen(recur))
      //(1, bindGen(Gen.oneOf(opGen(recur), lowerIdent.map(Var(_)), applyGen(recur))))
    )
  }
}
