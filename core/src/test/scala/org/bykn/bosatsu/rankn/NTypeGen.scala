package org.bykn.bosatsu
package rankn

import cats.data.NonEmptyList
import org.scalacheck.{Gen, Shrink}

object NTypeGen {
  val lower: Gen[Char] = Gen.oneOf('a' to 'z')
  val upper: Gen[Char] = Gen.oneOf('A' to 'Z')
  val num: Gen[Char] = Gen.oneOf('0' to '9')
  val identC: Gen[Char] = Gen.frequency((10, lower), (1, upper), (1, num))

  val upperIdent: Gen[String] =
    for {
      c <- upper
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString

  val consIdentGen: Gen[Identifier.Constructor] =
    upperIdent.map { n => Identifier.Constructor(n) }

  val typeNameGen: Gen[TypeName] =
    consIdentGen.map(TypeName(_))

  val keyWords = Set(
    "if",
    "ffi",
    "match",
    "struct",
    "enum",
    "else",
    "elif",
    "def",
    "external",
    "package",
    "import",
    "export",
    "forall",
    "recur",
    "recursive"
  )

  val lowerIdent: Gen[String] =
    (for {
      c <- lower
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString).filter { s => !keyWords(s) }

  val packageNameGen: Gen[PackageName] =
    for {
      pc <- Gen.choose(1, 5)
      (h, tail) <- Gen.listOfN(pc, upperIdent).map {
        case Nil => sys.error("got an empty list, but import count min is 1")
        case h :: tail => (h, tail)
      }
    } yield PackageName(NonEmptyList(h, tail))

  val genConst: Gen[Type.Const] =
    Gen
      .zip(packageNameGen, typeNameGen)
      .map { case (p, n) => Type.Const.Defined(p, n) }

  val genBound: Gen[Type.Var.Bound] =
    lowerIdent.map { v => Type.Var.Bound(v) }

  def genRootType(genC: Option[Gen[Type.Const]]): Gen[Type] = {
    val b = genBound.map(Type.TyVar(_))
    genC match {
      case None => b
      case Some(gc) =>
        Gen.oneOf(gc.map(Type.TyConst(_)), b)
    }
  }

  val genVariance =
    Gen.oneOf(Variance.co, Variance.in, Variance.contra, Variance.phantom)

  val genKind: Gen[Kind] = {
    val recurse = Gen.lzy(genKind)
    Gen.frequency(
      (
        1,
        Gen.zip(genVariance, recurse, recurse).map { case (v, a, b) =>
          Kind.Cons(a.withVar(v), b)
        }
      ),
      (15, Gen.oneOf(Kind.allKinds.take(100)))
    )
  }

  implicit val shrinkKind: Shrink[Kind] = {
    def shrink(k: Kind): Stream[Kind] =
      k match {
        case Kind.Type => Stream.empty
        case Kind.Cons(Kind.Arg(_, a), b) =>
          a #:: b #:: Stream.empty
      }

    Shrink(shrink(_))
  }

  val genKindArg: Gen[Kind.Arg] =
    Gen.zip(genVariance, genKind).map { case (v, k) => Kind.Arg(v, k) }

  val genPredefType: Gen[Type] = {
    import Type._

    val recurse = Gen.lzy(genPredefType)

    val t0 = List(
      BoolType,
      IntType,
      StrType,
      TestType,
      UnitType
    )

    val t1 = List(ListType, OptionType)
    val t2 = List(FnType, TupleConsType, DictType)

    lazy val tupleTypes: Gen[Type] = {
      val recTup = Gen.lzy(tupleTypes)

      // either Unit, TupleConsType(a, tuple)
      Gen.oneOf(
        Gen.const(UnitType),
        Gen.zip(recurse, recTup).map { case (h, t) =>
          Type.TyApply(Type.TyApply(TupleConsType, h), t)
        }
      )
    }

    Gen.frequency(
      (6, Gen.oneOf(t0)),
      (
        2,
        for {
          cons <- Gen.oneOf(t1)
          param <- recurse
        } yield TyApply(cons, param)
      ),
      (1, tupleTypes),
      (
        1,
        for {
          cons <- Gen.oneOf(t2)
          param1 <- recurse
          param2 <- recurse
        } yield TyApply(TyApply(cons, param1), param2)
      )
    )
  }

  def genDepth(d: Int, genC: Option[Gen[Type.Const]]): Gen[Type] =
    if (d <= 0) genRootType(genC)
    else {
      val recurse = Gen.lzy(genDepth(d - 1, genC))
      val genForAll =
        for {
          c <- Gen.choose(1, 5)
          ks = NTypeGen.genKind
          as <- Gen.listOfN(c, Gen.zip(genBound, ks))
          in <- recurse
        } yield Type.forAll(as, in)

      val genApply =
        Gen.zip(recurse, recurse).map { case (a, b) => Type.TyApply(a, b) }

      Gen.oneOf(recurse, genApply, genForAll)
    }

  val genDepth03: Gen[Type] =
    Gen.choose(0, 3).flatMap(genDepth(_, Some(genConst)))
}
