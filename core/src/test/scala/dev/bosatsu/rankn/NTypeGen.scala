package dev.bosatsu
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
    upperIdent.map(n => Identifier.Constructor(n))

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
    "exists",
    "recur",
    "recursive"
  )

  val lowerIdent: Gen[String] =
    (for {
      c <- lower
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString).filter(s => !keyWords(s))

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
    lowerIdent.map(v => Type.Var.Bound(v))

  def genRootType(genC: Option[Gen[Type.Const]]): Gen[Type.Leaf] = {
    val b = genBound.map(Type.TyVar(_))
    genC match {
      case None     => b
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
    def shrink(k: Kind): LazyList[Kind] =
      k match {
        case Kind.Type                    => LazyList.empty
        case Kind.Cons(Kind.Arg(_, a), b) =>
          a #:: b #:: LazyList.empty
      }

    Shrink.withLazyList(shrink)
  }

  implicit val shrinkType: Shrink[Type] = {
    import Type._
    def shrink(t: Type): LazyList[Type] =
      t match {
        case ForAll(items, in) =>
          Shrink
            .shrink((items.toList, in))
            .to(LazyList)
            .map { case (it, in) => Type.forAll(it, in) }
        case Exists(items, in) =>
          Shrink
            .shrink((items.toList, in))
            .to(LazyList)
            .map { case (it, in) => Type.exists(it, in) }
        case _: Leaf          => LazyList.empty
        case TyApply(on, arg) =>
          Shrink
            .shrink((on, arg))
            .to(LazyList)
            .collect { case (on: Type.Rho, arg) => TyApply(on, arg) }
      }
    Shrink.withLazyList(shrink)
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
    val t2 = List(FnType(1), Tuple.Arity(2), DictType)

    lazy val tupleTypes: Gen[Type] = {
      val recTup = Gen.lzy(tupleTypes)

      // either Unit, Tuple2(a, b)
      Gen.oneOf(
        Gen.const(UnitType),
        Gen.zip(recurse, recTup).map { case (h, t) =>
          Type.TyApply(Type.TyApply(Tuple.Arity(2), h), t)
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

  def genQuantifiers(
      genB: Gen[Type.Var.Bound]
  ): Gen[NonEmptyList[(Type.Var.Bound, Kind)]] =
    for {
      c <- Gen.choose(1, 5)
      bs0 <- Gen.listOfN(c, genB)
      bs = bs0.distinct
      ks <- Gen.listOfN(bs.length, genKind)
    } yield NonEmptyList.fromListUnsafe(bs.zip(ks))

  val genQuantArgs: Gen[List[(Type.Var.Bound, Kind)]] =
    Gen.frequency(
      (1, Gen.const(Nil)),
      (4, genQuantifiers(genBound).map(_.toList))
    )

  private def genQuantifiersFromFree(
      free: NonEmptyList[Type.Var.Bound]
  ): Gen[NonEmptyList[(Type.Var.Bound, Kind)]] =
    for {
      c <- Gen.choose(1, free.length)
      bs <- Gen.pick(c, free.toList).map(_.toList)
      ks <- Gen.listOfN(bs.length, genKind)
    } yield NonEmptyList.fromListUnsafe(bs.zip(ks))

  private def genRhoWithFreeBound(
      genT: Gen[Type.Rho]
  ): Gen[(Type.Rho, NonEmptyList[Type.Var.Bound])] =
    genT.flatMap { t =>
      NonEmptyList.fromList(Type.freeBoundTyVars(t :: Nil)) match {
        case Some(free) => Gen.const((t, free))
        case None       =>
          genBound.map { b =>
            (Type.TyVar(b): Type.Rho, NonEmptyList.one(b))
          }
      }
    }

  def genForAll(d: Int, genC: Option[Gen[Type.Const]]): Gen[Type.ForAll] = {
    val recurse = Gen.lzy(genTypeRho(d - 1, genC))

    for {
      (in, free) <- genRhoWithFreeBound(recurse)
      qs <- genQuantifiersFromFree(free)
    } yield Type.forAll(qs.toList, in).asInstanceOf[Type.ForAll]
  }

  def genExists(d: Int, genC: Option[Gen[Type.Const]]): Gen[Type.Exists] = {
    val recurse = Gen.lzy(genTypeRho(d - 1, genC))

    for {
      (in, free) <- genRhoWithFreeBound(recurse)
      qs <- genQuantifiersFromFree(free)
    } yield Type.existsRho(qs, in)
  }

  lazy val genQuant: Gen[TypedExpr.Quantification] =
    Gen
      .zip(genQuantArgs, genQuantArgs)
      .flatMap { case (fa, ex0) =>
        val faSet = fa.map(_._1).toSet
        val ex = ex0.filterNot { case (b, _) => faSet(b) }
        TypedExpr.Quantification.fromLists(fa, ex) match {
          case Some(q) => Gen.const(q)
          case None    => genQuant
        }
      }

  def genTypeRho(d: Int, genC: Option[Gen[Type.Const]]): Gen[Type.Rho] = {
    val root = genRootType(genC)
    if (d <= 0) root
    else {
      val recurse = Gen.lzy(genTypeRho(d - 1, genC))
      val genApply = Gen
        .zip(recurse, genDepth(d - 1, genC))
        .map { case (a, b) =>
          Type.apply1Rho(a, b)
        }

      Gen.frequency((3, root), (2, genApply), (1, genExists(d, genC)))
    }
  }

  def genDepth(d: Int, genC: Option[Gen[Type.Const]]): Gen[Type] =
    if (d <= 0) genRootType(genC)
    else {
      val recurse = Gen.lzy(genDepth(d - 1, genC))

      val genQ =
        Gen.zip(genQuantArgs, genQuantArgs, recurse).map { case (fa, ex0, t) =>
          val faSet = fa.map(_._1).toSet
          val ex = ex0.filterNot { case (b, _) => faSet(b) }
          Type.quantify(forallList = fa, existList = ex, t)
        }

      val genApply = Gen.zip(genTypeRho(d - 1, genC), recurse).map {
        case (a, b) => Type.apply1(a, b)
      }

      Gen.frequency(
        (2, recurse),
        (1, genApply),
        (1, Gen.oneOf(genForAll(d, genC), genExists(d, genC), genQ))
      )
    }

  val genDepth03: Gen[Type] =
    Gen.choose(0, 3).flatMap(genDepth(_, Some(genConst)))
}
