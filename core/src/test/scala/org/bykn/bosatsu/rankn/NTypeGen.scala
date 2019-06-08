package org.bykn.bosatsu.rankn

import org.scalacheck.Gen
import org.bykn.bosatsu.Generators

object NTypeGen {

  val genConst: Gen[Type.Const] =
    Gen.zip(Generators.packageNameGen, Generators.typeNameGen)
      .map { case (p, n) => Type.Const.Defined(p, n) }

  val genBound: Gen[Type.Var.Bound] =
    Generators.lowerIdent.map { v => Type.Var.Bound(v) }

  def genRootType(genC: Option[Gen[Type.Const]]): Gen[Type] = {
    val b = genBound.map(Type.TyVar(_))
    genC match {
      case None => b
      case Some(gc) =>
        Gen.oneOf(gc.map(Type.TyConst(_)), b)
    }
  }

  def genDepth(d: Int, genC: Option[Gen[Type.Const]]): Gen[Type] =
    if (d <= 0) genRootType(genC)
    else {
      val recurse = Gen.lzy(genDepth(d - 1, genC))
      val genForAll =
        for {
          c <- Gen.choose(1, 5)
          as <- Gen.listOfN(c, genBound)
          in <- recurse
        } yield Type.forAll(as, in)

      val genApply = Gen.zip(recurse, recurse).map { case (a, b) => Type.TyApply(a, b) }

      Gen.oneOf(recurse, genApply, genForAll)
    }


  val genDepth03: Gen[Type] = Gen.choose(0, 3).flatMap(genDepth(_, Some(genConst)))
}
