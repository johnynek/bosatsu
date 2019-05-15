package org.bykn.bosatsu.rankn

import org.scalacheck.Gen
import org.bykn.bosatsu.Generators

object NTypeGen {
  val genRootType: Gen[Type] = {
    val genConst =
      Gen.zip(Generators.packageNameGen, Generators.typeNameGen)
        .map { case (p, n) => Type.TyConst(Type.Const.Defined(p, n)) }

    val genVar =
      Generators.lowerIdent.map { v => Type.TyVar(Type.Var.Bound(v)) }

    Gen.oneOf(genVar, genConst)
  }

  val genBound: Gen[Type.Var.Bound] =
    Generators.lowerIdent.map { v => Type.Var.Bound(v) }

  def genDepth(d: Int): Gen[Type] =
    if (d <= 0) genRootType
    else {
      val recurse = Gen.lzy(genDepth(d - 1))
      val genForAll =
        for {
          c <- Gen.choose(1, 5)
          as <- Gen.listOfN(c, genBound)
          in <- recurse
        } yield Type.forAll(as, in)

      val genApply = Gen.zip(recurse, recurse).map { case (a, b) => Type.TyApply(a, b) }

      Gen.oneOf(recurse, genApply, genForAll)
    }


  val genDepth03: Gen[Type] = Gen.choose(0, 3).flatMap(genDepth(_))
}
