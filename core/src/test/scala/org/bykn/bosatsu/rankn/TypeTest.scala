package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks.forAll
import org.scalatest.FunSuite
import org.bykn.bosatsu.Generators

object NTypeGen {
  val genRootType: Gen[Type] = {
    val genConst =
      Gen.zip(Generators.packageNameGen, Generators.upperIdent)
        .map { case (p, n) => Type.TyConst(Type.Const.Defined(p, n)) }

    val genVar =
      Generators.lowerIdent.map { v => Type.TyVar(Type.Var.Bound(v)) }

    Gen.oneOf(genVar, genConst)
  }

  def allTypesIn(t: Type): List[Type] =
    t match {
      case f@Type.ForAll(_, in) => f :: allTypesIn(in)
      case t@Type.TyApply(a, b) => t :: allTypesIn(a) ::: allTypesIn(b)
      case other => other :: Nil
    }

  def genDepth(d: Int): Gen[Type] =
    if (d <= 0) genRootType
    else {
      val recurse = Gen.lzy(genDepth(d - 1))
      val args = Generators.lowerIdent.map { v => Type.Var.Bound(v) }
      val genForAll =
        for {
          c <- Gen.choose(1, 5)
          as <- Gen.listOfN(c, args)
          in <- recurse
        } yield Type.forAll(as, in)

      val genApply = Gen.zip(recurse, recurse).map { case (a, b) => Type.TyApply(a, b) }

      Gen.oneOf(recurse, genApply, genForAll)
    }


  val genDepth03: Gen[Type] = Gen.choose(0, 3).flatMap(genDepth(_))
}

class TypeTest extends FunSuite {

  test("free vars are not duplicated") {
    forAll(Gen.listOf(NTypeGen.genDepth03)) { ts =>
      val frees = Type.freeTyVars(ts)
      assert(frees.distinct == frees)
    }
  }

  test("types are well ordered") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03, NTypeGen.genDepth03) {
      org.bykn.bosatsu.OrderingLaws.law(_, _, _)
    }
  }

  test("test all binders") {
    assert(Type.allBinders.filter(_.name.startsWith("a")).take(100).map(_.name) ==
      ("a" #:: Stream.iterate(0)(_ + 1).map { i => s"a$i" }).take(100))
  }

  test("tyVarBinders is identity for Bound") {
    forAll(Gen.listOf(Generators.lowerIdent), NTypeGen.genDepth03) { (vs, t) =>
      val vsD = vs.distinct
      val bs = vsD.map(Type.Var.Bound(_))
      val fa = Type.forAll(bs, t)
      val binders = Type.tyVarBinders(List(fa))
      assert(bs.toSet.subsetOf(binders))
    }
  }

  test("if Type.hasNoVars then freeVars is empty") {
    forAll(NTypeGen.genDepth03) { t =>
      if (Type.hasNoVars(t)) assert(Type.freeTyVars(t :: Nil).isEmpty)
      else ()
    }
  }

  test("hasNoVars fully recurses") {
    forAll(NTypeGen.genDepth03) { t =>
      val allT = NTypeGen.allTypesIn(t)
      val hnv = Type.hasNoVars(t)

      if (hnv) assert(allT.forall(Type.hasNoVars), "hasNoVars == true")
      else assert(allT.exists { t => !Type.hasNoVars(t) }, "hasNoVars == false")
    }
  }

  test("Type.freeTyVars is empty for ForAll fully bound") {
    val ba = Type.Var.Bound("a")
    val fa = Type.ForAll(NonEmptyList.of(ba), Type.TyVar(ba))
    assert(Type.freeTyVars(fa :: Nil).isEmpty)

    val fb = Type.ForAll(NonEmptyList.of(ba), Type.Fun(Type.TyVar(ba), Type.IntType))
    assert(Type.freeTyVars(fb :: Nil).isEmpty)
  }

  test("Type.freeTyVars is left to right for functions") {
    val ba = Type.Var.Bound("a")
    val bb = Type.Var.Bound("b")

    val ta = Type.TyVar(ba)
    val tb = Type.TyVar(bb)
    val fb = Type.Fun(ta, tb)
    assert(Type.freeTyVars(fb :: ta :: Nil) == List(ba, bb))
    assert(Type.freeTyVars(fb :: tb :: Nil) == List(ba, bb))
  }
}
