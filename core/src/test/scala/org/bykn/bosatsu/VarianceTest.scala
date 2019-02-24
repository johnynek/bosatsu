package org.bykn.bosatsu

import cats.Id
import org.bykn.bosatsu.rankn.Type
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks.forAll
import org.scalatest.FunSuite

object VarianceGen {
  val gen: Gen[Variance] = Gen.oneOf(
    Variance.Phantom,
    Variance.Contravariant,
    Variance.Covariant,
    Variance.Invariant)

  implicit val arbVar: Arbitrary[Variance] = Arbitrary(gen)
}

class VarianceTest extends FunSuite {
  import VarianceGen.arbVar

  val V = Variance.varianceBoundedSemilattice

  test("variance is commutative") {
    forAll { (v1: Variance, v2: Variance) =>
      assert(V.combine(v1, v2) == V.combine(v2, v1))
    }
  }

  test("variance combine is associative") {
    forAll { (v1: Variance, v2: Variance, v3: Variance) =>
      assert(V.combine(v1, V.combine(v2, v3)) == V.combine(V.combine(v1, v2), v3))
    }
  }

  test("variance * is associative") {
    forAll { (v1: Variance, v2: Variance, v3: Variance) =>
      assert(((v1 * v2) * v3) == (v1 * (v2 * v3)))
    }
  }

  test("combine matches combineAll") {
    forAll { (vs: List[Variance]) =>
      assert(V.combineAllOption(vs) == vs.reduceOption(_ + _))
    }
  }

  test("variance is distributive") {
    forAll { (v1: Variance, v2: Variance, v3: Variance) =>
      val left =  v1 * (v2 + v3)
      val right = (v1 * v2) + (v1 * v3)
      assert(left == right, s"$left != $right")
    }

    // previous failures:
    {
      val v1 = Variance.in
      val v2 = Variance.phantom
      val v3 = Variance.co

      val left =  v1 * (v2 + v3)
      val right = (v1 * v2) + (v1 * v3)
      assert(left == right, s"$left != $right")
    }
  }

  test("negate is the same as muliplying by contra") {
    forAll { (v1: Variance) =>
      assert(-v1 == (Variance.contra * v1))
    }
  }

  test("double negation is identity") {
    forAll { (v1: Variance) =>
      assert(-(-v1) == v1)
    }
  }

  test("times is commutative") {
    forAll { (v1: Variance, v2: Variance) =>
      assert((v1 * v2) == (v2 * v1))
    }
  }

  test("variance is idempotent") {
    forAll { (v1: Variance) =>
      assert(V.combine(v1, v1) == v1)
    }
  }

  test("phantom is bottom") {
    assert(V.empty == Variance.Phantom)
    forAll { (v1: Variance) =>
      assert(V.combine(V.empty, v1) == v1)
      assert(V.combine(v1, V.empty) == v1)
    }
  }

  test("invariant is top") {
    forAll { (v1: Variance) =>
      assert(V.combine(Variance.Invariant, v1) == Variance.Invariant)
      assert(V.combine(v1, Variance.Invariant) == Variance.Invariant)
    }
  }

  test("negate combine gives either Phantom or Invariant") {
    forAll { (v1: Variance) =>
      if (v1 == Variance.Phantom) assert(-v1 == v1)
      else {
        assert(V.combine(-v1, v1) == Variance.Invariant)
        assert(V.combine(v1, -v1) == Variance.Invariant)
      }
    }
  }

  test("covariant combines to get either covariant or invariant") {
    assert(V.combine(Variance.Covariant, Variance.Contravariant) == Variance.Invariant)
    val results = Set(Variance.co, Variance.in)
    forAll { (v1: Variance) =>
      assert(results(V.combine(v1, Variance.Covariant)))
    }
  }

  test("contravariant combines to get either contravariant or invariant") {
    assert(V.combine(Variance.Covariant, Variance.Contravariant) == Variance.Invariant)
    val results = Set(Variance.contra, Variance.in)
    forAll { (v1: Variance) =>
      assert(results(V.combine(v1, Variance.Contravariant)))
    }
  }

  test("some example variance inferences") {
    val x = Type.Var.Bound("x")
    val y = Type.Var.Bound("y")

    val xv = Variance.varianceOf[Id](x, Type.Fun(Type.TyVar(x), Type.TyVar(y))) {
      case fn if Type.TyConst(fn) == Type.FnType => Some(Stream(Variance.contra, Variance.co))
      case _ => None
    }

    assert(xv == Some(Variance.contra))

    val yv = Variance.varianceOf[Id](y, Type.Fun(Type.TyVar(x), Type.TyVar(y))) {
      case fn if Type.TyConst(fn) == Type.FnType => Some(Stream(Variance.contra, Variance.co))
      case _ => None
    }

    assert(yv == Some(Variance.co))

    val xxv = Variance.varianceOf[Id](x, Type.Fun(Type.TyVar(x), Type.TyVar(x))) {
      case fn if Type.TyConst(fn) == Type.FnType => Some(Stream(Variance.contra, Variance.co))
      case _ => None
    }

    assert(xxv == Some(Variance.in))

    val phantomv = Variance.varianceOf[Id](y, Type.Fun(Type.TyVar(x), Type.TyVar(x))) {
      case fn if Type.TyConst(fn) == Type.FnType => Some(Stream(Variance.contra, Variance.co))
      case _ => None
    }

    assert(phantomv == Some(Variance.phantom))

    val f2v = Variance.varianceOf[Id](x, Type.Fun(Type.Fun(Type.TyVar(x), Type.TyVar(y)), Type.TyVar(y))) {
      case fn if Type.TyConst(fn) == Type.FnType => Some(Stream(Variance.contra, Variance.co))
      case _ => None
    }

    assert(f2v == Some(Variance.co))

    val tup = Type.Const.predef("Tup")
    val ftv = Variance.varianceOf[Id](x,
        Type.TyApply(
          Type.TyApply(
            Type.TyConst(tup),
              Type.Fun(Type.TyVar(x), Type.TyVar(y))),
              Type.TyVar(x))) {
      case fn if Type.TyConst(fn) == Type.FnType => Some(Stream(Variance.contra, Variance.co))
      case t if t == tup => Some(Stream(Variance.co, Variance.co))
      case _ => None
    }

    assert(ftv == Some(Variance.in))
  }
}
