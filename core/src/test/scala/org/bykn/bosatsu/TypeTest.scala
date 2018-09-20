package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.{Gen, Arbitrary}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.forAll


import Type._

object TypeGen {
  val genRoot: Gen[Type] = {
    val pn = Gen.choose(1, 5).flatMap { s =>
      Gen.listOfN(s, Gen.identifier)
        .map {
          case (h :: tail) => PackageName(NonEmptyList.of(h, tail :_*))
          case Nil => sys.error("unreachable")
        }
    }

    val decl = pn.flatMap { p => Gen.identifier.map(Type.Declared(p, _)) }
    val vart = Gen.oneOf('a' to 'z').flatMap { first => Gen.identifier.map { tail => s"$first$tail" } }.map(Type.Var(_))
    Gen.oneOf(decl, vart)
  }

  def genDepth(d: Int): Gen[Type] =
    if (d <= 0) genRoot
    else {
      val smaller = Gen.lzy(genDepth(d - 1))
      val arrow = Gen.zip(smaller, smaller).map { case (a, b) => Type.Arrow(a, b) }
      val tap = Gen.zip(smaller, smaller).map { case (a, b) => Type.TypeApply(a, b) }

      val lambda = smaller.flatMap { inner =>
        val freeVars = inner.varsIn.map(_.name)
        val nonFree = Gen.identifier
        val genVar =
          if (freeVars.isEmpty) nonFree
          else Gen.oneOf(freeVars)

        genVar.map(Type.TypeLambda(_, inner))
      }

      Gen.oneOf(arrow, tap, lambda)
    }

  implicit val arbType: Arbitrary[Type] =
    Arbitrary(Gen.oneOf(0 to 3).flatMap(genDepth(_)))
}

class TypeTest extends FunSuite {
  import TypeGen._

  test("simple varsIn examples") {
    val varA = Var("a")
    val varB = Var("b")
    assert(varA.varsIn == List(varA))
    assert(Substitutable[Type].typeVars(varA) == Set("a"))
    assert(Arrow(varA, varB).varsIn == List(varA, varB))
    assert(Substitutable[Type].typeVars(Arrow(varA, varB)) == Set("a", "b"))
    // a is bound and not a free variable here
    assert(TypeLambda("a", varA).varsIn == Nil)
    assert(Substitutable[Type].typeVars(TypeLambda("a", varA)).isEmpty)
    // b is a free variable
    assert(TypeLambda("a", varB).varsIn == List(varB))
    assert(Substitutable[Type].typeVars(TypeLambda("a", varB)) == Set("b"))
  }

  test("varsIn matches typeVars") {
    forAll { t: Type =>
      assert(t.varsIn.map(_.name).toSet == Substitutable[Type].typeVars(t))
    }
  }

  test("normalize is an idempotent pure function") {
    forAll { t: Type =>
      val normt = normalize(t)
      assert(normt == normalize(t))
      assert(normt == normalize(normt))
    }
  }

  test("free type vars don't change under normalize") {
    forAll { t: Type =>
      assert(normalize(t).varsIn == t.varsIn)
    }
  }

  test("simplify can only remove unused free variables") {
    def law(t: Type) =
      assert(simplifyApply(t).varsIn.toSet.subsetOf(t.varsIn.toSet))

    forAll(law _)

    val regressions =
      List(
        TypeApply(TypeLambda("a", Var("b")), Var("c"))
      )

    regressions.foreach(law _)
  }

  test("simple normalization examples") {
    assert(normalize(Var("a")) == Var("a"))
    assert(normalize(TypeLambda("z", Var("z"))) == TypeLambda("a", Var("a")))
    assert(normalize(TypeLambda("z", Var("b"))) == TypeLambda("a", Var("b")))
    assert(normalize(TypeLambda("z", TypeLambda("q", Var("b")))) == TypeLambda("a", TypeLambda("c", Var("b"))))
    assert(normalize(Arrow(Var("a"), TypeLambda("z", Var("b")))) == Arrow(Var("a"), TypeLambda("c", Var("b"))))
  }

  test("apply lambda simplifies") {
    assert(simplifyApply(TypeApply(TypeLambda("a", Var("a")), Var("b"))) == Var("b"))
  }

  test("simplifyApply is an idempotent pure function") {
    forAll { t: Type =>
      val appt = simplifyApply(t)
      assert(appt == simplifyApply(t))
      assert(appt == simplifyApply(appt))
    }
  }

  test("simplifyApply removes all cases of apply on lambda") {
    def isGood(t: Type): Boolean =
      t match {
        case Declared(_,_) | Var(_) => true
        case TypeApply(TypeLambda(_, _), _) => sys.error(t.toString)
        case TypeApply(a, b) => isGood(a) && isGood(b)
        case TypeLambda(_, t) => isGood(t)
        case Arrow(a, b) => isGood(a) && isGood(b)
      }
    forAll { t: Type =>
      assert(isGood(simplifyApply(t)))
    }

    val regressions = List(
      TypeApply(
        TypeApply(
          TypeLambda("gngm5rnpqeii8",Var("gngm5rnpqeii8")),
          TypeLambda("o1vmshpsf",Declared(PackageName(NonEmptyList.of("xJUgsavuLrqdfma1bfLxkp7byaw")),"zbcg8neK0x"))),
        TypeLambda("dc5rnic6mhogrrnOoqh",
          TypeApply(Var("dc5rnic6mhogrrnOoqh"),Declared(PackageName(NonEmptyList.of("hdiilmruzbjzRex")),"knxeCajMsm8fpdcrnDgfs1yarx")))))

    regressions.foreach { t => assert(isGood(simplifyApply(t))) }

  }

  test("Scheme is isomorphic to Type") {
    forAll { t: Type =>
      val scheme = Scheme.fromType(t)

      assert(scheme.toType == simplifyApply(t))
    }
  }

  test("Scheme knows all the free vars in Type") {
    forAll { t: Type =>
      assert(Scheme.typeConstructor(t).toType.varsIn.isEmpty)
    }
  }
}
