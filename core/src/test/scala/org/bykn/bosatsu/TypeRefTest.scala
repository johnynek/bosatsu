package org.bykn.bosatsu

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.bykn.bosatsu.rankn.Type
import org.scalatest.funsuite.AnyFunSuite

class TypeRefTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 500000)
    PropertyCheckConfiguration(minSuccessful = 5000)

  import Generators.{typeRefGen, shrinkTypeRef}

  test("Ordering is lawful") {
    forAll(typeRefGen, typeRefGen, typeRefGen) { (a, b, c) =>
      OrderingLaws.law(a, b, c)
    }
  }

  test("TypeRef -> Type -> TypeRef") {
    val pn = PackageName.parts("Test")

    forAll(typeRefGen) { tr =>
      val tpe = TypeRefConverter[cats.Id](tr) { c =>
        Type.Const.Defined(pn, TypeName(c))
      }
      val tr1 = TypeRefConverter.fromTypeA[Option](
        tpe,
        { _ => None },
        { _ => None },
        {
          case Type.Const.Defined(p, t) if p == pn => Some(TypeRef.TypeName(t))
          case _                                   => None
        }
      )

      assert(tr1 == Some(tr.normalizeForAll))
    }
  }
}
