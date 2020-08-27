package org.bykn.bosatsu

import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.bykn.bosatsu.rankn.Type

class TypeRefTest extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 500000)
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
      val tpe = TypeRefConverter[cats.Id](tr) { c => Type.Const.Defined(pn, TypeName(c)) }
      val tr1 = TypeRef.fromTypes(Some(pn), tpe :: Nil)(tpe)
      assert(tr1 == tr.normalizeForAll)
    }
  }
}
