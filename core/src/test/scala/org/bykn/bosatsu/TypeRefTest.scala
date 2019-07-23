package org.bykn.bosatsu

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
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
    val srcConv = new SourceConverter(
      { c => Type.Const.Defined(pn, TypeName(c)) },
      { c => (pn, c) })
    forAll(typeRefGen) { tr =>
      val tpe = srcConv.toType(tr)
      val tr1 = TypeRef.fromTypes(Some(pn), tpe :: Nil)(tpe)
      assert(tr1 == tr.normalizeForAll)
    }
  }
}
