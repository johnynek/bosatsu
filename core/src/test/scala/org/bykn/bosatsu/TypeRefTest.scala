package org.bykn.bosatsu

import org.scalacheck.Prop.forAll
import org.bykn.bosatsu.rankn.Type

class TypeRefTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 500000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(5000)

  import Generators.{typeRefGen, shrinkTypeRef}

  def show(t: TypeRef): String =
    TypeRef.document.document(t).render(80)

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
        _ => None,
        _ => None,
        {
          case Type.Const.Defined(p, t) if p == pn => Some(TypeRef.TypeName(t))
          case _                                   => None
        }
      )

      assert(tr1 == Some(tr.normalizeForAll), s"tpe = $tpe")
    }
  }

  test("normalizeAll is idempotent") {
    forAll(typeRefGen) { tr =>
      val norm1 = tr.normalizeForAll
      val norm2 = norm1.normalizeForAll
      assert(norm2 == norm1, s"${show(norm2)} != ${show(norm1)}")
    }
  }
}
