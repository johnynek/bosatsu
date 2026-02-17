package dev.bosatsu

import org.scalacheck.Prop.forAll
import cats.data.NonEmptyList
import dev.bosatsu.rankn.Type

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

      assertEquals(tr1, Some(tr.normalizeForAll), s"tpe = $tpe")
    }
  }

  test("normalizeAll is idempotent") {
    forAll(typeRefGen) { tr =>
      val norm1 = tr.normalizeForAll
      val norm2 = norm1.normalizeForAll
      assertEquals(norm2, norm1, s"${show(norm2)} != ${show(norm1)}")
    }
  }

  test("freeTypeRefVars are not duplicated") {
    forAll(typeRefGen) { tr =>
      val frees = TypeRef.freeTypeRefVars(tr)
      assertEquals(frees.distinct, frees)
    }
  }

  test("freeTypeRefVars are left-to-right") {
    val a = TypeRef.TypeVar("a")
    val b = TypeRef.TypeVar("b")
    val c = TypeRef.TypeVar("c")
    val d = TypeRef.TypeVar("d")
    val tr =
      TypeRef.TypeArrow(
        NonEmptyList.of(
          a,
          TypeRef.TypeApply(b, NonEmptyList.of(c, a))
        ),
        TypeRef.TypeTuple(List(d, b, c))
      )

    assertEquals(
      TypeRef.freeTypeRefVars(tr),
      List(a, b, c, d)
    )
  }

  test("freeTypeRefVars excludes vars bound by forall/exists") {
    val a = TypeRef.TypeVar("a")
    val b = TypeRef.TypeVar("b")
    val c = TypeRef.TypeVar("c")
    val d = TypeRef.TypeVar("d")
    val tr =
      TypeRef.TypeForAll(
        NonEmptyList.one((a, None)),
        TypeRef.TypeArrow(
          NonEmptyList.of(a, b),
          TypeRef.TypeExists(
            NonEmptyList.one((b, None)),
            TypeRef.TypeTuple(List(b, c, d, c))
          )
        )
      )

    assertEquals(
      TypeRef.freeTypeRefVars(tr),
      List(b, c, d)
    )
  }

  test("normalizeForAll preserves freeTypeRefVars") {
    forAll(typeRefGen) { tr =>
      val frees0 = TypeRef.freeTypeRefVars(tr)
      val frees1 = TypeRef.freeTypeRefVars(tr.normalizeForAll)
      assertEquals(frees1, frees0)
    }
  }
}
