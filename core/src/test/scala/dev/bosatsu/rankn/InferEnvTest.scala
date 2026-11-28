package dev.bosatsu.rankn

import cats.data.NonEmptyList
import dev.bosatsu.{Identifier, Kind, PackageName, TypeName}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class InferEnvTest extends munit.ScalaCheckSuite {

  private def metaType(id: Long): Type.TyMeta =
    Type.TyMeta(
      Type.Meta(Kind.Type, id, existential = id % 2 == 0, level = 0, RefSpace.constRef(None))
    )

  // Closed and open types: generated types (some with free variables), metas
  // on their own, and metas nested inside an application.
  private val genType: Gen[Type] =
    Gen.frequency(
      (4, NTypeGen.genDepth03),
      (1, Gen.choose(1L, 5L).map(metaType)),
      (
        1,
        Gen.zip(Gen.choose(1L, 5L), NTypeGen.genDepth03).map { case (id, t) =>
          // kinds don't matter here, only where metas and variables occur
          Type.TyApply(Type.TyApply(Type.ListType, metaType(id)), t)
        }
      )
    )

  // a small pool of names, so later bindings often shadow earlier ones
  private val genName: Gen[Infer.Name] =
    Gen.oneOf("a", "b", "c", "d").map(n => (None, Identifier.Name(n)))

  private val genBinding: Gen[(Infer.Name, Type)] = Gen.zip(genName, genType)

  private val genBatches: Gen[List[NonEmptyList[(Infer.Name, Type)]]] =
    Gen.listOf(
      Gen.zip(genBinding, Gen.listOf(genBinding)).map { case (h, t) =>
        NonEmptyList(h, t)
      }
    )

  property("addVars keeps openVars equal to the open entries of vars") {
    forAll(Gen.mapOf(genBinding), genBatches) { (init, batches) =>
      val env0 =
        new Infer.Env(
          RefSpace.constRef(0L),
          RefSpace.constRef(-1L),
          init,
          Map.empty,
          Map.empty,
          Map.empty
        )
      val env = batches.foldLeft(env0)(_.addVars(_))

      assertEquals(
        env.openVars,
        env.vars.filter { case (_, t) => Infer.Env.isOpen(t) }
      )
      // so leaving out the closed types loses no metas or free variables
      assertEquals(
        Type.metaTvs(env.openVars.values.toList),
        Type.metaTvs(env.vars.values.toList)
      )
      assertEquals(
        Type.freeTyVars(env.openVars.values.toList).toSet,
        Type.freeTyVars(env.vars.values.toList).toSet
      )
    }
  }

  test("normalizeAliasesDeep expands an alias to a forall under a forall") {
    val a = Type.Var.Bound("a")
    val outer = Type.Var.Bound("outer")
    val poly =
      TypeAlias(
        PackageName.parts("InferEnvTest"),
        TypeName(Identifier.Constructor("Poly")),
        Nil,
        Type.ForAll(
          NonEmptyList.one((a, Kind.Type)),
          Type.Fun(Type.TyVar(a), Type.TyVar(a))
        )
      )
    val env =
      new Infer.Env(
        RefSpace.constRef(0L),
        RefSpace.constRef(-1L),
        Map.empty,
        Map.empty,
        Map.empty,
        Map(poly.toTypeConst -> poly)
      )

    val outerVars = NonEmptyList.one((outer, Kind.Type))
    // the alias is a Rho, but its expansion is not
    val tpe = Type.ForAll(outerVars, poly.toTypeTyConst)
    val expected = Type.forAll(outerVars, poly.rhs)

    assertEquals(env.normalizeAliasesDeep(tpe), expected)
  }
}
