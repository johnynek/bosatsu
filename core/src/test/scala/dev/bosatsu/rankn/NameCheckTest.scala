package dev.bosatsu.rankn

import cats.data.NonEmptyList
import dev.bosatsu._

class NameCheckTest extends munit.FunSuite {
  private val pack = PackageName.parts("NameCheck")

  implicit val hasUnitRegion: HasRegion[Unit] =
    HasRegion.instance(_ => Region(0, 1))

  private def bn(str: String): Identifier.Bindable =
    Identifier.Name(str)

  private def let(
      name: String,
      expr: Expr[Unit],
      rec: RecursionKind = RecursionKind.NonRecursive
  ): NameCheck.Let[Unit] =
    (bn(name), rec, expr)

  private def local(name: String): Expr[Unit] =
    Expr.Local(bn(name), ())

  private def global(name: String): Expr[Unit] =
    Expr.Global(pack, bn(name), ())

  private def namedMatch(
      arg: Expr[Unit],
      bindName: String
  ): Expr[Unit] =
    Expr.Match(
      arg,
      NonEmptyList.one(
        Expr.Branch(
          Pattern.Var(bn(bindName)),
          Some(local(bindName)),
          local(bindName)
        )(using Region(0, 1))
      ),
      ()
    )

  test("lexical scopes include lambda args, local lets, and match bindings") {
    val okExpr =
      Expr.Lambda(
        NonEmptyList.one((bn("x"), None)),
        Expr.Let(
          bn("y"),
          local("x"),
          namedMatch(local("y"), "z"),
          RecursionKind.NonRecursive,
          ()
        ),
        ()
      )

    val lets = List(
      let("ok", okExpr),
      let("bad", local("missing")),
      let("after", Expr.Literal(Lit(1L), ()))
    )

    val (errors, result) = NameCheck.checkLets(pack, lets, Map.empty)
    assertEquals(result.nameErrorLets, Set(bn("bad")))
    assertEquals(errors.map(_.toNonEmptyList.length), Some(1))
    assertEquals(result.typecheckLets.map(_._1), List(bn("ok"), bn("after")))
  }

  test("recursive local let sees its own binder in the bound expression") {
    val recursiveLocal =
      Expr.Let(
        bn("self"),
        local("self"),
        local("self"),
        RecursionKind.Recursive,
        ()
      )

    val nonRecursiveLocal =
      Expr.Let(
        bn("self"),
        local("self"),
        local("self"),
        RecursionKind.NonRecursive,
        ()
      )

    val lets = List(
      let("ok", recursiveLocal),
      let("bad", nonRecursiveLocal)
    )

    val (errors, result) = NameCheck.checkLets(pack, lets, Map.empty)
    assertEquals(result.nameErrorLets, Set(bn("bad")))
    assertEquals(errors.map(_.toNonEmptyList.length), Some(1))
  }

  test(
    "dependency blocking is transitive and surviving lets keep source order"
  ) {
    val lets = List(
      let("keep1", Expr.Literal(Lit(1L), ())),
      let("root", local("missing")),
      let("dep1", global("root")),
      let("dep2", global("dep1")),
      let("keep2", Expr.Literal(Lit(2L), ())),
      let("keep3", global("keep2"))
    )

    val (errors, result) = NameCheck.checkLets(pack, lets, Map.empty)
    assertEquals(errors.map(_.toNonEmptyList.length), Some(1))
    assertEquals(result.nameErrorLets, Set(bn("root")))
    assertEquals(
      result.blockedLets,
      Set(bn("root"), bn("dep1"), bn("dep2"))
    )
    assertEquals(
      result.typecheckLets.map(_._1),
      List(bn("keep1"), bn("keep2"), bn("keep3"))
    )
    assertEquals(
      result.samePackageDeps.getOrElse(bn("dep1"), Set.empty),
      Set(bn("root"))
    )
    assertEquals(
      result.samePackageDeps.getOrElse(bn("dep2"), Set.empty),
      Set(bn("dep1"))
    )
  }
}
