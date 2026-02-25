package dev.bosatsu

import cats.Order
import dev.bosatsu.hashing.Algo
import dev.bosatsu.rankn.DataRepr
import dev.bosatsu.rankn.Type

class MatchlessSourceInfoTest extends munit.FunSuite {
  given Order[Unit] = Order.fromOrdering

  private val anyVariant: (PackageName, Identifier.Constructor) => Option[
    DataRepr
  ] =
    {
      case (PackageName.PredefName, Identifier.Constructor("EmptyList")) =>
        Some(DataRepr.Enum(0, 0, List(0, 1)))
      case (PackageName.PredefName, Identifier.Constructor("NonEmptyList")) =>
        Some(DataRepr.Enum(1, 2, List(0, 1)))
      case _ =>
        Some(DataRepr.Struct(0))
    }

  test("Matchless.fromLet attaches source info from region + package hash") {
    val hashIdent = Algo.hashBytes[Algo.Blake3]("abc".getBytes("UTF-8")).toIdent
    val expr = TypedExpr.Literal(Lit.fromInt(1), Type.IntType, Region(7, 9))
    val lowered = Matchless.fromLet(
      from = (),
      name = Identifier.Name("x"),
      rec = RecursionKind.NonRecursive,
      te = expr,
      sourceHashIdent = hashIdent
    )(anyVariant)

    assertEquals(lowered.sourceInfo.packageHashIdent, hashIdent)
    assertEquals(lowered.sourceInfo.region, Region(7, 9))
  }

  test("Matchless.fromLet uses sentinel source info when missing") {
    val expr = TypedExpr.Literal(Lit.fromInt(2), Type.IntType, ())
    val lowered = Matchless.fromLet(
      from = (),
      name = Identifier.Name("x"),
      rec = RecursionKind.NonRecursive,
      te = expr
    )(anyVariant)

    assertEquals(lowered.sourceInfo, Matchless.SourceInfo.empty)
  }
}
