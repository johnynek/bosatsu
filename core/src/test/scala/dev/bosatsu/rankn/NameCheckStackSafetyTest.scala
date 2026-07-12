package dev.bosatsu.rankn

import dev.bosatsu.{
  Expr,
  HasRegion,
  Identifier,
  Lit,
  PackageName,
  Platform,
  RecursionKind,
  Region
}

class NameCheckStackSafetyTest extends munit.FunSuite {
  private val pack = PackageName.parts("NameCheck", "Stack")

  private given HasRegion[Unit] = HasRegion.instance(_ => Region.empty)

  private def deepListExpr(depth: Int): Expr[Unit] = {
    val empty =
      Expr.Global(
        PackageName.PredefName,
        Identifier.Constructor("EmptyList"),
        ()
      )
    val cons =
      Expr.Global(
        PackageName.PredefName,
        Identifier.Constructor("NonEmptyList"),
        ()
      )

    var acc: Expr[Unit] = empty
    var idx = depth - 1

    while (idx >= 0) {
      val head = Expr.Literal(Lit.fromInt(idx), ())
      acc = Expr.buildApp(cons, head :: acc :: Nil, ())
      idx = idx - 1
    }

    acc
  }

  private def runNameCheckOnSmallStack(
      depth: Int,
      stackBytes: Long
  ): Option[Throwable] = {
    val let = (
      Identifier.Name("fixture"): Identifier.Bindable,
      RecursionKind.NonRecursive,
      deepListExpr(depth)
    )
    val initialScope: Map[Infer.Name, Type] = Map(
      (Some(PackageName.PredefName), Identifier.Constructor("EmptyList")) ->
        Type.UnitType,
      (Some(PackageName.PredefName), Identifier.Constructor("NonEmptyList")) ->
        Type.UnitType
    )

    @volatile var failure: Option[Throwable] = None

    val thread = new Thread(
      null,
      new Runnable {
        def run(): Unit =
          try {
            val (errors, result) =
              NameCheck.checkLets(pack, let :: Nil, initialScope)
            assertEquals(errors, None)
            assertEquals(result.nameErrorLets, Set.empty)
          } catch {
            case t: Throwable =>
              failure = Some(t)
          }
      },
      "name-check-small-stack",
      stackBytes
    )

    thread.start()
    thread.join()
    failure
  }

  private def warmNameCheck(): Unit = {
    val let = (
      Identifier.Name("fixture"): Identifier.Bindable,
      RecursionKind.NonRecursive,
      deepListExpr(4)
    )
    val initialScope: Map[Infer.Name, Type] = Map(
      (Some(PackageName.PredefName), Identifier.Constructor("EmptyList")) ->
        Type.UnitType,
      (Some(PackageName.PredefName), Identifier.Constructor("NonEmptyList")) ->
        Type.UnitType
    )

    val (errors, result) =
      NameCheck.checkLets(pack, let :: Nil, initialScope)
    assertEquals(errors, None)
    assertEquals(result.nameErrorLets, Set.empty)
  }

  Platform.onJvm(
    test("NameCheck.checkLets should not overflow on deep list expressions") {
      val depth = sys.props.get("repro.nameCheckDepth").fold(5000)(_.toInt)
      val stackBytes =
        sys.props.get("repro.stackBytes").fold(96L * 1024L)(_.toLong)
      warmNameCheck()

      runNameCheckOnSmallStack(depth, stackBytes) match {
        case Some(_: StackOverflowError) =>
          fail(
            s"NameCheck.checkLets overflowed on a deep list expression (depth=$depth, stackBytes=$stackBytes)"
          )
        case Some(other) =>
          val trace = other.getStackTrace.iterator.take(40).mkString("\n")
          fail(s"unexpected failure: $other\n$trace")
        case None =>
          ()
      }
    }
  )
}
