package dev.bosatsu

import cats.data.NonEmptyList
import dev.bosatsu.rankn.Type
import Identifier.Bindable

class Issue2087Test extends munit.FunSuite with ParTest {
  private val lazyPack =
    Predef.loadFileInCompile("test_workspace/Bosatsu/Lazy.bosatsu")

  private val reproPackage = PackageName.parts("Repro", "Issue2087")
  private val opaqueName = Identifier.Name("opaque_Int")
  private val thunkLetName = Identifier.Name("thunk_let")
  private val thunkShareName = Identifier.Name("thunk_share")

  private val reproPack = """
package Repro/Issue2087

from Bosatsu/Lazy import (
  Lazy,
  lazy,
)

external def opaque_Int(x: Int) -> Int

thunk_let: Lazy[Int] = lazy(() -> (
  y = opaque_Int(1)
  add(y, y)
))

thunk_share: Lazy[Int] = lazy(() -> add(opaque_Int(1), opaque_Int(1)))

main = [thunk_let, thunk_share]
"""

  @annotation.tailrec
  private def stripTypeWrappers[A](te: TypedExpr[A]): TypedExpr[A] =
    te match {
      case TypedExpr.Generic(_, in)       => stripTypeWrappers(in)
      case TypedExpr.Annotation(in, _, _) => stripTypeWrappers(in)
      case _                              => te
    }

  private def isSuspensionLambdaArgs(
      args: NonEmptyList[(Bindable, Type)]
  ): Boolean =
    args match {
      case NonEmptyList((_, argTpe), Nil) =>
        Type.normalize(argTpe).sameAs(Type.UnitType)
      case _ =>
        false
    }

  private def isOpaqueCall[A](te: TypedExpr[A]): Boolean =
    stripTypeWrappers(te) match {
      case TypedExpr.App(fn, _, _, _) =>
        stripTypeWrappers(fn) match {
          case TypedExpr.Global(_, name: Bindable, _, _) =>
            name == opaqueName
          case _ =>
            false
        }
      case _ =>
        false
    }

  private def countOpaqueCalls[A](
      te: TypedExpr[A],
      inSuspension: Boolean
  ): (Int, Int) = {
    val self =
      if (isOpaqueCall(te)) {
        if (inSuspension) (1, 0) else (0, 1)
      } else (0, 0)

    def add(left: (Int, Int), right: (Int, Int)): (Int, Int) =
      (left._1 + right._1, left._2 + right._2)

    te match {
      case TypedExpr.Generic(_, in) =>
        add(self, countOpaqueCalls(in, inSuspension))
      case TypedExpr.Annotation(in, _, _) =>
        add(self, countOpaqueCalls(in, inSuspension))
      case TypedExpr.AnnotatedLambda(args, in, _) =>
        val inSuspension1 = inSuspension || isSuspensionLambdaArgs(args)
        add(self, countOpaqueCalls(in, inSuspension1))
      case TypedExpr.App(fn, args, _, _) =>
        args.toList
          .foldLeft(add(self, countOpaqueCalls(fn, inSuspension))) {
            case (acc, arg) =>
              add(acc, countOpaqueCalls(arg, inSuspension))
          }
      case TypedExpr.Let(_, ex, in, _, _) =>
        add(
          add(self, countOpaqueCalls(ex, inSuspension)),
          countOpaqueCalls(in, inSuspension)
        )
      case TypedExpr.Loop(args, in, _) =>
        val initCounts = args.toList.foldLeft(self) { case (acc, (_, init)) =>
          add(acc, countOpaqueCalls(init, inSuspension))
        }
        add(initCounts, countOpaqueCalls(in, inSuspension))
      case TypedExpr.Recur(args, _, _) =>
        args.toList.foldLeft(self) { case (acc, arg) =>
          add(acc, countOpaqueCalls(arg, inSuspension))
        }
      case TypedExpr.Match(arg, branches, _) =>
        branches.toList
          .foldLeft(add(self, countOpaqueCalls(arg, inSuspension))) {
            case (acc, TypedExpr.Branch(_, guard, branchExpr)) =>
              val guardCount =
                guard.fold((0, 0))(countOpaqueCalls(_, inSuspension))
              add(
                add(acc, guardCount),
                countOpaqueCalls(branchExpr, inSuspension)
              )
          }
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        self
    }
  }

  private def assertOpaqueCallsRemainSuspended[A](expr: TypedExpr[A]): Unit = {
    val (insideSuspension, outsideSuspension) =
      countOpaqueCalls(expr, inSuspension = false)
    assert(insideSuspension > 0, expr.reprString)
    assertEquals(outsideSuspension, 0, expr.reprString)
  }

  test("issue 2087: lazy thunks keep delayed opaque work inside Unit lambdas") {
    var out: Option[Package.Compiled] = None
    TestUtils.testInferred(
      List(lazyPack, reproPack),
      reproPackage.asString,
      (pm, _) => out = pm.toMap.get(reproPackage)
    )

    val pack = out.getOrElse(
      fail(s"missing inferred package: ${reproPackage.asString}")
    )
    def getLet(name: Bindable): TypedExpr[Region] =
      pack.lets.find(_._1 == name) match {
        case Some((_, _, te)) => te
        case None             =>
          fail(s"missing let ${name.sourceCodeRepr} in ${pack.lets.map(_._1)}")
      }

    assertOpaqueCallsRemainSuspended(getLet(thunkLetName))
    assertOpaqueCallsRemainSuspended(getLet(thunkShareName))
  }
}
