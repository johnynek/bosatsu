package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.scalatest.FunSuite

class RankNTcTest extends FunSuite {

  def testType(term: Term, ty: Type) =
    Tc.typeCheck(term).runFully(Map.empty) match {
      case Left(err) => assert(false, err)
      case Right(tpe) => assert(tpe == ty, term.toString)
    }

  test("Basic inferences") {
    import Term._
    import Type.Var.Bound
    import Type.ForAll

    testType(Lit(100L), Type.intType)
    testType(Let("x", Lam("y", Var("y")), Lit(100L)), Type.intType)
    testType(Lam("y", Var("y")),
      ForAll(NonEmptyList.of(Bound("a")),
        Type.Fun(Type.TyVar(Bound("a")),Type.TyVar(Bound("a")))))
    testType(Lam("y", Lam("z", Var("y"))),
      ForAll(NonEmptyList.of(Bound("a"), Bound("b")),
        Type.Fun(Type.TyVar(Bound("a")),
          Type.Fun(Type.TyVar(Bound("b")),Type.TyVar(Bound("a"))))))

    testType(App(Lam("x", Var("x")), Lit(100L)), Type.intType)
    testType(Ann(App(Lam("x", Var("x")), Lit(100L)), Type.intType), Type.intType)
    testType(App(ALam("x", Type.intType, Var("x")), Lit(100L)), Type.intType)
  }
}
