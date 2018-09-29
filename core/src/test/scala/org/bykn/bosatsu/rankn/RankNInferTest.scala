package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.scalatest.FunSuite
import org.bykn.bosatsu.{PackageName, Pattern}

class RankNInferTest extends FunSuite {

  def defType(n: String): Type.Const.Defined =
    Type.Const.Defined(PackageName.parts("Test"), n)

  def testType(term: Term, ty: Type) =
    Infer.typeCheck(term).runFully(Map.empty, Map.empty) match {
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

    // test branches
    testType(If(Lit(true), Lit(0), Lit(1)), Type.intType)
    testType(Let("x", Lit(0), If(Lit(true), Var("x"), Lit(1))), Type.intType)

    val identFnType =
      ForAll(NonEmptyList.of(Bound("a")),
        Type.Fun(Type.TyVar(Bound("a")), Type.TyVar(Bound("a"))))
    testType(Let("x", Lam("y", Var("y")),
      If(Lit(true), Var("x"),
        Ann(Lam("x", Var("x")), identFnType))), identFnType)
  }

  test("Match inference") {
    import Term._

    testType(
      Match(Lit(10),
        NonEmptyList.of(
          (Pattern.WildCard, Lit(0))
          )), Type.intType)

    testType(
      Match(Lit(true),
        NonEmptyList.of(
          (Pattern.WildCard, Lit(0))
          )), Type.intType)
  }

  test("Match with custom non-generic types") {
    def b(a: String): Type.Var = Type.Var.Bound(a)
    def v(a: String): Type = Type.TyVar(b(a))

    val optName = defType("Option")
    val optType: Type.Tau = Type.TyConst(optName)

    val definedOption = Map(
      ("Some", (Nil, List(Type.intType), optName)),
      ("None", (Nil, Nil, optName)))

    val constructors = Map(
      ("Some", Type.Fun(Type.intType, optType))
    )

    def testWithOpt(term: Term, ty: Type) =
      Infer.typeCheck(term).runFully(constructors, definedOption) match {
        case Left(err) => assert(false, err)
        case Right(tpe) => assert(tpe == ty, term.toString)
      }

    def failWithOpt(term: Term) =
      Infer.typeCheck(term).runFully(constructors, definedOption) match {
        case Left(err) => assert(true)
        case Right(tpe) => assert(false, s"expected to fail, but inferred type $tpe")
      }

    import Term._

    testWithOpt(
      Match(App(Var("Some"), Lit(1)),
        NonEmptyList.of(
          (Pattern.WildCard, Lit(0))
          )), Type.intType)

    testWithOpt(
      Match(App(Var("Some"), Lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Some", List(Pattern.Var("a"))), Var("a")),
          (Pattern.PositionalStruct("None", Nil), Lit(42))
          )), Type.intType)

    failWithOpt(
      Match(App(Var("Some"), Lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Foo", List(Pattern.WildCard)), Lit(0))
          )))
  }

  test("Match with custom generic types") {
    def b(a: String): Type.Var = Type.Var.Bound(a)
    def v(a: String): Type = Type.TyVar(b(a))

    val optName = defType("Option")
    val optType: Type.Tau = Type.TyConst(optName)

    val definedOption = Map(
      ("Some", (List(b("a")), List(v("a")), optName)),
      ("None", (List(b("a")), Nil, optName)))

    val constructors = Map(
      ("Some", Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(v("a"), Type.TyApply(optType, v("a"))))),
      ("None", Type.ForAll(NonEmptyList.of(b("a")), Type.TyApply(optType, v("a"))))
    )

    def testWithOpt(term: Term, ty: Type) =
      Infer.typeCheck(term).runFully(constructors, definedOption) match {
        case Left(err) => assert(false, err)
        case Right(tpe) => assert(tpe == ty, term.toString)
      }

    def failWithOpt(term: Term) =
      Infer.typeCheck(term).runFully(constructors, definedOption) match {
        case Left(err) => assert(true)
        case Right(tpe) => assert(false, s"expected to fail, but inferred type $tpe")
      }

    import Term._

    testWithOpt(
      Match(App(Var("Some"), Lit(1)),
        NonEmptyList.of(
          (Pattern.WildCard, Lit(0))
          )), Type.intType)

    testWithOpt(
      Match(App(Var("Some"), Lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Some", List(Pattern.Var("a"))), Var("a")),
          (Pattern.PositionalStruct("None", Nil), Lit(42))
          )), Type.intType)

    // Nested Some
    testWithOpt(
      Match(App(Var("Some"), App(Var("Some"), Lit(1))),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Some", List(Pattern.Var("a"))), Var("a"))
          )), Type.TyApply(optType, Type.intType))

    failWithOpt(
      Match(App(Var("Some"), Lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Foo", List(Pattern.WildCard)), Lit(0))
          )))
  }

  test("Test a constructor with ForAll") {
    def b(a: String): Type.Var = Type.Var.Bound(a)
    def v(a: String): Type = Type.TyVar(b(a))

    val pureName = defType("Pure")
    val pureType: Type.Tau = Type.TyConst(pureName)
    val optName = defType("Option")
    val optType: Type.Tau = Type.TyConst(optName)

    /**
     * struct Pure(pure: forall a. a -> f[a])
     */
    val defined = Map(
      ("Pure", (List(b("f")),
        List(Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(v("a"), Type.TyApply(v("f"), v("a"))))),
        pureName)),
      ("Some", (List(b("a")), List(v("a")), optName)),
      ("None", (List(b("a")), Nil, optName)))

    val constructors = Map(
      ("Pure", Type.ForAll(NonEmptyList.of(b("f")),
        Type.Fun(Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(v("a"), Type.TyApply(v("f"), v("a")))),
          Type.TyApply(Type.TyConst(pureName), v("f")) ))),
      ("Some", Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(v("a"), Type.TyApply(optType, v("a"))))),
      ("None", Type.ForAll(NonEmptyList.of(b("a")), Type.TyApply(optType, v("a"))))
    )

    def testWithTypes(term: Term, ty: Type) =
      Infer.typeCheck(term).runFully(constructors, defined) match {
        case Left(err) => assert(false, err)
        case Right(tpe) => assert(tpe == ty, term.toString)
      }

    import Term._

    testWithTypes(
      App(Var("Pure"), Var("Some")), Type.TyApply(Type.TyConst(pureName), optType))
  }

  test("test all binders") {
    assert(Type.allBinders.filter(_.name.startsWith("a")).take(100).map(_.name) ==
      ("a" #:: Stream.iterate(0)(_ + 1).map { i => s"a$i" }).take(100))
  }
}
