package dev.bosatsu

import cats.data.{NonEmptyList, Validated}

import Identifier.Bindable
import ShadowedBindingTypeCheck.BindingSite
import dev.bosatsu.rankn.Type

class ShadowedBindingTypeCheckTest extends munit.FunSuite {
  private val pack = PackageName.parts("ShadowedBindingTypeCheck")

  private val x: Bindable = Identifier.Name("x")
  private val y: Bindable = Identifier.Name("y")
  private val main: Bindable = Identifier.Name("main")
  private val id: Bindable = Identifier.Name("id")

  private type TypedLet = (Bindable, RecursionKind, TypedExpr[Declaration])

  private def tagAt(offset: Int): Declaration = {
    given Region = Region(offset, offset + 1)
    Declaration.Var(Identifier.Name(s"tag_$offset"))
  }

  private def lambdaTagAt(
      args: NonEmptyList[Pattern.Parsed],
      offset: Int
  ): Declaration = {
    given Region = Region(offset, offset + 1)
    Declaration.Lambda(args, Declaration.Var(Identifier.Name(s"lambda_body_$offset")))
  }

  private def intLit(value: Int, offset: Int): TypedExpr[Declaration] =
    TypedExpr.Literal(Lit.fromInt(value), Type.IntType, tagAt(offset))

  private def strLit(value: String, offset: Int): TypedExpr[Declaration] =
    TypedExpr.Literal(Lit.Str(value), Type.StrType, tagAt(offset))

  private def local(
      name: Bindable,
      tpe: Type,
      offset: Int
  ): TypedExpr[Declaration] =
    TypedExpr.Local(name, tpe, tagAt(offset))

  private def lambda(
      arg: Bindable,
      argType: Type,
      body: TypedExpr[Declaration],
      offset: Int
  ): TypedExpr[Declaration] =
    TypedExpr.AnnotatedLambda(
      NonEmptyList.one((arg, argType)),
      body,
      tagAt(offset)
    )

  private def let(
      arg: Bindable,
      rhs: TypedExpr[Declaration],
      in: TypedExpr[Declaration],
      recursive: RecursionKind,
      offset: Int
  ): TypedExpr[Declaration] =
    TypedExpr.Let(arg, rhs, in, recursive, tagAt(offset))

  private def asType(tpe: Type, offset: Int): TypedExpr[Declaration] =
    TypedExpr.Annotation(intLit(0, offset + 5000), tpe, None)

  private val intToInt: Type = Type.Fun(Type.IntType, Type.IntType)

  private val polyIdA: Type = {
    val a = Type.Var.Bound("a")
    Type.forAll(
      NonEmptyList.one((a, Kind.Type)),
      Type.Fun(Type.TyVar(a), Type.TyVar(a))
    )
  }

  private val polyIdB: Type = {
    val b = Type.Var.Bound("b")
    Type.forAll(
      NonEmptyList.one((b, Kind.Type)),
      Type.Fun(Type.TyVar(b), Type.TyVar(b))
    )
  }

  private def check(lets: List[TypedLet]): ShadowedBindingTypeCheck.Res[Unit] =
    ShadowedBindingTypeCheck.checkLets(pack, lets)

  private def assertValid(lets: List[TypedLet]): Unit =
    check(lets) match {
      case Validated.Valid(()) => ()
      case Validated.Invalid(errs) =>
        fail(s"expected valid, got ${errs.toNonEmptyList.toList}")
    }

  private def singleError(lets: List[TypedLet]): ShadowedBindingTypeCheck.Error =
    check(lets) match {
      case Validated.Valid(()) =>
        fail("expected a shadowed binding type error")
      case Validated.Invalid(errs) =>
        errs.toNonEmptyList.toList match {
          case err :: Nil => err
          case many       => fail(s"expected one error, got ${many.length}: $many")
        }
    }

  test("top-level shadowing by locals is allowed") {
    val mainExpr =
      let(
        x,
        strLit("two", 20),
        local(x, Type.StrType, 21),
        RecursionKind.NonRecursive,
        19
      )
    assertValid(
      List(
        (x, RecursionKind.NonRecursive, intLit(1, 10)),
        (main, RecursionKind.NonRecursive, mainExpr)
      )
    )
  }

  test("let shadow with same type passes") {
    val expr =
      let(
        x,
        intLit(1, 20),
        let(
          x,
          intLit(2, 22),
          local(x, Type.IntType, 23),
          RecursionKind.NonRecursive,
          21
        ),
        RecursionKind.NonRecursive,
        19
      )
    assertValid(List((main, RecursionKind.NonRecursive, expr)))
  }

  test("let shadow with different type fails") {
    val expr =
      let(
        x,
        intLit(1, 20),
        let(
          x,
          strLit("two", 22),
          local(x, Type.StrType, 23),
          RecursionKind.NonRecursive,
          21
        ),
        RecursionKind.NonRecursive,
        19
      )
    val err = singleError(List((main, RecursionKind.NonRecursive, expr)))
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, Type.IntType)
    assertEquals(err.current.tpe, Type.StrType)
    assertEquals(err.current.site, BindingSite.LetBinding)
  }

  test("lambda arg shadow same type passes") {
    val expr =
      let(
        x,
        intLit(1, 20),
        lambda(x, Type.IntType, local(x, Type.IntType, 30), 29),
        RecursionKind.NonRecursive,
        19
      )
    assertValid(List((main, RecursionKind.NonRecursive, expr)))
  }

  test("let shadowing lambda argument with a different type fails") {
    val lamBody =
      let(
        x,
        strLit("two", 30),
        local(x, Type.StrType, 31),
        RecursionKind.NonRecursive,
        29
      )
    val expr = lambda(x, Type.IntType, lamBody, 28)
    val err = singleError(List((main, RecursionKind.NonRecursive, expr)))
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, Type.IntType)
    assertEquals(err.current.tpe, Type.StrType)
    assertEquals(err.current.site, BindingSite.LetBinding)
  }

  test("loop binder shadow different type fails") {
    val loopExpr =
      let(
        x,
        strLit("outer", 40),
        TypedExpr.Loop(
          NonEmptyList.one((x, intLit(1, 41))),
          local(x, Type.IntType, 42),
          tagAt(39)
        ),
        RecursionKind.NonRecursive,
        38
      )
    val err = singleError(List((main, RecursionKind.NonRecursive, loopExpr)))
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, Type.StrType)
    assertEquals(err.current.tpe, Type.IntType)
    assertEquals(err.current.site, BindingSite.LoopBinding)
  }

  test("match pattern binder shadow different type fails") {
    val branch = TypedExpr.Branch(
      Pattern.Annotation(Pattern.Var(x), Type.StrType),
      None,
      local(x, Type.StrType, 51)
    )
    val expr =
      let(
        x,
        intLit(1, 49),
        TypedExpr.Match(intLit(1, 50), NonEmptyList.one(branch), tagAt(48)),
        RecursionKind.NonRecursive,
        47
      )
    val err = singleError(List((main, RecursionKind.NonRecursive, expr)))
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, Type.IntType)
    assertEquals(err.current.tpe, Type.StrType)
    assertEquals(err.current.site, BindingSite.PatternBinding)
  }

  test("match pattern binder may shadow lambda arguments") {
    val branch = TypedExpr.Branch(
      Pattern.Annotation(Pattern.Var(x), Type.StrType),
      None,
      local(x, Type.StrType, 56)
    )
    val expr =
      lambda(
        x,
        Type.IntType,
        TypedExpr.Match(local(x, Type.IntType, 55), NonEmptyList.one(branch), tagAt(54)),
        53
      )

    assertValid(List((main, RecursionKind.NonRecursive, expr)))
  }

  test("recursive let binder is in scope for rhs") {
    val innerRhs =
      let(
        x,
        intLit(1, 62),
        local(x, Type.IntType, 63),
        RecursionKind.NonRecursive,
        61
      )
    val recursiveLet =
      let(
        x,
        TypedExpr.Annotation(innerRhs, intToInt, None),
        local(y, intToInt, 64),
        RecursionKind.Recursive,
        60
      )
    val err =
      singleError(
        List(
          (main, RecursionKind.NonRecursive, recursiveLet)
        )
      )
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, intToInt)
    assertEquals(err.current.tpe, Type.IntType)
    assertEquals(err.current.site, BindingSite.LetBinding)
  }

  test("quantified alpha-equivalent shadows pass") {
    val expr =
      let(
        id,
        asType(polyIdA, 70),
        let(
          id,
          asType(polyIdB, 72),
          local(id, polyIdB, 73),
          RecursionKind.NonRecursive,
          71
        ),
        RecursionKind.NonRecursive,
        69
      )

    assertValid(List((main, RecursionKind.NonRecursive, expr)))
  }

  test("quantified non-equivalent shadows fail") {
    val expr =
      let(
        id,
        asType(polyIdA, 80),
        let(
          id,
          asType(intToInt, 82),
          local(id, intToInt, 83),
          RecursionKind.NonRecursive,
          81
        ),
        RecursionKind.NonRecursive,
        79
      )

    val err = singleError(List((main, RecursionKind.NonRecursive, expr)))
    assertEquals(err.name, id)
    assertEquals(err.previous.tpe, polyIdA)
    assertEquals(err.current.tpe, intToInt)
    assertEquals(err.current.site, BindingSite.LetBinding)
  }

  test("wildcard source lambda args are ignored for shadow checks") {
    val wildcardSource = lambdaTagAt(NonEmptyList.one(Pattern.WildCard), 90)
    val wildcardLambda =
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((Identifier.Name("arg_from_desugar"), Type.StrType)),
        strLit("ok", 91),
        wildcardSource
      )

    val expr =
      let(
        Identifier.Name("arg_from_desugar"),
        intLit(1, 92),
        wildcardLambda,
        RecursionKind.NonRecursive,
        89
      )

    assertValid(List((main, RecursionKind.NonRecursive, expr)))
  }

  test("synthetic binders are ignored") {
    val syn = Identifier.synthetic("tmp")
    val expr =
      let(
        syn,
        intLit(1, 100),
        let(
          syn,
          strLit("two", 102),
          local(syn, Type.StrType, 103),
          RecursionKind.NonRecursive,
          101
        ),
        RecursionKind.NonRecursive,
        99
      )

    assertValid(List((main, RecursionKind.NonRecursive, expr)))
  }
}
