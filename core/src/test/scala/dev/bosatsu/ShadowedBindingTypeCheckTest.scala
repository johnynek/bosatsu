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

  test("let shadow with same type passes") {
    val mainExpr =
      let(
        x,
        intLit(2, 20),
        local(x, Type.IntType, 21),
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

  test("let shadow with different type fails") {
    val mainExpr =
      let(
        x,
        strLit("two", 20),
        local(x, Type.StrType, 21),
        RecursionKind.NonRecursive,
        19
      )
    val err =
      singleError(
        List(
          (x, RecursionKind.NonRecursive, intLit(1, 10)),
          (main, RecursionKind.NonRecursive, mainExpr)
        )
      )
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, Type.IntType)
    assertEquals(err.current.tpe, Type.StrType)
    assertEquals(err.current.site, BindingSite.LetBinding)
  }

  test("lambda arg shadow same type passes") {
    val mainExpr =
      lambda(x, Type.IntType, local(x, Type.IntType, 30), 29)
    assertValid(
      List(
        (x, RecursionKind.NonRecursive, intLit(1, 10)),
        (main, RecursionKind.NonRecursive, mainExpr)
      )
    )
  }

  test("lambda arg shadow different type fails") {
    val mainExpr =
      lambda(x, Type.StrType, local(x, Type.StrType, 30), 29)
    val err =
      singleError(
        List(
          (x, RecursionKind.NonRecursive, intLit(1, 10)),
          (main, RecursionKind.NonRecursive, mainExpr)
        )
      )
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, Type.IntType)
    assertEquals(err.current.tpe, Type.StrType)
    assertEquals(err.current.site, BindingSite.LambdaArg)
  }

  test("loop binder shadow different type fails") {
    val loopExpr =
      TypedExpr.Loop(
        NonEmptyList.one((x, intLit(1, 40))),
        local(x, Type.IntType, 41),
        tagAt(39)
      )
    val err =
      singleError(
        List(
          (x, RecursionKind.NonRecursive, strLit("outer", 10)),
          (main, RecursionKind.NonRecursive, loopExpr)
        )
      )
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
    val matchExpr =
      TypedExpr.Match(intLit(1, 50), NonEmptyList.one(branch), tagAt(49))
    val err =
      singleError(
        List(
          (x, RecursionKind.NonRecursive, intLit(1, 10)),
          (main, RecursionKind.NonRecursive, matchExpr)
        )
      )
    assertEquals(err.name, x)
    assertEquals(err.previous.tpe, Type.IntType)
    assertEquals(err.current.tpe, Type.StrType)
    assertEquals(err.current.site, BindingSite.PatternBinding)
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
    val mainExpr =
      let(
        id,
        asType(polyIdB, 70),
        local(id, polyIdB, 71),
        RecursionKind.NonRecursive,
        69
      )
    assertValid(
      List(
        (id, RecursionKind.NonRecursive, asType(polyIdA, 10)),
        (main, RecursionKind.NonRecursive, mainExpr)
      )
    )
  }

  test("quantified non-equivalent shadows fail") {
    val mainExpr =
      let(
        id,
        asType(intToInt, 80),
        local(id, intToInt, 81),
        RecursionKind.NonRecursive,
        79
      )
    val err =
      singleError(
        List(
          (id, RecursionKind.NonRecursive, asType(polyIdA, 10)),
          (main, RecursionKind.NonRecursive, mainExpr)
        )
      )
    assertEquals(err.name, id)
    assertEquals(err.previous.tpe, polyIdA)
    assertEquals(err.current.tpe, intToInt)
    assertEquals(err.current.site, BindingSite.LetBinding)
  }
}
