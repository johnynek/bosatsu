package dev.bosatsu

import cats.data.NonEmptyList
import dev.bosatsu.rankn.{Type, TypeEnv}
import Identifier.Constructor

class TestUtilsTypeValidationTest extends munit.FunSuite {
  private val tag = ()

  private def renderErrors(
      res: cats.data.ValidatedNec[TestUtils.TypeValidationError, Unit]
  ): String =
    res.fold(_.toNonEmptyList.toList.map(_.toString).mkString("\n"), _ => "")

  test("validateTypes accepts NamedChar bindings from string patterns") {
    val input = Identifier.Name("input")
    val ch = Identifier.Name("ch")

    val scrutinee = TypedExpr.Local(input, Type.StrType, tag)
    val pattern: Pattern[(PackageName, Constructor), Type] =
      Pattern.Annotation(
        Pattern.StrPat(NonEmptyList.one(Pattern.StrPart.NamedChar(ch))),
        Type.StrType
      )
    val body = TypedExpr.Local(ch, Type.CharType, tag)
    val te = TypedExpr.Match(
      scrutinee,
      NonEmptyList.one(TypedExpr.Branch(pattern, None, body)),
      tag
    )

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(scrutinee -> Type.StrType)
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    assert(res.isValid, renderErrors(res))
  }

  test("validateTypes rejects mismatched NamedChar binding type") {
    val input = Identifier.Name("input")
    val ch = Identifier.Name("ch")

    val scrutinee = TypedExpr.Local(input, Type.StrType, tag)
    val pattern: Pattern[(PackageName, Constructor), Type] =
      Pattern.Annotation(
        Pattern.StrPat(NonEmptyList.one(Pattern.StrPart.NamedChar(ch))),
        Type.StrType
      )
    val body = TypedExpr.Local(ch, Type.StrType, tag)
    val te = TypedExpr.Match(
      scrutinee,
      NonEmptyList.one(TypedExpr.Branch(pattern, None, body)),
      tag
    )

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(scrutinee -> Type.StrType)
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
    assert(err.contains("local type mismatch"), err)
  }

  test("validateTypes rejects application argument type mismatch") {
    val fnName = Identifier.Name("f")
    val fnType = Type.Fun(NonEmptyList.one(Type.IntType), Type.IntType)
    val fn = TypedExpr.Local(fnName, fnType, tag)
    val badArg = TypedExpr.Literal(Lit.Str("x"), Type.StrType, tag)
    val te = TypedExpr.App(fn, NonEmptyList.one(badArg), Type.IntType, tag)

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(fn -> fnType)
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
    assert(
      err.contains("application type mismatch") || err.contains(
        "app arg type mismatch"
      ),
      err
    )
  }

  test("validateTypes accepts application with scoped generic bound type") {
    val a = Type.Var.Bound("a")
    val fnName = Identifier.Name("f")
    val argName = Identifier.Name("x")

    val aType = Type.TyVar(a)
    val fnType = Type.Fun(NonEmptyList.one(aType), aType)
    val fn = TypedExpr.Local(fnName, fnType, tag)
    val arg = TypedExpr.Local(argName, aType, tag)
    val app = TypedExpr.App(fn, NonEmptyList.one(arg), aType, tag)
    val te = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      app
    )

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(fn -> fnType, arg -> aType)
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    assert(res.isValid, renderErrors(res))
  }

  test("validateTypes rejects free bound type vars in root type") {
    val a = Type.Var.Bound("a")
    val leaked = Identifier.Name("leaked")
    val te = TypedExpr.Local(leaked, Type.TyVar(a), tag)

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(te -> Type.TyVar(a))
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
    assert(err.contains("illegal inferred type"), err)
  }
}
