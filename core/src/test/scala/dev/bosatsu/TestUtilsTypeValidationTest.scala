package dev.bosatsu

import cats.data.NonEmptyList
import dev.bosatsu.rankn.{Type, TypeAlias, TypeEnv}
import Identifier.Constructor
import scala.collection.immutable.SortedMap

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
      NonEmptyList.one(TypedExpr.Branch(pattern, None, body)(using Region.empty)),
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
      NonEmptyList.one(TypedExpr.Branch(pattern, None, body)(using Region.empty)),
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

  // `ap: (Int -> Int -> forall b. b -> b) -> String` applied to `k: argType`
  private def validateNestedForallResultArg(argType: Type) = {
    val b = Type.Var.Bound("b")
    val bType = Type.TyVar(b)
    val paramType = Type.Fun(
      NonEmptyList.one(Type.IntType),
      Type.Fun(
        NonEmptyList.one(Type.IntType),
        Type.forAll(
          List(b -> Kind.Type),
          Type.Fun(NonEmptyList.one(bType), bType)
        )
      )
    )
    val fnType = Type.Fun(NonEmptyList.one(paramType), Type.StrType)
    val fn = TypedExpr.Local(Identifier.Name("ap"), fnType, tag)
    val arg = TypedExpr.Local(Identifier.Name("k"), argType, tag)
    val te = TypedExpr.App(fn, NonEmptyList.one(arg), Type.StrType, tag)

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(fn -> fnType, arg -> argType)
    TestUtils.validateTypes(te, names, TypeEnv.empty)
  }

  test("validateTypes accepts a weak-prenex arg for a nested forall result") {
    val a = Type.Var.Bound("a")
    val aType = Type.TyVar(a)
    val argType = Type.forAll(
      List(a -> Kind.Type),
      Type.Fun(
        NonEmptyList.one(Type.IntType),
        Type.Fun(
          NonEmptyList.one(Type.IntType),
          Type.Fun(NonEmptyList.one(aType), aType)
        )
      )
    )
    val res = validateNestedForallResultArg(argType)
    assert(res.isValid, renderErrors(res))
  }

  test("validateTypes rejects a less polymorphic arg for a nested forall result") {
    val a = Type.Var.Bound("a")
    val aType = Type.TyVar(a)
    val argType = Type.forAll(
      List(a -> Kind.Type),
      Type.Fun(
        NonEmptyList.one(Type.IntType),
        Type.Fun(
          NonEmptyList.one(Type.IntType),
          Type.Fun(NonEmptyList.one(aType), Type.IntType)
        )
      )
    )
    val res = validateNestedForallResultArg(argType)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
    assert(err.contains("app arg type mismatch"), err)
  }

  // `ap: paramType -> String` applied to `k: argType`
  private def validateArg(paramType: Type, argType: Type) = {
    val fnType = Type.Fun(NonEmptyList.one(paramType), Type.StrType)
    val fn = TypedExpr.Local(Identifier.Name("ap"), fnType, tag)
    val arg = TypedExpr.Local(Identifier.Name("k"), argType, tag)
    val te = TypedExpr.App(fn, NonEmptyList.one(arg), Type.StrType, tag)

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(fn -> fnType, arg -> argType)
    TestUtils.validateTypes(te, names, TypeEnv.empty)
  }

  private def intToIntTo(res: Type): Type =
    Type.Fun(
      NonEmptyList.one(Type.IntType),
      Type.Fun(NonEmptyList.one(Type.IntType), res)
    )

  test("validateTypes accepts a weak-prenex arg for a nested exists result") {
    val a = Type.Var.Bound("a")
    val b = Type.Var.Bound("b")
    // Int -> Int -> exists b. List[b]
    val paramType = intToIntTo(
      Type.exists(
        List(b -> Kind.Type),
        Type.apply1(Type.ListType, Type.TyVar(b))
      )
    )
    // exists a. Int -> Int -> List[a]
    val argType = Type.exists(
      List(a -> Kind.Type),
      intToIntTo(Type.apply1(Type.ListType, Type.TyVar(a)))
    )
    val res = validateArg(paramType, argType)
    assert(res.isValid, renderErrors(res))
  }

  test("validateTypes does not choose a nested existential in an arg") {
    val a = Type.Var.Bound("a")
    // Int -> Int -> List[Int]
    val paramType = intToIntTo(Type.apply1(Type.ListType, Type.IntType))
    // Int -> Int -> exists a. List[a]
    val argType = intToIntTo(
      Type.exists(
        List(a -> Kind.Type),
        Type.apply1(Type.ListType, Type.TyVar(a))
      )
    )
    val res = validateArg(paramType, argType)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
    assert(err.contains("app arg type mismatch"), err)
  }

  private given Ordering[Type.Var.Bound] =
    Type.Var.Bound.orderBound.toOrdering

  // `xs: List[Int]` annotated as `exists b. List[b]`, hiding `witness` for b
  private def validateIntListPack(witness: Type) = {
    val b = Type.Var.Bound("b")
    val listInt = Type.apply1(Type.ListType, Type.IntType)
    val coerce = Type.exists(
      List(b -> Kind.Type),
      Type.apply1(Type.ListType, Type.TyVar(b))
    )
    val term = TypedExpr.Local(Identifier.Name("xs"), listInt, tag)
    val evidence = TypedExpr.QuantifierEvidence(
      sourceAtSolve = listInt,
      targetAtSolve = coerce,
      forallSolved = SortedMap.empty,
      existsHidden = SortedMap(b -> (Kind.Type, witness))
    )
    val te = TypedExpr.Annotation(term, coerce, Some(evidence))
    val names: Map[TypedExpr.Name[Unit], Type] = Map(term -> listInt)
    TestUtils.validateTypes(te, names, TypeEnv.empty)
  }

  test("validateTypes accepts an annotation hiding the term's witness") {
    val res = validateIntListPack(Type.IntType)
    assert(res.isValid, renderErrors(res))
  }

  test("validateTypes rejects an annotation hiding another witness") {
    val res = validateIntListPack(Type.StrType)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
    assert(err.contains("annotation hides witnesses"), err)
  }

  test("validateTypes accepts alias-expanded nested foralls without crashing") {
    val aliasPack = PackageName.parts("TypeValidatorTest")
    val aliasName = TypeName(Constructor("Poly"))
    val polyVar = Type.Var.Bound("poly_a")
    val outerVar = Type.Var.Bound("outer")
    val actualVar = Type.Var.Bound("actual")
    val localName = Identifier.Name("poly")

    val polyAlias =
      TypeAlias(
        aliasPack,
        aliasName,
        Nil,
        Type.ForAll(
          NonEmptyList.one((polyVar, Kind.Type)),
          Type.Fun(
            NonEmptyList.one(Type.TyVar(polyVar)),
            Type.TyVar(polyVar)
          )
        )
      )

    val expected = Type.ForAll(
      NonEmptyList.one((outerVar, Kind.Type)),
      Type.TyConst(polyAlias.toTypeConst)
    )
    val actual = Type.ForAll(
      NonEmptyList.of((outerVar, Kind.Type), (actualVar, Kind.Type)),
      Type.Fun(
        NonEmptyList.one(Type.TyVar(actualVar)),
        Type.TyVar(actualVar)
      )
    )
    val te = TypedExpr.Local(localName, actual, tag)
    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(te -> expected)
    val env = TypeEnv.empty.addTypeAlias(polyAlias)

    val res = TestUtils.validateTypes(te, names, env)
    assert(res.isValid, renderErrors(res))
  }

  test("validateTypes accepts higher-kinded application solved from result type") {
    val wrap = Type.Var.Bound("wrap")
    val f = Type.Var.Bound("f")
    val g = Type.Var.Bound("g")
    val a = Type.Var.Bound("a")
    val fnName = Identifier.Name("mk")
    val argName = Identifier.Name("pure_g")

    val starToStar = Kind(Kind.Type.in)
    val wrapperKind = Kind(starToStar.in)

    def pureType(on: Type.Var.Bound): Type =
      Type.ForAll(
        NonEmptyList.one((a, Kind.Type)),
        Type.Fun(
          NonEmptyList.one(Type.TyVar(a)),
          Type.TyApply(Type.TyVar(on), Type.TyVar(a))
        )
      )

    val fnType = Type.ForAll(
      NonEmptyList.one((f, starToStar)),
      Type.Fun(
        NonEmptyList.one(pureType(f)),
        Type.TyApply(Type.TyVar(wrap), Type.TyVar(f))
      )
    )
    val argType = pureType(g)
    val fn = TypedExpr.Local(fnName, fnType, tag)
    val arg = TypedExpr.Local(argName, argType, tag)
    val app = TypedExpr.App(
      fn,
      NonEmptyList.one(arg),
      Type.TyApply(Type.TyVar(wrap), Type.TyVar(g)),
      tag
    )
    val te = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(
        NonEmptyList.of((wrap, wrapperKind), (g, starToStar))
      ),
      app
    )

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(fn -> fnType, arg -> argType)
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

  test("validateTypes rejects inconsistent instantiation of the same free bound var across app args") {
    val a = Type.Var.Bound("a")
    val aType = Type.TyVar(a)
    val fnName = Identifier.Name("f")
    val fnType = Type.Fun(NonEmptyList.of(aType, aType), Type.IntType)
    val fn = TypedExpr.Local(fnName, fnType, tag)
    val arg0 = TypedExpr.Literal(Lit.Integer(1), Type.IntType, tag)
    val arg1 = TypedExpr.Literal(Lit.Str("x"), Type.StrType, tag)
    val app = TypedExpr.App(fn, NonEmptyList.of(arg0, arg1), Type.IntType, tag)
    val te = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      app
    )

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(fn -> fnType)
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
  }

  test("validateTypes rejects inconsistent instantiation of the same free bound var across match branches") {
    val a = Type.Var.Bound("a")
    val aType = Type.TyVar(a)
    val input = Identifier.Name("input")
    val fnName = Identifier.Name("f")
    val fnType = Type.Fun(NonEmptyList.one(aType), aType)
    val scrutinee = TypedExpr.Local(input, Type.IntType, tag)
    val fn = TypedExpr.Local(fnName, fnType, tag)
    val branch0 = TypedExpr.Branch(
      Pattern.WildCard,
      None,
      TypedExpr.App(
        fn,
        NonEmptyList.one(TypedExpr.Literal(Lit.Integer(1), Type.IntType, tag)),
        aType,
        tag
      )
    )(using Region.empty)
    val branch1 = TypedExpr.Branch(
      Pattern.WildCard,
      None,
      TypedExpr.App(
        fn,
        NonEmptyList.one(TypedExpr.Literal(Lit.Str("x"), Type.StrType, tag)),
        aType,
        tag
      )
    )(using Region.empty)
    val m = TypedExpr.Match(scrutinee, NonEmptyList.of(branch0, branch1), tag)
    val te = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      m
    )

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(scrutinee -> Type.IntType, fn -> fnType)
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
  }

  test("validateTypes rejects inconsistent instantiation of the same free bound var across recur args") {
    val a = Type.Var.Bound("a")
    val aType = Type.TyVar(a)
    val seed0 = Identifier.Name("seed0")
    val seed1 = Identifier.Name("seed1")
    val loop0 = Identifier.Name("loop0")
    val loop1 = Identifier.Name("loop1")

    val init0 = TypedExpr.Local(seed0, aType, tag)
    val init1 = TypedExpr.Local(seed1, aType, tag)
    val recur = TypedExpr.Recur(
      NonEmptyList.of(
        TypedExpr.Literal(Lit.Integer(1), Type.IntType, tag),
        TypedExpr.Literal(Lit.Str("x"), Type.StrType, tag)
      ),
      aType,
      tag
    )
    val loop = TypedExpr.Loop(
      NonEmptyList.of((loop0, init0), (loop1, init1)),
      recur,
      tag
    )
    val te = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      loop
    )

    val names: Map[TypedExpr.Name[Unit], Type] =
      Map(init0 -> aType, init1 -> aType)
    val res = TestUtils.validateTypes(te, names, TypeEnv.empty)
    val err = renderErrors(res)
    assert(res.isInvalid, s"expected invalid but got valid: $err")
  }
}
