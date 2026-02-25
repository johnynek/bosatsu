package dev.bosatsu

import cats.Functor
import cats.data.NonEmptyList
import cats.data.Validated

import rankn.{Type, TypeEnv}

import Identifier.Constructor

class TypedTotalityTest extends munit.FunSuite {
  private val pack = PackageName.parts("TypedTotality")

  given Region = Region(0, 1)

  private def toKindedTypeEnv(
      parsed: rankn.ParsedTypeEnv[Option[Kind.Arg]]
  ): TypeEnv[Kind.Arg] = {
    val defs = parsed.allDefinedTypes.map(
      Functor[rankn.DefinedType].map(_) { opt =>
        opt.getOrElse(Kind.invariantTypeArg)
      }
    )
    val fromDefs = TypeEnv.fromDefinitions(defs)
    parsed.externalDefs.foldLeft(fromDefs) { case (acc, (pn, n, tpe)) =>
      acc.addExternalValue(pn, n, tpe)
    }
  }

  private val predefTypeEnv: TypeEnv[Kind.Arg] =
    toKindedTypeEnv(TestUtils.predefParsedTypeEnv)

  private val neverName = TypeName(Constructor("Never"))
  private val resultName = TypeName(Constructor("Result"))
  private val neverType: Type =
    Type.TyConst(Type.Const.Defined(pack, neverName))
  private val resultTypeConst: Type =
    Type.TyConst(Type.Const.Defined(pack, resultName))

  private def resultType(errorType: Type, valueType: Type): Type =
    Type.applyAll(resultTypeConst, errorType :: valueType :: Nil)

  private val neverDt = rankn.DefinedType[Kind.Arg](
    packageName = pack,
    name = neverName,
    annotatedTypeParams = Nil,
    constructors = List(
      rankn.ConstructorFn[Kind.Arg](
        name = Constructor("Never"),
        args = List(
          rankn.ConstructorParam(
            Identifier.Name("never"),
            neverType,
            defaultBinding = None
          )
        )
      )
    )
  )

  private val resultDt = {
    val errV = Type.Var.Bound("err")
    val okV = Type.Var.Bound("ok")
    rankn.DefinedType[Kind.Arg](
      packageName = pack,
      name = resultName,
      annotatedTypeParams =
        List((errV, Kind.invariantTypeArg), (okV, Kind.invariantTypeArg)),
      constructors = List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("Err"),
          args = List(
            rankn.ConstructorParam(
              Identifier.Name("error"),
              Type.TyVar(errV),
              defaultBinding = None
            )
          )
        ),
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("Ok"),
          args = List(
            rankn.ConstructorParam(
              Identifier.Name("value"),
              Type.TyVar(okV),
              defaultBinding = None
            )
          )
        )
      )
    )
  }

  private val typeEnv: TypeEnv[Kind.Arg] =
    predefTypeEnv ++ TypeEnv.fromDefinitions(neverDt :: resultDt :: Nil)

  private def tag(name: String): Declaration =
    Declaration.Var(Identifier.Name(name))

  private def intExpr(value: Int): TypedExpr[Declaration] =
    TypedExpr.Literal(Lit.fromInt(value), Type.IntType, tag(s"i$value"))

  private def boolExpr(value: Boolean): TypedExpr[Declaration] =
    TypedExpr.Global(
      PackageName.PredefName,
      Constructor(if (value) "True" else "False"),
      Type.BoolType,
      tag(if (value) "true" else "false")
    )

  private def mkMatch(
      scrutineeType: Type,
      branches: NonEmptyList[TypedExpr.Branch[Declaration]]
  ): TypedExpr.Match[Declaration] = {
    val argName = Identifier.Name("arg")
    val argTag = Declaration.Var(argName)
    val arg = TypedExpr.Local(argName, scrutineeType, argTag)
    TypedExpr.Match(arg, branches, tag("match"))
  }

  private def branch(
      pattern: Pattern[(PackageName, Constructor), Type],
      guard: Option[TypedExpr[Declaration]] = None,
      value: Int = 0
  ): TypedExpr.Branch[Declaration] =
    TypedExpr.Branch(pattern, guard, intExpr(value))

  private def errors(
      expr: TypedExpr[Declaration]
  ): List[TotalityCheck.ExprError[Declaration]] =
    TotalityCheck(typeEnv).checkExpr(expr) match {
      case Validated.Valid(()) => Nil
      case Validated.Invalid(nel) =>
        nel.toList
    }

  private def hasNonTotal(
      errs: List[TotalityCheck.ExprError[Declaration]]
  ): Boolean =
    errs.exists {
      case _: TotalityCheck.NonTotalMatch[Declaration] => true
      case _                                            => false
    }

  private def hasUnreachable(
      errs: List[TotalityCheck.ExprError[Declaration]]
  ): Boolean =
    errs.exists {
      case _: TotalityCheck.UnreachableBranches[Declaration] => true
      case _                                                 => false
    }

  test("Option[Never] can omit impossible Some branch") {
    val optionNever = Type.apply1(Type.OptionType, neverType)
    val nonePat =
      Pattern.PositionalStruct((PackageName.PredefName, Constructor("None")), Nil)

    val errs = errors(
      mkMatch(optionNever, NonEmptyList.one(branch(nonePat, value = 1)))
    )
    assert(!hasNonTotal(errs), errs.toString)
  }

  test("wildcard-first branch still makes later constructor branch unreachable") {
    val truePat =
      Pattern.PositionalStruct((PackageName.PredefName, Constructor("True")), Nil)

    val errs = errors(
      mkMatch(
        Type.BoolType,
        NonEmptyList.of(
          branch(Pattern.WildCard, value = 0),
          branch(truePat, value = 1)
        )
      )
    )
    assert(hasUnreachable(errs), errs.toString)
  }

  test("Result[Never, a] can be total with only Ok branch") {
    val okPat = Pattern.PositionalStruct((pack, Constructor("Ok")), Pattern.WildCard :: Nil)
    val errs = errors(
      mkMatch(resultType(neverType, Type.IntType), NonEmptyList.one(branch(okPat)))
    )
    assert(!hasNonTotal(errs), errs.toString)
  }

  test("guarded branches still do not count toward coverage") {
    val truePat =
      Pattern.PositionalStruct((PackageName.PredefName, Constructor("True")), Nil)

    val errs = errors(
      mkMatch(
        Type.BoolType,
        NonEmptyList.one(branch(truePat, guard = Some(boolExpr(true)), value = 1))
      )
    )
    assert(hasNonTotal(errs), errs.toString)
  }

  test("definitely-uninhabited scrutinee is vacuously total") {
    val errs = errors(
      mkMatch(
        neverType,
        NonEmptyList.one(
          branch(Pattern.WildCard, guard = Some(boolExpr(true)), value = 1)
        )
      )
    )
    assert(!hasNonTotal(errs), errs.toString)
  }

  test("structural invalid patterns are still reported") {
    val invalidListPat = Pattern.ListPat(
      Pattern.ListPart.NamedList(Identifier.Name("a")) ::
        Pattern.ListPart.NamedList(Identifier.Name("b")) ::
        Pattern.ListPart.Item(Pattern.WildCard) ::
        Nil
    )

    val errs = errors(
      mkMatch(
        Type.apply1(Type.ListType, Type.IntType),
        NonEmptyList.one(branch(invalidListPat))
      )
    )

    assert(
      errs.exists {
        case TotalityCheck.InvalidPattern(
              _,
              TotalityCheck.MultipleSplicesInPattern(_, _)
            ) =>
          true
        case _ => false
      },
      errs.toString
    )
  }

  test("typed constructor invariants are internal errors, not user-facing pattern errors") {
    val missingPat = Pattern.PositionalStruct((pack, Constructor("Missing")), Nil)
    val errs = errors(
      mkMatch(Type.IntType, NonEmptyList.one(branch(missingPat)))
    )

    assert(
      errs.exists {
        case TotalityCheck.InternalInvariantViolation(_, msg) =>
          msg.contains("internal invariant violation")
        case _ => false
      },
      errs.toString
    )
  }
}
