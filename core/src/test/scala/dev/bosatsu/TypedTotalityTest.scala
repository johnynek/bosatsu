package dev.bosatsu

import cats.data.{NonEmptyList, Validated}
import dev.bosatsu.rankn.{Type, TypeEnv}

import Identifier.{Constructor, Name}

class TypedTotalityTest extends munit.FunSuite {
  given Region = Region(0, 1)

  private val pack = PackageName.parts("TypedTotality")
  private val neverName = TypeName(Constructor("Never"))
  private val resultName = TypeName(Constructor("Result"))

  private val neverType: Type.TyConst =
    Type.TyConst(Type.Const.Defined(pack, neverName))
  private val resultType: Type.TyConst =
    Type.TyConst(Type.Const.Defined(pack, resultName))

  private val typeEnv: TypeEnv[Kind.Arg] = {
    val errVar = Type.Var.Bound("e")
    val okVar = Type.Var.Bound("a")

    def mkParam(name: String, tpe: Type): rankn.ConstructorParam =
      rankn.ConstructorParam(
        name = Name(name),
        tpe = tpe,
        defaultBinding = None
      )

    val neverDt = rankn.DefinedType[Kind.Arg](
      packageName = pack,
      name = neverName,
      annotatedTypeParams = Nil,
      constructors = List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("Never"),
          args = List(mkParam("next", neverType))
        )
      )
    )

    val resultDt = rankn.DefinedType[Kind.Arg](
      packageName = pack,
      name = resultName,
      annotatedTypeParams = List(
        (errVar, Kind.invariantTypeArg),
        (okVar, Kind.invariantTypeArg)
      ),
      constructors = List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("Err"),
          args = List(mkParam("err", Type.TyVar(errVar)))
        ),
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("Ok"),
          args = List(mkParam("ok", Type.TyVar(okVar)))
        )
      )
    )

    TypeEnv.fromDefinitions(List(neverDt, resultDt))
  }

  private def decl(name: String): Declaration =
    Declaration.Var(Name(name))

  private def matchTag(name: String): Declaration =
    Declaration.Matches(Declaration.Var(Name(name)), Pattern.WildCard)

  private def intExpr(i: Long, tag: Declaration): TypedExpr[Declaration] =
    TypedExpr.Literal(Lit.Integer(i), Type.IntType, tag)

  private def resultExprType(err: Type, ok: Type): Type =
    Type.applyAll(resultType, err :: ok :: Nil)

  test("Result[Never, Int] with only Ok branch is total") {
    val tag = decl("x")
    val okPat =
      Pattern.PositionalStruct((pack, Constructor("Ok")), Pattern.WildCard :: Nil)
    val scrutinee =
      TypedExpr.Local(Name("x"), resultExprType(neverType, Type.IntType), tag)
    val mTag = matchTag("m0")
    val m = TypedExpr.Match(
      scrutinee,
      NonEmptyList.one(TypedExpr.Branch(okPat, None, intExpr(1L, tag))),
      mTag
    )

    TotalityCheck(typeEnv).checkExpr(m) match {
      case Validated.Valid(())    => ()
      case Validated.Invalid(errs) => fail(errs.toList.mkString(", "))
    }
  }

  test("unreachable detection remains shadowing based") {
    val tag = decl("y")
    val okPat =
      Pattern.PositionalStruct((pack, Constructor("Ok")), Pattern.WildCard :: Nil)
    val scrutinee =
      TypedExpr.Local(Name("y"), resultExprType(Type.IntType, Type.IntType), tag)
    val mTag = matchTag("m1")
    val m = TypedExpr.Match(
      scrutinee,
      NonEmptyList.of(
        TypedExpr.Branch(Pattern.WildCard, None, intExpr(0L, tag)),
        TypedExpr.Branch(okPat, None, intExpr(1L, tag))
      ),
      mTag
    )

    TotalityCheck(typeEnv).checkExpr(m) match {
      case Validated.Invalid(errs) =>
        val hasUnreachableOk = errs.exists {
          case TotalityCheck.UnreachableBranches(_, pats) =>
            pats.toList.contains(okPat)
          case _ => false
        }
        assert(hasUnreachableOk, errs.toList.mkString(", "))
      case Validated.Valid(()) =>
        fail("expected unreachable branch error")
    }
  }

  test("definitively uninhabited scrutinee is vacuously total") {
    val tag = decl("z")
    val scrutinee = TypedExpr.Local(Name("z"), neverType, tag)
    val mTag = matchTag("m2")
    val m = TypedExpr.Match(
      scrutinee,
      NonEmptyList.one(
        TypedExpr.Branch(
          Pattern.Literal(Lit.Integer(0L)),
          None,
          intExpr(0L, tag)
        )
      ),
      mTag
    )

    TotalityCheck(typeEnv).checkExpr(m) match {
      case Validated.Valid(())    => ()
      case Validated.Invalid(errs) => fail(errs.toList.mkString(", "))
    }
  }
}
