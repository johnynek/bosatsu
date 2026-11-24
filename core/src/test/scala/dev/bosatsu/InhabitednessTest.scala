package dev.bosatsu

import cats.Functor
import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.all._
import dev.bosatsu.rankn.{ParsedTypeEnv, Type, TypeEnv}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import Identifier.Constructor

class InhabitednessTest extends munit.ScalaCheckSuite {
  import Inhabitedness.Error
  import Inhabitedness.State

  private val localPackage: PackageName = TestUtils.testPackage

  private def toKindedTypeEnv(
      parsed: ParsedTypeEnv[Option[Kind.Arg]]
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

  private val customTypeEnv: TypeEnv[Kind.Arg] = {
    val neverName = TypeName(Constructor("Never"))
    val mixedName = TypeName(Constructor("Mixed"))
    val neverTyConst = Type.TyConst(Type.Const.Defined(localPackage, neverName))
    val mixedTyConst = Type.TyConst(Type.Const.Defined(localPackage, mixedName))

    val never = rankn.DefinedType[Kind.Arg](
      packageName = localPackage,
      name = neverName,
      annotatedTypeParams = Nil,
      constructors = List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("Never"),
          args = List(
            rankn.ConstructorParam(
              name = Identifier.Name("never"),
              tpe = neverTyConst,
              defaultBinding = None
            )
          )
        )
      )
    )

    val mixed = rankn.DefinedType[Kind.Arg](
      packageName = localPackage,
      name = mixedName,
      annotatedTypeParams = Nil,
      constructors = List(
        rankn.ConstructorFn[Kind.Arg](name = Constructor("Good"), args = Nil),
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("Bad"),
          args = List(
            rankn.ConstructorParam(
              name = Identifier.Name("never"),
              tpe = neverTyConst,
              defaultBinding = None
            )
          )
        )
      )
    )

    // Touch both roots so test setup fails early if constructor/type wiring is broken.
    val _ = (neverTyConst, mixedTyConst)
    TypeEnv.fromDefinitions(List(never, mixed))
  }

  private val predefTypeEnv: TypeEnv[Kind.Arg] =
    toKindedTypeEnv(TestUtils.predefParsedTypeEnv)

  private val nonTrivialEnv: TypeEnv[Kind.Arg] =
    predefTypeEnv ++ customTypeEnv

  private def localType(name: String): Type.TyConst =
    Type.TyConst(Type.Const.Defined(localPackage, TypeName(Constructor(name))))

  private val neverType: Type.TyConst = localType("Never")
  private val mixedType: Type.TyConst = localType("Mixed")

  private def mkParam(name: String, tpe: Type): rankn.ConstructorParam =
    rankn.ConstructorParam(
      name = Identifier.Name(name),
      tpe = tpe,
      defaultBinding = None
    )

  private def mkType(
      name: String,
      constructors: List[rankn.ConstructorFn[Kind.Arg]]
  ): rankn.DefinedType[Kind.Arg] =
    rankn.DefinedType[Kind.Arg](
      packageName = localPackage,
      name = TypeName(Constructor(name)),
      annotatedTypeParams = Nil,
      constructors = constructors
    )

  private val edgeNoPredefEnv: TypeEnv[Kind.Arg] = {
    val q = Type.Var.Bound("q")
    val a = Type.Var.Bound("a")
    val f = Type.Var.Bound("f")
    val many = (0 to 10).toList.map(i => Type.Var.Bound(s"t$i"))
    val badApply = Type.TyApply(mixedType, Type.IntType)
    val nonConstApply = Type.TyApply(Type.TyVar(q), Type.IntType)
    val listOfInt = Type.apply1(Type.ListType, Type.IntType)
    val unknownOrNever = mkType(
      "EdgeUnknownOrNever",
      List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("EdgeUnknown"),
          args = List(mkParam("x", Type.TyVar(q)))
        ),
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("EdgeNever"),
          args = List(mkParam("n", neverType))
        )
      )
    )
    val existsNonType = mkType(
      "EdgeExistsNonType",
      List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("EdgeExistsNonType"),
          args = List(mkParam("x", Type.TyVar(f))),
          exists = List((f, Kind.Arg(Variance.in, Kind(Kind.Type.co))))
        )
      )
    )
    val existsMany = mkType(
      "EdgeExistsMany",
      List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("EdgeExistsMany"),
          args = List(mkParam("x", Type.TyVar(many.head))),
          exists = many.map(_ -> Kind.invariantTypeArg)
        )
      )
    )
    val existsUnknown = mkType(
      "EdgeExistsUnknown",
      List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("EdgeExistsUnknown"),
          args = List(mkParam("a", Type.TyVar(a)), mkParam("q", Type.TyVar(q))),
          exists = List((a, Kind.invariantTypeArg))
        )
      )
    )
    val dt = List(
      mkType("EdgeOpaque", Nil),
      mkType(
        "EdgeMissingBuiltin",
        List(
          rankn.ConstructorFn[Kind.Arg](
            name = Constructor("EdgeMissingBuiltin"),
            args = List(mkParam("x", Type.ListType))
          )
        )
      ),
      mkType(
        "EdgeFreeVar",
        List(
          rankn.ConstructorFn[Kind.Arg](
            name = Constructor("EdgeFreeVar"),
            args = List(mkParam("x", Type.TyVar(q)))
          )
        )
      ),
      mkType(
        "EdgeBadApply",
        List(
          rankn.ConstructorFn[Kind.Arg](
            name = Constructor("EdgeBadApply"),
            args = List(mkParam("x", badApply))
          )
        )
      ),
      mkType(
        "EdgeNonConstApply",
        List(
          rankn.ConstructorFn[Kind.Arg](
            name = Constructor("EdgeNonConstApply"),
            args = List(mkParam("x", nonConstApply))
          )
        )
      ),
      mkType(
        "EdgeFnUnknown",
        List(
          rankn.ConstructorFn[Kind.Arg](
            name = Constructor("EdgeFnUnknown"),
            args = List(mkParam("x", Type.Fun(Type.IntType, listOfInt)))
          )
        )
      ),
      existsNonType,
      existsMany,
      existsUnknown,
      unknownOrNever
    )
    customTypeEnv ++ TypeEnv.fromDefinitions(dt)
  }

  private val edgeFakeListEnv: TypeEnv[Kind.Arg] = {
    val a = Type.Var.Bound("a")
    val fakeList = rankn.DefinedType[Kind.Arg](
      packageName = PackageName.PredefName,
      name = TypeName(Constructor("List")),
      annotatedTypeParams = List((a, Kind.invariantTypeArg)),
      constructors = List(
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("EmptyList"),
          args = List(mkParam("x", Type.TyVar(a)))
        ),
        rankn.ConstructorFn[Kind.Arg](
          name = Constructor("NonEmptyList"),
          args = List(mkParam("x", Type.TyVar(a)))
        )
      )
    )
    edgeNoPredefEnv ++ TypeEnv.fromDefinitions(fakeList :: Nil)
  }

  private val edgeOpaqueType: Type.TyConst = localType("EdgeOpaque")
  private val edgeMissingBuiltinType: Type.TyConst = localType("EdgeMissingBuiltin")
  private val edgeFreeVarType: Type.TyConst = localType("EdgeFreeVar")
  private val edgeBadApplyType: Type.TyConst = localType("EdgeBadApply")
  private val edgeNonConstApplyType: Type.TyConst = localType("EdgeNonConstApply")
  private val edgeExistsNonTypeType: Type.TyConst = localType("EdgeExistsNonType")
  private val edgeExistsManyType: Type.TyConst = localType("EdgeExistsMany")
  private val edgeExistsUnknownType: Type.TyConst = localType("EdgeExistsUnknown")
  private val edgeUnknownOrNeverType: Type.TyConst = localType("EdgeUnknownOrNever")
  private val edgeFnUnknownType: Type.TyConst = localType("EdgeFnUnknown")

  private def typeIfPresent(
      env: TypeEnv[Kind.Arg],
      tpe: Type.TyConst
  ): Option[Type.TyConst] =
    env.getType(tpe).map(_ => tpe)

  private def assertState(
      result: Inhabitedness.Result[State],
      expected: State
  )(implicit loc: munit.Location): Unit =
    result match {
      case Validated.Valid(state) =>
        assertEquals(state, expected)
      case Validated.Invalid(errs) =>
        fail(s"expected $expected, got errors: ${errs.toList}")
    }

  private def expectInvalid(
      result: Inhabitedness.Result[State]
  )(pred: Error => Boolean): Unit =
    result match {
      case Validated.Valid(state) =>
        fail(s"expected error, got $state")
      case Validated.Invalid(errs) =>
        assert(errs.toList.exists(pred), errs.toList.toString)
    }

  private val forallA_A: Type = {
    val a = Type.Var.Bound("a")
    Type.forAll(List((a, Kind.Type)), Type.TyVar(a))
  }

  private val forallA_AtoA: Type = {
    val a = Type.Var.Bound("a")
    Type.forAll(
      List((a, Kind.Type)),
      Type.Fun(Type.TyVar(a), Type.TyVar(a))
    )
  }

  test("check classifies forall a. a as uninhabited") {
    assertState(
      Inhabitedness.check(forallA_A, nonTrivialEnv),
      State.Uninhabited
    )
  }

  test("check classifies recursive self-field struct as uninhabited") {
    assertState(
      Inhabitedness.check(neverType, nonTrivialEnv),
      State.Uninhabited
    )
  }

  test("check applies function inhabitedness rules") {
    val unitToNever = Type.Fun(Type.UnitType, neverType)
    val neverToNever = Type.Fun(neverType, neverType)

    assertState(
      Inhabitedness.check(unitToNever, nonTrivialEnv),
      State.Uninhabited
    )
    assertState(
      Inhabitedness.check(neverToNever, nonTrivialEnv),
      State.Inhabited
    )
  }

  test("checkMatch can distinguish reachable and unreachable constructors") {
    val optionNever = Type.apply1(Type.OptionType, neverType)
    val somePat =
      Pattern.PositionalStruct(
        (PackageName.PredefName, Constructor("Some")),
        Pattern.WildCard :: Nil
      )
    val nonePat =
      Pattern.PositionalStruct(
        (PackageName.PredefName, Constructor("None")),
        Nil
      )

    assertState(
      Inhabitedness.checkMatch(optionNever, somePat, nonTrivialEnv),
      State.Uninhabited
    )
    assertState(
      Inhabitedness.checkMatch(optionNever, nonePat, nonTrivialEnv),
      State.Inhabited
    )
  }

  test("checkMatch handles local enum constructors with uninhabited fields") {
    val badPat =
      Pattern.PositionalStruct(
        (localPackage, Constructor("Bad")),
        Pattern.WildCard :: Nil
      )
    val goodPat =
      Pattern.PositionalStruct(
        (localPackage, Constructor("Good")),
        Nil
      )

    assertState(
      Inhabitedness.checkMatch(mixedType, badPat, nonTrivialEnv),
      State.Uninhabited
    )
    assertState(
      Inhabitedness.checkMatch(mixedType, goodPat, nonTrivialEnv),
      State.Inhabited
    )
  }

  test("check reports type-level validation errors") {
    expectInvalid(Inhabitedness.check(Type.OptionType, nonTrivialEnv)) {
      case Error.TypeNotValueKind(_, _) => true
      case _                            => false
    }

    val missingType =
      Type.TyConst(
        Type.Const.Defined(localPackage, TypeName(Constructor("MissingType")))
      )
    expectInvalid(Inhabitedness.check(missingType, nonTrivialEnv)) {
      case Error.UnknownTypeConstructor(_) => true
      case _                               => false
    }

    expectInvalid(Inhabitedness.check(Type.TyVar(Type.Var.Bound("u")), nonTrivialEnv)) {
      case Error.UnknownTypeVariable(_) => true
      case _                            => false
    }

    expectInvalid(
      Inhabitedness.check(Type.TyApply(Type.IntType, Type.IntType), nonTrivialEnv)
    ) {
      case Error.IllKindedTypeApply(_) => true
      case _                           => false
    }

    expectInvalid(
      Inhabitedness.check(
        Type.TyApply(Type.OptionType, Type.OptionType),
        nonTrivialEnv
      )
    ) {
      case Error.KindSubsumptionError(_, _, _) => true
      case _                                   => false
    }
  }

  test("checkMatch reports pattern validation errors and accumulates them") {
    val unknown1 = Pattern.PositionalStruct((localPackage, Constructor("Nope1")), Nil)
    val unknown2 = Pattern.PositionalStruct((localPackage, Constructor("Nope2")), Nil)
    val badAnn = Pattern.Annotation(Pattern.WildCard, Type.OptionType)

    val pattern =
      Pattern.Union(
        unknown1,
        NonEmptyList.of(unknown2, badAnn)
      )
    val res = Inhabitedness.checkMatch(Type.IntType, pattern, nonTrivialEnv)

    res match {
      case Validated.Valid(state) =>
        fail(s"expected accumulated validation errors, got state: $state")
      case Validated.Invalid(errs) =>
        assert(errs.exists { case Error.UnknownConstructor(_) => true; case _ => false })
        assert(errs.exists { case Error.TypeNotValueKind(_, _) => true; case _ => false })
        assert(errs.size >= 3, errs.toList.toString)
    }
  }

  test("checkMatch reports constructor arity mismatches") {
    val tooManyArgs =
      Pattern.PositionalStruct(
        (PackageName.PredefName, Constructor("None")),
        Pattern.WildCard :: Nil
      )
    expectInvalid(
      Inhabitedness.checkMatch(
        Type.apply1(Type.OptionType, Type.IntType),
        tooManyArgs,
        nonTrivialEnv
      )
    ) {
      case Error.ConstructorArityMismatch(_, _, _) => true
      case _                                       => false
    }
  }

  test("checkMatch handles list and string patterns") {
    val listType = Type.apply1(Type.ListType, Type.IntType)
    val partialList =
      Pattern.ListPat(
        Pattern.ListPart.WildList ::
          Pattern.ListPart.Item(Pattern.Literal(Lit.Integer(1L))) :: Nil
      )
    val prefixedListOnNonList =
      Pattern.ListPat(
        Pattern.ListPart.WildList ::
          Pattern.ListPart.Item(Pattern.Literal(Lit.Integer(1L))) :: Nil
      )
    val prefixedListWithBadItem =
      Pattern.ListPat(
        Pattern.ListPart.WildList ::
          Pattern.ListPart.Item(Pattern.StrPat.fromLitStr("x")) :: Nil
      )
    val strPat = Pattern.StrPat.fromLitStr("abc")

    assertState(
      Inhabitedness.checkMatch(listType, partialList, nonTrivialEnv),
      State.Inhabited
    )
    assertState(
      Inhabitedness.checkMatch(Type.IntType, prefixedListOnNonList, nonTrivialEnv),
      State.Unknown
    )
    assertState(
      Inhabitedness.checkMatch(listType, prefixedListWithBadItem, nonTrivialEnv),
      State.Uninhabited
    )
    assertState(
      Inhabitedness.checkMatch(Type.StrType, strPat, nonTrivialEnv),
      State.Inhabited
    )
    assertState(
      Inhabitedness.checkMatch(Type.IntType, strPat, nonTrivialEnv),
      State.Uninhabited
    )
  }

  test("check covers unknown-state and malformed environment fallbacks") {
    assertState(Inhabitedness.check(edgeOpaqueType, edgeNoPredefEnv), State.Unknown)
    assertState(
      Inhabitedness.check(edgeMissingBuiltinType, edgeNoPredefEnv),
      State.Unknown
    )
    assertState(Inhabitedness.check(edgeFreeVarType, edgeNoPredefEnv), State.Unknown)
    assertState(Inhabitedness.check(edgeBadApplyType, edgeNoPredefEnv), State.Unknown)
    assertState(
      Inhabitedness.check(edgeNonConstApplyType, edgeNoPredefEnv),
      State.Unknown
    )
    assertState(
      Inhabitedness.check(edgeExistsNonTypeType, edgeNoPredefEnv),
      State.Unknown
    )
    assertState(
      Inhabitedness.check(edgeExistsManyType, edgeNoPredefEnv),
      State.Unknown
    )
    assertState(
      Inhabitedness.check(edgeExistsUnknownType, edgeNoPredefEnv),
      State.Unknown
    )
    assertState(
      Inhabitedness.check(edgeUnknownOrNeverType, edgeNoPredefEnv),
      State.Unknown
    )
    assertState(Inhabitedness.check(edgeFnUnknownType, edgeNoPredefEnv), State.Unknown)
  }

  test("check handles existential and quantifier edge cases") {
    val a = Type.Var.Bound("a")
    val existsA = Type.exists(List((a, Kind.Type)), Type.TyVar(a))
    assertState(Inhabitedness.check(existsA, nonTrivialEnv), State.Inhabited)

    val many = (0 to 10).toList.map(i => (Type.Var.Bound(s"x$i"), Kind.Type))
    val hugeBody = Type.Tuple(many.map { case (v, _) => (Type.TyVar(v): Type) })
    val hugeForAll = Type.forAll(many, hugeBody)
    assertState(Inhabitedness.check(hugeForAll, nonTrivialEnv), State.Unknown)

    val listUnknownForAll =
      Type.forAll(List((a, Kind.Type)), Type.apply1(Type.ListType, Type.TyVar(a)))
    val listUnknownExists =
      Type.exists(List((a, Kind.Type)), Type.apply1(Type.ListType, Type.TyVar(a)))
    assertState(Inhabitedness.check(listUnknownForAll, predefTypeEnv), State.Inhabited)
    assertState(Inhabitedness.check(listUnknownExists, predefTypeEnv), State.Inhabited)

    val skolem =
      Type.TyVar(Type.Var.Skolem("s", Kind.Type, existential = false, id = 7L))
    assertState(Inhabitedness.check(skolem, nonTrivialEnv), State.Unknown)
  }

  test("checkMatch covers named, annotation, union and positional unknown paths") {
    val namedPat = Pattern.Named(Identifier.Name("x"), Pattern.WildCard)
    assertState(
      Inhabitedness.checkMatch(Type.IntType, namedPat, nonTrivialEnv),
      State.Inhabited
    )

    val annInh = Pattern.Annotation(Pattern.WildCard, Type.IntType)
    assertState(
      Inhabitedness.checkMatch(Type.IntType, annInh, nonTrivialEnv),
      State.Inhabited
    )
    val annFnSameNonConst =
      Pattern.Annotation(
        Pattern.WildCard,
        forallA_AtoA
      )
    assertState(
      Inhabitedness.checkMatch(
        forallA_AtoA,
        annFnSameNonConst,
        nonTrivialEnv
      ),
      State.Unknown
    )

    val annUnknown = Pattern.Annotation(Pattern.WildCard, edgeOpaqueType)
    assertState(
      Inhabitedness.checkMatch(edgeOpaqueType, annUnknown, edgeNoPredefEnv),
      State.Unknown
    )
    val annDisjoint = Pattern.Annotation(Pattern.WildCard, Type.StrType)
    assertState(
      Inhabitedness.checkMatch(Type.IntType, annDisjoint, nonTrivialEnv),
      State.Uninhabited
    )
    val annUninh = Pattern.Annotation(Pattern.WildCard, neverType)
    assertState(
      Inhabitedness.checkMatch(neverType, annUninh, nonTrivialEnv),
      State.Uninhabited
    )

    val litInt = Pattern.Literal(Lit.Integer(2L))
    assertState(
      Inhabitedness.checkMatch(Type.IntType, litInt, nonTrivialEnv),
      State.Inhabited
    )
    assertState(
      Inhabitedness.checkMatch(neverType, litInt, nonTrivialEnv),
      State.Uninhabited
    )
    assertState(
      Inhabitedness.checkMatch(edgeOpaqueType, litInt, edgeNoPredefEnv),
      State.Uninhabited
    )
    val litUnknown =
      Pattern.PositionalStruct(
        (localPackage, Constructor("EdgeFreeVar")),
        Pattern.Literal(Lit.Integer(1L)) :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(edgeFreeVarType, litUnknown, edgeNoPredefEnv),
      State.Uninhabited
    )

    val unknownUnion = Pattern.Union(
      Pattern.WildCard,
      NonEmptyList.one(
        Pattern.PositionalStruct((localPackage, Constructor("Good")), Nil)
      )
    )
    assertState(
      Inhabitedness.checkMatch(edgeOpaqueType, unknownUnion, edgeNoPredefEnv),
      State.Unknown
    )

    val listRight =
      Pattern.ListPat(
        Pattern.ListPart.Item(Pattern.WildCard) ::
          Pattern.ListPart.WildList :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(
        Type.apply1(Type.ListType, Type.IntType),
        listRight,
        nonTrivialEnv
      ),
      State.Inhabited
    )
    assertState(
      Inhabitedness.checkMatch(
        Type.apply1(Type.ListType, Type.IntType),
        listRight,
        predefTypeEnv
      ),
      State.Inhabited
    )
    assertState(
      Inhabitedness.checkMatch(
        Type.apply1(Type.ListType, Type.IntType),
        listRight,
        predefTypeEnv ++ edgeFakeListEnv
      ),
      State.Unknown
    )
    val listLeftOnUninh =
      Pattern.ListPat(
        Pattern.ListPart.WildList ::
          Pattern.ListPart.Item(Pattern.Literal(Lit.Integer(3L))) :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(neverType, listLeftOnUninh, nonTrivialEnv),
      State.Uninhabited
    )

    val strUnknown =
      Pattern.PositionalStruct(
        (localPackage, Constructor("EdgeFreeVar")),
        Pattern.StrPat.fromLitStr("x") :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(edgeFreeVarType, strUnknown, edgeNoPredefEnv),
      State.Uninhabited
    )

    val badApplyPattern =
      Pattern.PositionalStruct(
        (localPackage, Constructor("EdgeBadApply")),
        Pattern.PositionalStruct((localPackage, Constructor("Good")), Nil) :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(edgeBadApplyType, badApplyPattern, edgeNoPredefEnv),
      State.Unknown
    )

    val nonConstApplyPattern =
      Pattern.PositionalStruct(
        (localPackage, Constructor("EdgeNonConstApply")),
        Pattern.PositionalStruct((localPackage, Constructor("Good")), Nil) :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(
        edgeNonConstApplyType,
        nonConstApplyPattern,
        edgeNoPredefEnv
      ),
      State.Unknown
    )
    val strUninhByExists =
      Pattern.PositionalStruct(
        (localPackage, Constructor("EdgeExistsUnknown")),
        Pattern.StrPat.fromLitStr("x") :: Pattern.WildCard :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(
        edgeExistsUnknownType,
        strUninhByExists,
        edgeNoPredefEnv
      ),
      State.Uninhabited
    )
    val litUninhByExists =
      Pattern.PositionalStruct(
        (localPackage, Constructor("EdgeExistsUnknown")),
        Pattern.Literal(Lit.Integer(4L)) :: Pattern.WildCard :: Nil
      )
    assertState(
      Inhabitedness.checkMatch(
        edgeExistsUnknownType,
        litUninhByExists,
        edgeNoPredefEnv
      ),
      State.Uninhabited
    )
  }

  private def sourceTypeEnv(src: String): TypeEnv[Kind.Arg] =
    predefTypeEnv ++ toKindedTypeEnv(TestUtils.parsedTypeEnvOf(localPackage, src))

  private val captureEnv: TypeEnv[Kind.Arg] =
    sourceTypeEnv("""#
enum Never: Never(next: Never)
enum T[x]: Base(v: x), Step[a](t: T[a])
enum Box[x]: MkBox[a](h: x, k: a -> Never)
enum Loop[x]: Loop[a](l: Loop[a])
enum D[x]: A(v: x), B(w: exists y. D[y])
enum E[x]: EA(v: x), EB(w: forall y. E[y])
enum P4[w, x, y, z]: Base4(w: w, x: x, y: y, z: z), Step4[a, b, c, d](p: P4[a, b, c, d])
enum Loop4[w, x, y, z]: Loop4[a, b, c, d](l: Loop4[a, b, c, d])
enum Q4[w, x, y, z]: QBase4(w: w, x: x, y: y, z: z), QStep4(q: forall a, b, c, d. Q4[a, b, c, d])
struct DoubleNeg(f: (DoubleNeg -> Never) -> Never)
struct Wrap[x](w: x)
enum Nested[x]: NBase(v: x), NStep(n: Nested[Wrap[x]])
enum NestedLoop[x]: NLoop(n: NestedLoop[Wrap[x]])
# Not covariant, so the compiler rejects it; only a hand-built env has it.
struct Liar(f: Liar -> Never)
struct Pair[a, b](fst: a, snd: b)
enum Twice[x]: TBase(v: x), TStep(t: Twice[Pair[x, x]])
enum TwiceLoop[x]: TLoop(t: TwiceLoop[Pair[x, x]])
struct Tok
enum Wrapper[x, y]: W1(v: y), W2(f: x -> Never)
# Rec occurs covariantly (under two contravariant parameters), so the
# compiler accepts it, but its component reads Wrapper[Rec, Tok]
# contravariantly.
struct Rec(w: Wrapper[Wrapper[Rec, Tok], Rec])
struct Wrap2[x](w: x)
enum Branch[x]: BLeft(l: Branch[Wrap[x]]), BRight(r: Branch[Wrap2[x]])
struct SelfWithFn(s: SelfWithFn, f: Never -> Tok)
struct Neg[a](f: a -> Never)
struct DN2(f: Neg[Neg[DN2]])
enum DNLoop4[w, x, y, z]: DNL4[a, b, c, d](f: Neg[Neg[DNLoop4[a, b, c, d]]])
enum NR: NR1(r: NR), NR2(g: Neg[NR] -> Never)
enum NegT[a]: N1(n: NegT[a]), N2(f: a -> Never)
enum NTR: NT1(r: NTR), NT2(g: NegT[NTR] -> Never)
struct RecN(w: Wrapper[Wrapper[RecN, Never], RecN])
""")

  private def quantified(
      q: (List[(Type.Var.Bound, Kind)], Type) => Type,
      name: String,
      tc: Type.TyConst
  ): Type = {
    val v = Type.Var.Bound(name)
    q(List((v, Kind.Type)), Type.apply1(tc, Type.TyVar(v)))
  }

  test("constructor existentials do not capture an outer variable in a recursive field") {
    // Type.normalize names the quantified variable `a`, the same name as
    // Step's existential. Step(Base(1)) has type forall a. T[a].
    List("a", "b").foreach { name =>
      assertState(
        Inhabitedness.check(
          quantified(Type.forAll(_, _), name, localType("T")),
          captureEnv
        ),
        State.Inhabited
      )
    }
  }

  test("constructor existentials do not capture an outer variable through type arguments") {
    // MkBox(1, x -> x) has type Box[Int] (with a = Never), so exists a. Box[a]
    // has a value. Substituting the outer `a` for `x` must not let MkBox's
    // existential `a` capture it.
    val existsBox = quantified(Type.exists(_, _), "a", localType("Box"))
    assertState(Inhabitedness.check(existsBox, captureEnv), State.Inhabited)

    val mkBox = Pattern.PositionalStruct(
      (localPackage, Constructor("MkBox")),
      List(Pattern.WildCard, Pattern.WildCard)
    )
    assertState(
      Inhabitedness.checkMatch(existsBox, mkBox, captureEnv),
      State.Inhabited
    )
  }

  test("quantifiers inside field types do not capture an outer variable in a recursive field") {
    // `forall a. D[a]` and B's field `exists y. D[y]` (normalized to
    // `exists a. D[a]`) use the same name for different variables.
    // B(A(1)) has type forall a. D[a].
    assertState(
      Inhabitedness.check(
        quantified(Type.forAll(_, _), "b", localType("D")),
        captureEnv
      ),
      State.Inhabited
    )
    // forall b. E[b] has no value: E[Never] would need a Never (EA) or a
    // forall y. E[y], which is again an E[Never] (EB). The field's E[a] at
    // a = Uninhabited has the same key as the E[a] being expanded, or the
    // keys would never end.
    assertState(
      Inhabitedness.check(
        quantified(Type.forAll(_, _), "b", localType("E")),
        captureEnv
      ),
      State.Uninhabited
    )
  }

  test("renaming existentials apart keeps recursive expansion finite") {
    // Loop has no base case. Each expansion opens a fresh existential, so
    // Loop[a] and Loop[b] (same state) must have the same key.
    val quantifiers: List[(List[(Type.Var.Bound, Kind)], Type) => Type] =
      List(Type.forAll(_, _), Type.exists(_, _))
    quantifiers.foreach { q =>
      assertState(
        Inhabitedness.check(quantified(q, "a", localType("Loop")), captureEnv),
        State.Uninhabited
      )
    }
  }

  test("types that open many variables per expansion are solved exactly") {
    // Each expansion of these types opens four kind-* variables, so there are
    // 16 data-type keys, and a search that only stops at keys on its current
    // path has up to 16! paths. The least fixed point needs 16 equations.
    def applied4(name: String, args: List[Type]): Type =
      Type.applyAll(localType(name), args)
    val ints = List.fill(4)(Type.IntType: Type)
    val vars = List("a", "b", "c", "d").map(Type.Var.Bound(_))
    def forall4(name: String): Type =
      Type.forAll(vars.map((_, Kind.Type)), applied4(name, vars.map(Type.TyVar(_))))

    assertState(Inhabitedness.check(applied4("P4", ints), captureEnv), State.Inhabited)
    assertState(Inhabitedness.check(applied4("Q4", ints), captureEnv), State.Inhabited)
    // P4[a, b, c, d] has Step4(Base4(1, 1, 1, 1)) for every a, b, c, d.
    assertState(Inhabitedness.check(forall4("P4"), captureEnv), State.Inhabited)
    // Loop4 has no base case.
    assertState(Inhabitedness.check(forall4("Loop4"), captureEnv), State.Uninhabited)
    assertState(Inhabitedness.check(applied4("Loop4", ints), captureEnv), State.Uninhabited)
  }

  test("recursion through two function arguments is covariant and solved") {
    // DoubleNeg occurs to the left of two arrows in its own definition. Its
    // least fixed point is empty: DoubleNeg -> Never is then Inhabited, so
    // (DoubleNeg -> Never) -> Never is Uninhabited.
    assertState(
      Inhabitedness.check(localType("DoubleNeg"), captureEnv),
      State.Uninhabited
    )
  }

  test("a data type that reads itself under one function argument is Unknown") {
    // KindFormula rejects Liar, so only an unchecked env like this one has
    // it. Liar = Liar -> Never has no least fixed point: assuming Liar empty
    // makes it inhabited, and assuming it inhabited makes it empty. The
    // solver answers Unknown rather than rely on KindFormula.
    assertState(Inhabitedness.check(localType("Liar"), captureEnv), State.Unknown)
  }

  test("a component read under an even number of function arguments is searched") {
    // Rec = Wrapper[Wrapper[Rec, Tok], Rec] = Rec or not Wrapper[Rec, Tok],
    // and Wrapper[Rec, Tok] = Tok or not Rec, which is Inhabited. So Rec is
    // empty. Iterating all three keys up from Uninhabited would instead read
    // Wrapper[Rec, Tok] as Uninhabited at first, make Rec Inhabited, and
    // never come back down. Every cycle passes two function arguments, so
    // the depth-first search with the cycle rule solves them.
    val rec = localType("Rec")
    val inner = Type.applyAll(localType("Wrapper"), List(rec, localType("Tok")))
    val outer = Type.applyAll(localType("Wrapper"), List(inner, rec))
    assertState(Inhabitedness.check(rec, captureEnv), State.Uninhabited)
    assertState(Inhabitedness.check(outer, captureEnv), State.Uninhabited)
    assertState(Inhabitedness.check(inner, captureEnv), State.Inhabited)
    // (None, absurd) is a value of the tuple.
    val tuple = Type.Tuple(
      List(Type.apply1(Type.OptionType, outer), Type.Fun(rec, neverType))
    )
    assertState(Inhabitedness.check(tuple, captureEnv), State.Inhabited)

    // DN2 = Neg[Neg[DN2]] = not not DN2, so it is empty.
    assertState(Inhabitedness.check(localType("DN2"), captureEnv), State.Uninhabited)
  }

  test("a function from an empty type is inhabited, inside a data type too") {
    val neg = localType("Neg")
    // any argument without values makes the function inhabited
    assertState(
      Inhabitedness.check(
        Type.Fun(NonEmptyList.of(Type.IntType, neverType), neverType),
        captureEnv
      ),
      State.Inhabited
    )
    // Neg[Never] holds `n -> n`, and Neg[Int] holds nothing
    assertState(
      Inhabitedness.check(Type.apply1(neg, neverType), captureEnv),
      State.Inhabited
    )
    assertState(
      Inhabitedness.check(Type.apply1(neg, Type.IntType), captureEnv),
      State.Uninhabited
    )
    // through a variable's state: Neg[a] has a value when a has none
    assertState(
      Inhabitedness.check(quantified(Type.exists(_, _), "a", neg), captureEnv),
      State.Inhabited
    )
    assertState(
      Inhabitedness.check(quantified(Type.forAll(_, _), "a", neg), captureEnv),
      State.Uninhabited
    )
  }

  test("the search nests a component's fixed points by type level") {
    // NR has no values, so NR -> Never holds a total `absurd`, and
    // Neg[NR] has a value. Assuming Neg[NR] empty while expanding it made
    // NR2 buildable, so NR inhabited and Neg[NR] empty. Neg[NR] is Neg
    // applied to NR's answer, so NR's fixed point is the outer one.
    val nr = localType("NR")
    assertState(Inhabitedness.check(nr, captureEnv), State.Uninhabited)
    assertState(
      Inhabitedness.check(Type.apply1(localType("Neg"), nr), captureEnv),
      State.Inhabited
    )

    // the same when the wrapper is recursive itself: NegT[NTR] assumes
    // itself empty through N1, its own recursion, but not through NTR
    val ntr = localType("NTR")
    assertState(Inhabitedness.check(ntr, captureEnv), State.Uninhabited)
    assertState(
      Inhabitedness.check(Type.apply1(localType("NegT"), ntr), captureEnv),
      State.Inhabited
    )

    // Rec with Never for Tok: Wrapper[RecN, Never] = Never or not RecN,
    // which is Inhabited because RecN is empty
    val recN = localType("RecN")
    val inner = Type.applyAll(localType("Wrapper"), List(recN, neverType))
    val outer = Type.applyAll(localType("Wrapper"), List(inner, recN))
    assertState(Inhabitedness.check(recN, captureEnv), State.Uninhabited)
    assertState(Inhabitedness.check(inner, captureEnv), State.Inhabited)
    assertState(Inhabitedness.check(outer, captureEnv), State.Uninhabited)
  }

  test("the search runs on an env without the types its definitions mention") {
    // TotalityCheck's lint replay uses only a package's own types, so a
    // type their definitions mention can be missing from the env, as Tok
    // is here. Levels were then computed with an empty first layer, and
    // the search threw.
    val tok: Type.Const = localType("Tok").tpe
    val env = TypeEnv.fromDefinitions(
      sourceTypeEnv("""#
struct Tok
struct Pred[a](test: a -> Tok)
enum Expr: Lit, Test(f: Pred[Expr] -> Tok)
""").allDefinedTypes.filter(dt =>
        (dt.packageName == localPackage) && (dt.toTypeConst != tok)
      )
    )
    assertState(Inhabitedness.check(localType("Expr"), env), State.Inhabited)
  }

  test("a component of mutually recursive types has no levels and is Unknown") {
    // Bosatsu rejects mutually recursive types, so only a hand-built env
    // has these. They read each other under one function argument each,
    // so they would be searched, but without levels the search can't nest
    // their fixed points.
    val env = sourceTypeEnv("""#
enum Never: Never(next: Never)
struct MA(f: MB -> Never)
struct MB(g: MA -> Never)
""")
    assertState(Inhabitedness.check(localType("MA"), env), State.Unknown)
    assertState(Inhabitedness.check(localType("MB"), env), State.Unknown)
  }

  test("the search over a component read under function arguments is bounded") {
    // 16 DNLoop4 keys, each reading all 16 through two Negs: the search
    // could follow 16! paths, so it stops at its visit and depth limits.
    val vars = List("a", "b", "c", "d").map(Type.Var.Bound(_))
    val forallDNLoop4 = Type.forAll(
      vars.map((_, Kind.Type)),
      Type.applyAll(localType("DNLoop4"), vars.map(Type.TyVar(_)))
    )
    assertState(Inhabitedness.check(forallDNLoop4, captureEnv), State.Unknown)
  }

  test("polymorphic recursion stops at the data-type key limits") {
    // Nested[Int] reaches Nested[Wrap[Int]], Nested[Wrap[Wrap[Int]]], ...
    assertState(
      Inhabitedness.check(Type.apply1(localType("Nested"), Type.IntType), captureEnv),
      State.Inhabited
    )
    // NestedLoop has no value, but proving that needs every key, so the
    // check runs out of keys and answers Unknown.
    assertState(
      Inhabitedness.check(Type.apply1(localType("NestedLoop"), Type.IntType), captureEnv),
      State.Unknown
    )
    // Twice doubles its argument at each step, so its keys hit the size
    // limit after a few steps, long before the number of keys matters.
    assertState(
      Inhabitedness.check(Type.apply1(localType("Twice"), Type.IntType), captureEnv),
      State.Inhabited
    )
    assertState(
      Inhabitedness.check(Type.apply1(localType("TwiceLoop"), Type.IntType), captureEnv),
      State.Unknown
    )
    // Branch reaches 2^k keys of size k + 2 at depth k, so it runs out of
    // keys long before any key is too large.
    assertState(
      Inhabitedness.check(Type.apply1(localType("Branch"), Type.IntType), captureEnv),
      State.Unknown
    )
  }

  test("a contravariant read of a key outside the component is fine") {
    // SelfWithFn reads Never under a function argument, but Never is solved
    // first, so SelfWithFn's component is still monotone: it is empty.
    assertState(
      Inhabitedness.check(localType("SelfWithFn"), captureEnv),
      State.Uninhabited
    )
  }

  test("a variable with no state keeps its name in a data-type key") {
    // A hand-built field that uses `q` without declaring it.
    val q = Type.Var.Bound("q")
    val env = predefTypeEnv ++ TypeEnv.fromDefinitions(
      mkType(
        "EdgeFreeVarKey",
        List(
          rankn.ConstructorFn[Kind.Arg](
            name = Constructor("EdgeFreeVarKey"),
            args = List(mkParam("x", Type.apply1(Type.OptionType, Type.TyVar(q))))
          )
        )
      ) :: Nil
    )
    // Option[q] has None whatever q is.
    assertState(
      Inhabitedness.check(localType("EdgeFreeVarKey"), env),
      State.Inhabited
    )
  }

  private def genInhabited(env: TypeEnv[Kind.Arg]): Gen[Type] = {
    val optionNever = Type.apply1(Type.OptionType, neverType)
    val envSpecific = Vector(
      typeIfPresent(env, mixedType).map(t => Gen.const(t: Type)),
      typeIfPresent(env, Type.BoolType).map(t => Gen.const(t: Type))
    ).flatten

    val base = Vector[Gen[Type]](
      Gen.const(Type.IntType),
      Gen.const(Type.UnitType),
      Gen.const(Type.StrType),
      Gen.const(Type.BoolType),
      Gen.const(optionNever),
      Gen.const(forallA_AtoA)
    ) ++ envSpecific

    def loopInh(depth: Int): Gen[Type] =
      if (depth <= 0) Gen.oneOf(base).flatMap(identity)
      else {
        val next = depth - 1
        val fnResInh =
          for {
            arg <- Gen.lzy(loopInh(next))
            res <- Gen.lzy(loopInh(next))
          } yield Type.Fun(arg, res)
        val vacuousFn =
          for {
            arg <- Gen.lzy(loopUninh(next))
            res <- Gen.lzy(loopUninh(next))
          } yield Type.Fun(arg, res)

        Gen.frequency(
          (5, Gen.oneOf(base).flatMap(identity)),
          (3, fnResInh),
          (2, vacuousFn)
        )
      }

    def loopUninh(depth: Int): Gen[Type] =
      if (depth <= 0) Gen.oneOf(Gen.const(neverType), Gen.const(forallA_A))
      else {
        val next = depth - 1
        val fnUninh =
          for {
            arg <- Gen.lzy(loopInh(next))
            res <- Gen.lzy(loopUninh(next))
          } yield Type.Fun(arg, res)

        Gen.frequency(
          (4, Gen.const(neverType)),
          (4, Gen.const(forallA_A)),
          (2, fnUninh)
        )
      }

    Gen.sized { sz =>
      val depth = (sz % 4) + 1
      loopInh(depth)
    }
  }

  private def genUninhabited(env: TypeEnv[Kind.Arg]): Gen[Type] = {
    val neverSeed: Gen[Type] =
      typeIfPresent(env, neverType) match {
        case Some(t) => Gen.const(t: Type)
        case None    => Gen.const(forallA_A)
      }

    def loopInh(depth: Int): Gen[Type] =
      if (depth <= 0) {
        val baseInh = Vector(
          Gen.const(Type.IntType),
          Gen.const(Type.UnitType),
          Gen.const(Type.StrType)
        ) ++ typeIfPresent(env, mixedType).map(t => Gen.const(t: Type))
        Gen.oneOf(baseInh).flatMap(identity)
      } else {
        val next = depth - 1
        Gen.frequency(
          (4, loopInh(0)),
          (
            2,
            for {
              arg <- Gen.lzy(loopInh(next))
              res <- Gen.lzy(loopInh(next))
            } yield Type.Fun(arg, res)
          ),
          (
            2,
            for {
              arg <- Gen.lzy(loopUninh(next))
              res <- Gen.lzy(loopUninh(next))
            } yield Type.Fun(arg, res)
          )
        )
      }

    def loopUninh(depth: Int): Gen[Type] =
      if (depth <= 0) Gen.oneOf(neverSeed, Gen.const(forallA_A))
      else {
        val next = depth - 1
        val fnUninh =
          for {
            arg <- Gen.lzy(loopInh(next))
            res <- Gen.lzy(loopUninh(next))
          } yield Type.Fun(arg, res)
        Gen.frequency(
          (4, neverSeed),
          (4, Gen.const(forallA_A)),
          (2, fnUninh)
        )
      }

    Gen.sized { sz =>
      val depth = (sz % 4) + 1
      loopUninh(depth)
    }
  }

  property("genInhabited produces inhabited types") {
    forAll(genInhabited(nonTrivialEnv)) { tpe =>
      assertState(Inhabitedness.check(tpe, nonTrivialEnv), State.Inhabited)
    }
  }

  property("genUninhabited produces uninhabited types") {
    forAll(genUninhabited(nonTrivialEnv)) { tpe =>
      assertState(Inhabitedness.check(tpe, nonTrivialEnv), State.Uninhabited)
    }
  }

  property("wildcard reachability agrees with inhabitedness") {
    val pat = Pattern.WildCard
    val both =
      Gen.oneOf(
        genInhabited(nonTrivialEnv).map((_, State.Inhabited)),
        genUninhabited(nonTrivialEnv).map((_, State.Uninhabited))
      )

    forAll(both) { case (tpe, expected) =>
      assertState(Inhabitedness.checkMatch(tpe, pat, nonTrivialEnv), expected)
    }
  }
}
