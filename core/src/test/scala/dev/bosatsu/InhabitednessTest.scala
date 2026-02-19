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
    assertState(Inhabitedness.check(listUnknownForAll, edgeNoPredefEnv), State.Unknown)
    assertState(Inhabitedness.check(listUnknownExists, edgeNoPredefEnv), State.Unknown)

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
        edgeNoPredefEnv
      ),
      State.Unknown
    )
    assertState(
      Inhabitedness.checkMatch(
        Type.apply1(Type.ListType, Type.IntType),
        listRight,
        edgeFakeListEnv
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
