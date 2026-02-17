package dev.bosatsu

import cats.Functor
import cats.data.{Ior, NonEmptyList}
import cats.syntax.all._
import dev.bosatsu.rankn.{ParsedTypeEnv, Type}
import org.scalacheck.Gen
import MonadGen.genMonad

/** Typed-by-construction source generator used by WellTypedTests.
  *
  * The generator is intentionally incremental:
  *   - phase 1: bind-only statements
  *   - phase 2: defs + local type definitions
  *   - phase 3: typed match generation
  *   - phase 4: backend-ready profile (same surface, stricter sizing profile)
  */
object WellTypedGen {
  sealed trait Phase extends Product with Serializable derives CanEqual
  object Phase {
    case object Phase1 extends Phase
    case object Phase2 extends Phase
    case object Phase3 extends Phase
    case object Phase4 extends Phase
  }

  final case class Config(
      maxStatements: Int,
      maxExprDepth: Int,
      maxTypeDepth: Int,
      allowDefs: Boolean,
      allowTypeDefs: Boolean,
      allowMatch: Boolean,
      allowGuards: Boolean
  )

  object Config {
    val phase1: Config =
      Config(
        maxStatements = 6,
        maxExprDepth = 4,
        maxTypeDepth = 3,
        allowDefs = false,
        allowTypeDefs = false,
        allowMatch = false,
        allowGuards = false
      )

    val phase2: Config =
      phase1.copy(
        allowDefs = true,
        allowTypeDefs = true,
        maxStatements = 8
      )

    val phase3: Config =
      phase2.copy(
        allowMatch = true,
        allowGuards = true,
        maxExprDepth = 5
      )

    // Same syntax surface as phase3, but a tighter profile for downstream use.
    val phase4: Config =
      phase3.copy(
        maxStatements = 5,
        maxExprDepth = 3,
        maxTypeDepth = 2
      )

    def forPhase(phase: Phase): Config =
      phase match {
        case Phase.Phase1 => phase1
        case Phase.Phase2 => phase2
        case Phase.Phase3 => phase3
        case Phase.Phase4 => phase4
      }
  }

  final case class WellTypedProgram(
      packageName: PackageName,
      statements: List[Statement],
      expected: Map[Identifier.Bindable, Type],
      typeEnv: rankn.TypeEnv[Kind.Arg]
  )

  final case class ValSig(name: Identifier, sigma: Type)
  final case class TypeCtorSig(tpe: Type.TyConst, kind: Kind)
  final case class CtorSig(
      pack: PackageName,
      cons: Identifier.Constructor,
      args: List[Type],
      result: Type.TyConst
  )
  final case class Ctx(
      packageName: PackageName,
      vals: Vector[ValSig],
      ctors: Vector[CtorSig],
      localTypeCtors: Vector[TypeCtorSig],
      usedValueNames: Set[Identifier.Bindable],
      usedConstructors: Set[Identifier.Constructor],
      nextValueId: Int,
      nextTypeId: Int
  )

  private final case class Build(
      ctx: Ctx,
      statementsRev: List[Statement],
      expected: Map[Identifier.Bindable, Type]
  )

  private final case class Step(
      statement: Statement,
      next: Ctx,
      expected: Option[(Identifier.Bindable, Type)]
  )

  private val emptyRegion: Region = Region(0, 0)
  private given Region = emptyRegion

  private val predefTrue = Identifier.Constructor("True")
  private val predefFalse = Identifier.Constructor("False")
  private val quantVarPool: Vector[String] = ('a' to 'h').map(_.toString).toVector

  private val baseTypes: Vector[Type.TyConst] =
    Vector(
      Type.IntType,
      Type.StrType,
      Type.CharType,
      Type.Float64Type,
      Type.UnitType,
      Type.BoolType
    )

  private def boolCtor(
      cons: Identifier.Constructor
  ): CtorSig =
    CtorSig(PackageName.PredefName, cons, Nil, Type.BoolType)

  private def ctorType(c: CtorSig): Type =
    NonEmptyList.fromList(c.args) match {
      case None      => c.result
      case Some(args) => Type.Fun(args, c.result)
    }

  private def constructorPattern(cons: Identifier.Constructor): Pattern.Parsed =
    Pattern.PositionalStruct(
      Pattern.StructKind.Named(cons, Pattern.StructKind.Style.TupleLike),
      Nil
    )

  private def initialCtx(pn: PackageName): Ctx = {
    val boolCtors = Vector(boolCtor(predefTrue), boolCtor(predefFalse))
    val ctorVals = boolCtors.map(c => ValSig(c.cons, ctorType(c)))
    Ctx(
      packageName = pn,
      vals = ctorVals,
      ctors = boolCtors,
      localTypeCtors = Vector.empty,
      usedValueNames = Set.empty,
      usedConstructors = Set(predefTrue, predefFalse),
      nextValueId = 0,
      nextTypeId = 0
    )
  }

  private def combineTypeEnv(
      imported: rankn.TypeEnv[Kind.Arg],
      parsed: ParsedTypeEnv[Option[Kind.Arg]]
  ): rankn.TypeEnv[Kind.Arg] = {
    val defs = parsed.allDefinedTypes.map(
      Functor[rankn.DefinedType].map(_) { opt =>
        opt.getOrElse(Kind.invariantTypeArg)
      }
    )
    val fromDefs = rankn.TypeEnv.fromDefinitions(defs)
    val withExternals =
      parsed.externalDefs.foldLeft(fromDefs) { case (acc, (pn, n, tpe)) =>
        acc.addExternalValue(pn, n, tpe)
      }
    imported ++ withExternals
  }

  private def finalizeTypeEnv(
      packageName: PackageName,
      statements: List[Statement]
  ): rankn.TypeEnv[Kind.Arg] =
    SourceConverter.toProgram(packageName, Nil, statements) match {
      case Ior.Left(_) =>
        rankn.TypeEnv.empty
      case Ior.Right(program) =>
        combineTypeEnv(program.types._1, program.types._2)
      case Ior.Both(_, program) =>
        combineTypeEnv(program.types._1, program.types._2)
    }

  private def isFunctionType(tpe: Type): Boolean =
    Type.Fun.unapply(tpe).isDefined

  @annotation.tailrec
  private def dropForAll(tpe: Type): Type =
    tpe match {
      case Type.ForAll(_, in) => dropForAll(in)
      case other              => other
    }

  private def freshValueName(
      ctx: Ctx,
      prefix: String = "wtv"
  ): (Identifier.Name, Ctx) = {
    @annotation.tailrec
    def loop(i: Int): (Identifier.Name, Int) = {
      val candidate = Identifier.Name(s"${prefix}_$i")
      if (ctx.usedValueNames(candidate)) loop(i + 1)
      else (candidate, i)
    }

    val (name, idx) = loop(ctx.nextValueId)
    val nextCtx = ctx.copy(
      usedValueNames = ctx.usedValueNames + name,
      nextValueId = idx + 1
    )
    (name, nextCtx)
  }

  private def freshConstructor(
      ctx: Ctx,
      prefix: String
  ): (Identifier.Constructor, Ctx) = {
    @annotation.tailrec
    def loop(i: Int): (Identifier.Constructor, Int) = {
      val candidate = Identifier.Constructor(s"${prefix}$i")
      if (ctx.usedConstructors(candidate)) loop(i + 1)
      else (candidate, i)
    }

    val (cons, idx) = loop(ctx.nextTypeId)
    val nextCtx = ctx.copy(
      usedConstructors = ctx.usedConstructors + cons,
      nextTypeId = idx + 1
    )
    (cons, nextCtx)
  }

  private def typeRefOf(tpe: Type): TypeRef =
    TypeRefConverter
      .fromTypeA[Option](
        tpe,
        _ => None,
        _ => None,
        defined => Some(TypeRef.TypeName(defined.name))
      )
      .getOrElse(
        throw new IllegalArgumentException(
          s"unsupported TypeRef conversion: $tpe"
        )
      )

  private final case class KindedTypeVar(tpe: Type.TyVar, kind: Kind)

  private def dedupeTypeCtors(items: Vector[TypeCtorSig]): Vector[TypeCtorSig] =
    items
      .foldLeft((Set.empty[Type.TyConst], Vector.empty[TypeCtorSig])) {
        case ((seen, acc), item) =>
          if (seen(item.tpe)) (seen, acc)
          else (seen + item.tpe, acc :+ item)
      }
      ._2

  private val builtInTypeCtors: Vector[TypeCtorSig] = {
    val scalars = baseTypes.map(TypeCtorSig(_, Kind.Type))
    val containers = Vector(
      TypeCtorSig(Type.OptionType, Kind(Kind.Type.co)),
      TypeCtorSig(Type.ListType, Kind(Kind.Type.co)),
      TypeCtorSig(Type.DictType, Kind(Kind.Type.in, Kind.Type.co))
    )
    val fnKinds = Type.FnType.FnKinds.iterator.take(2).map { case (tpe, kind) =>
      TypeCtorSig(tpe, kind)
    }.toVector
    val tupleKinds = Type.Tuple.Kinds.iterator.take(3).map { case (tpe, kind) =>
      TypeCtorSig(tpe, kind)
    }.toVector
    dedupeTypeCtors(scalars ++ containers ++ fnKinds ++ tupleKinds)
  }

  private def allTypeCtors(ctx: Ctx): Vector[TypeCtorSig] =
    dedupeTypeCtors(builtInTypeCtors ++ ctx.localTypeCtors)

  private def kindsIn(ctx: Ctx): NonEmptyList[Kind] = {
    val allKinds = (Kind.Type +: allTypeCtors(ctx).map(_.kind)).distinct
    NonEmptyList.fromListUnsafe(allKinds.toList)
  }

  private def oneOfVector[A](values: Vector[A]): Option[Gen[A]] =
    NonEmptyList
      .fromList(values.toList)
      .map(nel => Gen.oneOf(nel.toList))

  private def oneOfGenerators[A](values: Vector[Gen[A]]): Option[Gen[A]] =
    oneOfVector(values).map(_.flatMap(identity))

  private def weightedOneOf[A](values: Vector[(Int, Gen[A])]): Option[Gen[A]] =
    NonEmptyList
      .fromList(values.toList)
      .map(nel => Gen.frequency(nel.toList*))

  @annotation.tailrec
  private def requiredArgKinds(
      from: Kind,
      to: Kind,
      accRev: List[Kind] = Nil
  ): Option[List[Kind]] =
    if (from == to) Some(accRev.reverse)
    else {
      from match {
        case Kind.Cons(Kind.Arg(_, argKind), next) =>
          requiredArgKinds(next, to, argKind :: accRev)
        case Kind.Type =>
          None
      }
    }

  private def typeAtoms(
      ctx: Ctx,
      localVars: Vector[KindedTypeVar]
  ): Vector[(Type, Kind)] = {
    val varAtoms = localVars.map(v => ((v.tpe: Type), v.kind))
    val constAtoms = allTypeCtors(ctx).map(c => ((c.tpe: Type), c.kind))
    (varAtoms ++ constAtoms).distinct
  }

  private def genTypeFromVarsOfKind(
      kind: Kind,
      ctx: Ctx,
      depth: Int,
      localVars: Vector[KindedTypeVar]
  ): Option[Gen[Type]] = {
    val candidates = localVars.flatMap { v =>
      requiredArgKinds(v.kind, kind).flatMap { needed =>
        if (needed.isEmpty) Some(Gen.const(v.tpe))
        else if (depth <= 0) None
        else {
          needed
            .traverse(k => genTypeOfKind(k, ctx, depth - 1, localVars))
            .map { argGens =>
              argGens.map(Gen.lzy(_)).sequence.map(args =>
                Type.applyAll(v.tpe, args)
              )
            }
        }
      }
    }
    oneOfGenerators(candidates)
  }

  private def genTypeOfKind(
      kind: Kind,
      ctx: Ctx,
      depth: Int
  ): Option[Gen[Type]] =
    genTypeOfKind(kind, ctx, depth, Vector.empty)

  private def genTypeOfKind(
      kind: Kind,
      ctx: Ctx,
      depth: Int,
      localVars: Vector[KindedTypeVar]
  ): Option[Gen[Type]] = {
    val candidates = typeAtoms(ctx, localVars).flatMap { case (head, headKind) =>
      requiredArgKinds(headKind, kind).flatMap { needed =>
        if (needed.isEmpty) Some(Gen.const(head))
        else if (depth <= 0) None
        else {
          needed
            .traverse(k => genTypeOfKind(k, ctx, depth - 1, localVars))
            .map { argGens =>
              argGens.map(Gen.lzy(_)).sequence.map(args =>
                Type.applyAll(head, args)
              )
            }
        }
      }
    }
    oneOfGenerators(candidates)
  }

  private def genFieldType(ctx: Ctx): Gen[Type.TyConst] = {
    val all = allTypeCtors(ctx).collect { case TypeCtorSig(tc, Kind.Type) => tc }
    oneOfVector(all).getOrElse(Gen.const(Type.IntType))
  }

  private def genGoalType(ctx: Ctx, depth: Int): Gen[Type] = {
    val localMonotypes = ctx.localTypeCtors.collect {
      case TypeCtorSig(tc, Kind.Type) => tc
    }
    val atoms = (baseTypes ++ localMonotypes).distinct
    val atomGen = oneOfVector(atoms).getOrElse(Gen.const(Type.IntType))

    if (depth <= 0) atomGen
    else {
      val fnType =
        for {
          arity <- Gen.choose(1, 2)
          args <- Gen.listOfN(arity, Gen.lzy(genGoalType(ctx, depth - 1)))
          res <- Gen.lzy(genGoalType(ctx, depth - 1))
          nel = NonEmptyList.fromListUnsafe(args)
        } yield Type.Fun(nel, res)

      Gen.frequency((7, atomGen), (3, fnType))
    }
  }

  // This is intentionally richer than `genGoalType`: it includes applied and
  // quantified types for source-level type syntax coverage.
  private def genType(ctx: Ctx, depth: Int): Gen[Type] = {
    val mono = genTypeOfKind(Kind.Type, ctx, depth).getOrElse(Gen.const(Type.IntType))
    if (depth <= 0) mono
    else {
      val kindGen =
        oneOfVector(kindsIn(ctx).toList.toVector).getOrElse(Gen.const(Kind.Type))
      val quantified =
        for {
          cnt <- Gen.choose(1, 2)
          names <- Gen.pick(cnt, quantVarPool).map(_.toList.distinct.sorted)
          tailKinds <- Gen.listOfN((names.length - 1).max(0), kindGen)
          bounds = NonEmptyList.fromListUnsafe(
            names.zip(Kind.Type :: tailKinds).map { case (n, k) =>
              (Type.Var.Bound(n), k)
            }
          )
          localVars = bounds.toList.map { case (b, k) =>
            KindedTypeVar(Type.TyVar(b), k)
          }.toVector
          inner <- genTypeFromVarsOfKind(Kind.Type, ctx, depth - 1, localVars)
            .orElse(genTypeOfKind(Kind.Type, ctx, depth - 1, localVars))
            .getOrElse(Gen.const(Type.IntType))
        } yield Type.forAll(bounds, inner)

      Gen.frequency((6, mono), (2, quantified))
    }
  }

  private def genTypeWithVars(
      ctx: Ctx,
      depth: Int,
      vars: Vector[KindedTypeVar]
  ): Option[Gen[Type]] =
    genTypeOfKind(Kind.Type, ctx, depth, vars)

  private def genLiteralFor(goal: Type.TyConst): Option[Gen[Declaration.NonBinding]] =
    if (goal.sameAs(Type.IntType)) {
      Some(Gen.choose(-20, 20).map(n => Declaration.Literal(Lit.Integer(n.toLong))))
    } else if (goal.sameAs(Type.StrType)) {
      Some(Gen.alphaStr.map(s => Declaration.Literal(Lit.Str(s.take(8)))))
    } else if (goal.sameAs(Type.CharType)) {
      Some(
        Gen
          .oneOf(('a' to 'z').toVector)
          .map(ch => Declaration.Literal(Lit.Chr.fromCodePoint(ch.toInt)))
      )
    } else if (goal.sameAs(Type.Float64Type)) {
      Some(
        Gen
          .chooseNum(-10.0, 10.0)
          .map(d => Declaration.Literal(Lit.Float64.fromDouble(d)))
      )
    } else if (goal.sameAs(Type.UnitType)) {
      Some(Gen.const(Declaration.TupleCons(Nil)))
    } else if (goal.sameAs(Type.BoolType)) {
      Some(
        Gen.oneOf(
          Declaration.Var(predefTrue),
          Declaration.Var(predefFalse)
        )
      )
    } else {
      None
    }

  private def genCtorIntro(
      ctx: Ctx,
      goal: Type.TyConst,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] = {
    val candidates = ctx.ctors.filter(_.result.sameAs(goal)).toVector
    val ctorExprs = candidates.flatMap { ctor =>
      ctor.args
        .traverse(arg => genExpr(ctx, arg, depth - 1, cfg))
        .map { argExprGens =>
          argExprGens.map(Gen.lzy(_)).sequence.map { args =>
            val fn = Declaration.Var(ctor.cons)
            NonEmptyList.fromList(args) match {
              case None      => fn
              case Some(nel) =>
                Declaration.Apply(fn, nel, Declaration.ApplyKind.Parens)
            }
          }
        }
    }
    oneOfGenerators(ctorExprs)
  }

  private def genLambda(
      ctx: Ctx,
      args: NonEmptyList[Type],
      res: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] = {
    val params: List[(Identifier.Name, Type)] =
      args.toList.zipWithIndex.map { case (tpe, idx) =>
        (Identifier.Name(s"arg_${idx}_d${depth.max(0)}"), tpe)
      }
    val argPatterns = NonEmptyList.fromListUnsafe(params.map { case (name, _) =>
      Pattern.Var(name)
    })
    val withArgs = ctx.copy(vals =
      ctx.vals ++ params.map { case (name, tpe) => ValSig(name, tpe) }
    )

    genExpr(withArgs, res, depth - 1, cfg).map { bodyGen =>
      Gen.lzy(bodyGen).map(body => Declaration.Lambda(argPatterns, body))
    }
  }

  private def genBoolMatch(
      ctx: Ctx,
      goal: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] =
    for {
      scrutineeGen <- genExpr(ctx, Type.BoolType, depth - 1, cfg)
      onTrueGen <- genExpr(ctx, goal, depth - 1, cfg)
      onFalseGen <- genExpr(ctx, goal, depth - 1, cfg)
    } yield {
      for {
        scrutinee <- Gen.lzy(scrutineeGen)
        onTrue <- Gen.lzy(onTrueGen)
        onFalse <- Gen.lzy(onFalseGen)
        useGuard <- Gen.oneOf(true, false)
        guard <- {
          val guardExpr =
            if (useGuard && cfg.allowGuards)
              genExpr(ctx, Type.BoolType, depth - 1, cfg.copy(allowMatch = false))
            else None
          guardExpr match {
            case Some(g) => Gen.lzy(g).map(Some(_))
            case None    => Gen.const(None)
          }
        }
        case1 = Declaration.MatchBranch(
          constructorPattern(predefTrue),
          guard,
          OptIndent.SameLine(onTrue)
        )
        case2 = Declaration.MatchBranch(
          guard.fold(constructorPattern(predefFalse))(_ => Pattern.WildCard),
          None,
          OptIndent.SameLine(onFalse)
        )
        cases = NonEmptyList(case1, case2 :: Nil)
      } yield Declaration.Match(
        RecursionKind.NonRecursive,
        scrutinee,
        OptIndent.SameLine(cases)
      )
    }

  private def genIntro(
      ctx: Ctx,
      goal: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] =
    goal match {
      case Type.Fun(args, res) =>
        genLambda(ctx, args, res, depth, cfg)
      case tc: Type.TyConst =>
        genLiteralFor(tc).orElse(genCtorIntro(ctx, tc, depth, cfg))
      case _ =>
        val exact = ctx.vals.filter(v => v.sigma.sameAs(goal)).toVector
        oneOfVector(exact).map(_.map(v => Declaration.Var(v.name)))
    }

  private def genExpr(
      ctx: Ctx,
      goal: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] = {
    val intro = genIntro(ctx, goal, depth, cfg)
    val byVar = oneOfVector(ctx.vals.filter(_.sigma.sameAs(goal)).toVector)
      .map(_.map(v => Declaration.Var(v.name)))
    val byApplyCandidates = ctx.vals.collect {
      case v @ ValSig(_, Type.Fun(args, res)) if res.sameAs(goal) =>
        (v, args)
    }.toVector.flatMap { case (fn, args) =>
      args.toList
        .traverse(t => genExpr(ctx, t, depth - 1, cfg))
        .map { argExprGens =>
          argExprGens.map(Gen.lzy(_)).sequence.map(argv =>
            Declaration.Apply(
              Declaration.Var(fn.name),
              NonEmptyList.fromListUnsafe(argv),
              Declaration.ApplyKind.Parens
            )
          )
        }
    }
    val byApply = oneOfGenerators(byApplyCandidates)
    val byMatch =
      if (cfg.allowMatch && !isFunctionType(goal) && depth > 1)
        genBoolMatch(ctx, goal, depth - 1, cfg)
      else None

    if (depth <= 0) {
      oneOfGenerators(intro.toVector ++ byVar.toVector)
    } else {
      val choices = Vector.newBuilder[(Int, Gen[Declaration.NonBinding])]
      intro.foreach(g => choices += ((4, Gen.lzy(g))))
      byVar.foreach(g => choices += ((2, g)))
      byApply.foreach(g => choices += ((2, g)))
      byMatch.foreach(g => choices += ((1, g)))
      weightedOneOf(choices.result())
    }
  }

  private def requireGen[A](label: String, gen: Option[Gen[A]]): Gen[A] =
    gen.getOrElse {
      throw new IllegalStateException(s"expected generator for: $label")
    }

  private def genBindStep(ctx: Ctx, cfg: Config): Gen[Step] = {
    val (name, ctx1) = freshValueName(ctx)
    for {
      goal <- genGoalType(ctx1, cfg.maxTypeDepth)
      expr <- Gen.lzy(
        requireGen(
          s"bind expression for goal: $goal",
          genExpr(ctx1, goal, cfg.maxExprDepth, cfg)
        )
      )
      stmt = Statement.Bind(
        BindingStatement(Pattern.Var(name), expr, ())
      )(emptyRegion)
      next = ctx1.copy(vals = ctx1.vals :+ ValSig(name, goal))
    } yield Step(stmt, next, Some((name, goal)))
  }

  private def genDefStep(ctx: Ctx, cfg: Config): Gen[Step] = {
    val (name, ctx1) = freshValueName(ctx, "wtf")
    for {
      argc <- Gen.choose(1, 2)
      args <- Gen.listOfN(argc, genGoalType(ctx1, (cfg.maxTypeDepth - 1).max(0)))
      result <- genGoalType(ctx1, cfg.maxTypeDepth)
      argNames = args.zipWithIndex.map { case (_, idx) =>
        Identifier.Name(s"${name.asString}_arg$idx")
      }
      argPatterns = NonEmptyList.fromListUnsafe(argNames.map(Pattern.Var(_)))
      localCtx = ctx1.copy(vals =
        ctx1.vals ++ argNames.zip(args).map { case (n, tpe) =>
          ValSig(n, tpe)
        }
      )
      body <- Gen.lzy(
        requireGen(
          s"def body for result type: $result",
          genExpr(localCtx, result, cfg.maxExprDepth, cfg)
        )
      )
      stmt = Statement.Def(
        DefStatement(
          name = name,
          typeArgs = None,
          args = NonEmptyList.one(argPatterns),
          retType = None,
          result = OptIndent.SameLine(body)
        )
      )(emptyRegion)
      fnType = Type.Fun(NonEmptyList.fromListUnsafe(args), result)
      next = ctx1.copy(vals = ctx1.vals :+ ValSig(name, fnType))
    } yield Step(stmt, next, Some((name, fnType)))
  }

  private def genExternalDefStep(ctx: Ctx, cfg: Config): Gen[Step] = {
    val (name, ctx1) = freshValueName(ctx, "wte")
    for {
      kindGen <- oneOfVector(kindsIn(ctx1).toList.toVector).getOrElse(Gen.const(Kind.Type))
      varCount <- Gen.choose(1, 2)
      rawNames <- Gen.pick(varCount, quantVarPool).map(_.toList.distinct.sorted)
      tailKinds <- Gen.listOfN((rawNames.length - 1).max(0), kindGen)
      bounds = NonEmptyList.fromListUnsafe(
        rawNames.zip(Kind.Type :: tailKinds).map { case (n, k) =>
          (Type.Var.Bound(n), k)
        }
      )
      tvs = bounds.toList.map { case (b, k) =>
        KindedTypeVar(Type.TyVar(b), k)
      }.toVector
      argTypeGen = genTypeFromVarsOfKind(Kind.Type, ctx1, cfg.maxTypeDepth, tvs)
        .orElse(genTypeWithVars(ctx1, cfg.maxTypeDepth, tvs))
        .getOrElse(Gen.const(Type.IntType))
      firstArg <- Gen.lzy(argTypeGen)
      extraArgCount <- Gen.choose(0, 2)
      extraArgs <- Gen.listOfN(extraArgCount, Gen.lzy(argTypeGen))
      args = firstArg :: extraArgs
      resultBaseGen = genTypeFromVarsOfKind(Kind.Type, ctx1, cfg.maxTypeDepth, tvs)
        .orElse(genTypeWithVars(ctx1, cfg.maxTypeDepth, tvs))
        .getOrElse(Gen.const(firstArg))
      resultBase <- Gen.oneOf(
        Gen.const(firstArg),
        Gen.lzy(resultBaseGen)
      )
      extraSourceType <- genType(ctx1, cfg.maxTypeDepth).map(dropForAll)
      result <- Gen.oneOf(
        Gen.const(resultBase),
        Gen.const(Type.TyApply(Type.OptionType, resultBase)),
        Gen.const(Type.TyApply(Type.ListType, resultBase)),
        Gen.const(extraSourceType)
      )
      argRefs = args.zipWithIndex.map { case (tpe, idx) =>
        (Identifier.Name(s"arg$idx"): Identifier.Bindable, typeRefOf(tpe))
      }
      typeArgsRef = Some(bounds.map { case (b, k) =>
        (TypeRef.TypeVar(b.name), if (k.isType) None else Some(k))
      })
      stmt = Statement.ExternalDef(
        name = name,
        typeArgs = typeArgsRef,
        params = argRefs,
        result = typeRefOf(result)
      )(emptyRegion)
      sigma = Type.forAll(bounds, Type.Fun(NonEmptyList.fromListUnsafe(args), result))
      next = ctx1.copy(vals = ctx1.vals :+ ValSig(name, sigma))
    } yield Step(stmt, next, None)
  }

  private def genStructStep(ctx: Ctx): Gen[Step] = {
    val (cons, ctx1) = freshConstructor(ctx, "WtStruct")
    val resultType = Type.TyConst(Type.Const.Defined(ctx.packageName, TypeName(cons)))
    for {
      argc <- Gen.choose(0, 2)
      args <- Gen.listOfN(argc, genFieldType(ctx1))
      stmtArgs: List[(Identifier.Bindable, Option[TypeRef])] =
        args.zipWithIndex.map {
          case (tpe, idx) =>
            (Identifier.Name(s"f$idx"): Identifier.Bindable, Option(typeRefOf(tpe)))
        }
      stmt = Statement.Struct(cons, None, stmtArgs)(emptyRegion)
      ctor = CtorSig(ctx.packageName, cons, args, resultType)
      next = ctx1.copy(
        ctors = ctx1.ctors :+ ctor,
        vals = ctx1.vals :+ ValSig(cons, ctorType(ctor)),
        localTypeCtors =
          dedupeTypeCtors(ctx1.localTypeCtors :+ TypeCtorSig(resultType, Kind.Type))
      )
    } yield Step(stmt, next, None)
  }

  private def allocateConstructors(
      ctx: Ctx,
      count: Int,
      prefix: String
  ): (List[Identifier.Constructor], Ctx) = {
    @annotation.tailrec
    def loop(
        n: Int,
        acc: List[Identifier.Constructor],
        curr: Ctx
    ): (List[Identifier.Constructor], Ctx) =
      if (n <= 0) (acc.reverse, curr)
      else {
        val (cons, next) = freshConstructor(curr, prefix)
        loop(n - 1, cons :: acc, next)
      }

    loop(count, Nil, ctx)
  }

  private def genEnumStep(ctx: Ctx): Gen[Step] = {
    val (enumName, ctx1) = freshConstructor(ctx, "WtEnum")
    val resultType = Type.TyConst(Type.Const.Defined(ctx.packageName, TypeName(enumName)))
    for {
      branchCount <- Gen.choose(2, 3)
      allocated = allocateConstructors(ctx1, branchCount, s"${enumName.asString}C")
      (constructors, ctx2) = allocated
      branchArgs <- constructors.traverse { _ =>
        for {
          argc <- Gen.choose(0, 1)
          args <- Gen.listOfN(argc, genFieldType(ctx2))
        } yield args
      }
      branches = constructors.zip(branchArgs).map { case (cons, args) =>
        Statement.EnumBranch(
          name = cons,
          typeArgs = None,
          args = args.zipWithIndex.map { case (tpe, idx) =>
            (Identifier.Name(s"a$idx"): Identifier.Bindable, Option(typeRefOf(tpe)))
          },
          region = emptyRegion
        )
      }
      stmt = Statement.Enum(
        name = enumName,
        typeArgs = None,
        items = OptIndent.SameLine(NonEmptyList.fromListUnsafe(branches))
      )(emptyRegion)
      sigs = constructors.zip(branchArgs).map { case (cons, args) =>
        CtorSig(ctx.packageName, cons, args, resultType)
      }
      next = ctx2.copy(
        ctors = ctx2.ctors ++ sigs,
        vals = ctx2.vals ++ sigs.map(s => ValSig(s.cons, ctorType(s))),
        localTypeCtors =
          dedupeTypeCtors(ctx2.localTypeCtors :+ TypeCtorSig(resultType, Kind.Type))
      )
    } yield Step(stmt, next, None)
  }

  private def genTypeDefStep(ctx: Ctx): Gen[Step] =
    Gen.oneOf(genStructStep(ctx), genEnumStep(ctx))

  private def genNextStep(
      ctx: Ctx,
      cfg: Config,
      forceValue: Boolean
  ): Gen[Step] =
    if (forceValue) genBindStep(ctx, cfg)
    else {
      val steps = List.newBuilder[(Int, Gen[Step])]
      steps += ((6, genBindStep(ctx, cfg)))
      if (cfg.allowDefs) {
        steps += ((2, genDefStep(ctx, cfg)))
        steps += ((2, genExternalDefStep(ctx, cfg)))
      }
      if (cfg.allowTypeDefs) {
        steps += ((2, genTypeDefStep(ctx)))
      }
      Gen.frequency(steps.result()*)
    }

  private def genProgramState(
      cfg: Config,
      statementCount: Int,
      init: Build
  ): Gen[Build] =
    if (statementCount <= 0) Gen.const(init)
    else {
      val forceValue = init.expected.isEmpty
      val forceQuantified =
        cfg.allowDefs && init.statementsRev.isEmpty
      val nextStepGen =
        if (forceQuantified) genExternalDefStep(init.ctx, cfg)
        else genNextStep(init.ctx, cfg, forceValue)

      nextStepGen.flatMap { step =>
        val nextExpected = step.expected match {
          case None         => init.expected
          case Some((n, t)) => init.expected.updated(n, t)
        }
        val nextBuild = Build(
          ctx = step.next,
          statementsRev = step.statement :: init.statementsRev,
          expected = nextExpected
        )
        genProgramState(cfg, statementCount - 1, nextBuild)
      }
    }

  def wellTypedProgramGen(cfg: Config): Gen[WellTypedProgram] =
    for {
      suffix <- Gen.choose(0, Int.MaxValue)
      packageName = PackageName.parts("WellTypedGenerated", s"P$suffix")
      minStatements = if (cfg.allowDefs) 2 else 1
      statementCount <- Gen.choose(
        minStatements,
        cfg.maxStatements.max(minStatements)
      )
      init = Build(
        ctx = initialCtx(packageName),
        statementsRev = Nil,
        expected = Map.empty
      )
      built <- genProgramState(cfg, statementCount, init)
      statements = built.statementsRev.reverse
      typeEnv = finalizeTypeEnv(packageName, statements)
    } yield WellTypedProgram(
      packageName = packageName,
      statements = statements,
      expected = built.expected,
      typeEnv = typeEnv
    )

  def wellTypedProgramGen(phase: Phase): Gen[WellTypedProgram] =
    wellTypedProgramGen(Config.forPhase(phase))
}
