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
      typeParams: List[(Type.Var.Bound, Kind)],
      args: List[Type],
      result: Type
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
    CtorSig(PackageName.PredefName, cons, Nil, Nil, Type.BoolType)

  private def ctorType(c: CtorSig): Type =
    val res = NonEmptyList.fromList(c.args) match {
      case None      => c.result
      case Some(args) => Type.Fun(args, c.result)
    }
    NonEmptyList.fromList(c.typeParams) match {
      case None         => res
      case Some(bounds) => Type.forAll(bounds, res)
    }

  private def initialCtx(pn: PackageName): Ctx = {
    val boolCtors = Vector(boolCtor(predefTrue), boolCtor(predefFalse))
    val ctorVals = boolCtors.map(c => ValSig(c.cons, ctorType(c)))
    Ctx(
      packageName = pn,
      vals = ctorVals,
      // Keep bool values available for expression generation, but avoid exposing
      // predef constructors as local match families in source conversion.
      ctors = Vector.empty,
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

  private inline def requireSome[A](inline label: => String, opt: Option[A]): A =
    opt match {
      case Some(a) => a
      case None => 
        throw new IllegalStateException(s"expected value for: $label")
    }

  private def typeParamKindRef(kind: Kind): Option[Kind.Arg] =
    if (kind == Kind.Type) None else Some(Kind.Arg(Variance.in, kind))

  private def typeParamRefs(
      params: List[(Type.Var.Bound, Kind)]
  ): Option[NonEmptyList[(TypeRef.TypeVar, Option[Kind.Arg])]] =
    NonEmptyList.fromList(
      params.map { case (b, k) =>
        (TypeRef.TypeVar(b.name), typeParamKindRef(k))
      }
    )

  private def kindFromParams(params: List[(Type.Var.Bound, Kind)]): Kind =
    params.foldRight(Kind.Type: Kind) { case ((_, k), acc) =>
      Kind.Cons(Kind.Arg(Variance.in, k), acc)
    }

  private def resultTypeFor(
      root: Type.TyConst,
      typeParams: List[(Type.Var.Bound, Kind)]
  ): Type =
    Type.applyAll(root, typeParams.map { case (b, _) => (Type.TyVar(b): Type) })

  private def mapTemplateVars(
      vars: Set[Type.Var.Bound],
      template: Type,
      actual: Type
  ): Option[Map[Type.Var.Bound, Type]] = {
    def loop(
        t: Type,
        a: Type,
        acc: Map[Type.Var.Bound, Type]
    ): Option[Map[Type.Var.Bound, Type]] =
      (t, a) match {
        case (Type.TyVar(b: Type.Var.Bound), _) if vars(b) =>
          acc.get(b) match {
            case None          => Some(acc.updated(b, a))
            case Some(prev)    =>
              if (prev.sameAs(a)) Some(acc)
              else None
          }
        case (Type.TyApply(tOn, tArg), Type.TyApply(aOn, aArg)) =>
          loop(tOn, aOn, acc).flatMap(loop(tArg, aArg, _))
        case _ =>
          if (t.sameAs(a)) Some(acc)
          else None
      }

    loop(template, actual, Map.empty)
  }

  @annotation.tailrec
  private def unfoldForAll(
      t: Type,
      accRev: List[(Type.Var.Bound, Kind)] = Nil
  ): (List[(Type.Var.Bound, Kind)], Type) =
    t match {
      case Type.ForAll(vs, in) =>
        unfoldForAll(in, vs.toList.reverse_:::(accRev))
      case other =>
        (accRev.reverse, other)
    }

  private def instantiatedFnArgsForGoal(
      sigma: Type,
      goal: Type
  ): Option[List[Type]] = {
    val (bounds, body) = unfoldForAll(sigma)
    body match {
      case Type.Fun(args, res) =>
        if (bounds.isEmpty) {
          if (res.sameAs(goal)) Some(args.toList) else None
        } else {
          val vars = bounds.iterator.map(_._1).toSet
          mapTemplateVars(vars, res, goal).flatMap { sub =>
            if (sub.size != bounds.size) None
            else {
              val env: Map[Type.Var, Type] =
                sub.iterator.map { case (b, tpe) =>
                  (b: Type.Var) -> tpe
                }.toMap
              Some(args.toList.map(t => Type.substituteVar(t, env)))
            }
          }
        }
      case _ =>
        None
    }
  }

  private def instantiatedCtorArgsForGoal(
      ctor: CtorSig,
      goal: Type
  ): Option[List[Type]] = {
    val (goalRoot, goalArgs) = Type.unapplyAll(goal)
    val (ctorRoot, ctorResArgs) = Type.unapplyAll(ctor.result)
    val paramVars = ctor.typeParams.map(_._1)
    val shapeOk =
      ctorResArgs.length == paramVars.length &&
        ctorResArgs.zip(paramVars).forall { case (arg, v) =>
          arg.sameAs(Type.TyVar(v))
        }
    if (!ctorRoot.sameAs(goalRoot) || !shapeOk || goalArgs.length != paramVars.length) {
      None
    } else {
      val env: Map[Type.Var, Type] =
        paramVars.iterator.zip(goalArgs.iterator).map { case (b, t) =>
          (b: Type.Var) -> t
        }.toMap
      Some(ctor.args.map(arg => Type.substituteVar(arg, env)))
    }
  }

  private def instantiatedCtorsForGoal(
      ctx: Ctx,
      goal: Type
  ): Vector[(CtorSig, List[Type])] =
    ctx.ctors.flatMap { ctor =>
      instantiatedCtorArgsForGoal(ctor, goal).map(args => (ctor, args))
    }

  private def constructorFamilies(
      ctx: Ctx
  ): Map[Type.TyConst, Vector[CtorSig]] =
    ctx.ctors.foldLeft(Map.empty[Type.TyConst, Vector[CtorSig]]) { (acc, ctor) =>
      Type.rootConst(ctor.result) match {
        case Some(root) =>
          val prev = acc.getOrElse(root, Vector.empty)
          acc.updated(root, prev :+ ctor)
        case None =>
          acc
      }
    }

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

  private def genTypeFromVarsOnlyOfKind(
      kind: Kind,
      depth: Int,
      localVars: Vector[KindedTypeVar]
  ): Option[Gen[Type]] = {
    val candidates = localVars.flatMap { v =>
      requiredArgKinds(v.kind, kind).flatMap {
        case Nil =>
          Some(Gen.const(v.tpe))
        case needed =>
          if (depth <= 0) None
          else
            needed
              .traverse(k => genTypeFromVarsOnlyOfKind(k, depth - 1, localVars))
              .map { argGens =>
                argGens.map(Gen.lzy(_)).sequence.map(args =>
                  Type.applyAll(v.tpe, args)
                )
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

  private def genFieldType(
      ctx: Ctx,
      depth: Int,
      vars: Vector[KindedTypeVar]
  ): Gen[Type] =
    genTypeFromVarsOfKind(Kind.Type, ctx, depth, vars)
      .orElse(genTypeWithVars(ctx, depth, vars))
      .getOrElse(Gen.const(Type.IntType))

  private def genGoalType(ctx: Ctx, depth: Int): Gen[Type] = {
    val atom = oneOfVector(baseTypes).map(_.map(tc => (tc: Type)))
      .getOrElse(Gen.const(Type.IntType))
    val mono = genTypeOfKind(Kind.Type, ctx, depth).getOrElse(atom)
    if (depth <= 0) mono
    else {
      val subDepth = (depth - 1).max(0)
      val fnType =
        for {
          arity <- Gen.frequency((4, Gen.const(1)), (2, Gen.const(2)))
          args <- Gen.listOfN(arity, Gen.lzy(genGoalType(ctx, subDepth)))
          res <- Gen.lzy(genGoalType(ctx, subDepth))
          nel = NonEmptyList.fromListUnsafe(args)
        } yield Type.Fun(nel, res)
      weightedOneOf(Vector((5, mono), (3, fnType))).getOrElse(mono)
    }
  }

  private def genTypeWithVars(
      ctx: Ctx,
      depth: Int,
      vars: Vector[KindedTypeVar]
  ): Option[Gen[Type]] =
    genTypeOfKind(Kind.Type, ctx, depth, vars)

  private def genTypeParams(
      ctx: Ctx,
      maxCount: Int
  ): Gen[List[(Type.Var.Bound, Kind)]] =
    if (maxCount <= 0) Gen.const(Nil)
    else {
      val kindGen =
        oneOfVector(kindsIn(ctx).toList.toVector).getOrElse(Gen.const(Kind.Type))
      val countGen =
        if (maxCount == 1)
          Gen.frequency((2, Gen.const(0)), (4, Gen.const(1)))
        else
          Gen.frequency((2, Gen.const(0)), (4, Gen.const(1)), (2, Gen.const(2)))

      for {
        count0 <- countGen
        count = count0.min(maxCount).max(0)
        names <- Gen.pick(count, quantVarPool).map(_.toList.sorted)
        kinds <- Gen.listOfN(
          count,
          Gen.frequency((6, Gen.const(Kind.Type)), (2, kindGen))
        )
      } yield names.zip(kinds).map { case (n, k) =>
        (Type.Var.Bound(n), k)
      }
    }

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
      goal: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] = {
    val argDepth = (depth - 1).max(0)
    val candidates = instantiatedCtorsForGoal(ctx, goal)
    val ctorExprs = candidates.flatMap { ctor =>
      val (sig, args) = ctor
      args
        .traverse(arg => genExpr(ctx, arg, argDepth, cfg))
        .map { argExprGens =>
          argExprGens.map(Gen.lzy(_)).sequence.map { args =>
            val fn = Declaration.Var(sig.cons)
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
    val argPatterns = NonEmptyList.fromListUnsafe(args.toList.map(_ => Pattern.WildCard))
    genExpr(ctx, res, depth - 1, cfg).map { bodyGen =>
      Gen.lzy(bodyGen).map(body => Declaration.Lambda(argPatterns, body))
    }
  }

  private def genLambdaApply(
      ctx: Ctx,
      goal: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] =
    if (depth <= 1) None
    else {
      val bodyDepth = (depth - 1).max(0)
      val argDepth = (depth - 2).max(0)
      val innerCfg = cfg.copy(allowMatch = false)
      val argTypeCandidates =
        (baseTypes.map(tc => (tc: Type)) ++
          allTypeCtors(ctx).collect { case TypeCtorSig(tc, Kind.Type) => (tc: Type) } ++
          ctx.vals.map(v => dropForAll(v.sigma))).distinct

      val lambdaApplyCandidates = argTypeCandidates.flatMap { argType =>
        (
          genExpr(ctx, goal, bodyDepth, innerCfg),
          genExpr(ctx, argType, argDepth, innerCfg)
        ) match {
          case (Some(bodyGen), Some(argGen)) =>
            Some(
              for {
                body <- Gen.lzy(bodyGen)
                arg <- Gen.lzy(argGen)
                lambda = Declaration.Lambda(NonEmptyList.one(Pattern.WildCard), body)
              } yield Declaration.Apply(
                lambda,
                NonEmptyList.one(arg),
                Declaration.ApplyKind.Parens
              )
            )
          case _ =>
            None
        }
      }
      oneOfGenerators(lambdaApplyCandidates)
    }

  private def genTotalMatch(
      ctx: Ctx,
      goal: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] =
    if (depth <= 1) None
    else {
      val scrutDepth = (depth - 1).max(0)
      val branchDepth = (depth - 2).max(0)
      val innerCfg = cfg.copy(allowMatch = false, allowGuards = false)
      val families0 = constructorFamilies(ctx).valuesIterator.toVector
      val monoFamilies = families0.filter(_.forall(_.typeParams.isEmpty))
      val families =
        monoFamilies.filter(_.lengthCompare(1) > 0) match {
          case nonEmpty if nonEmpty.nonEmpty => nonEmpty
          case _                             => monoFamilies
        }

      val candidates = families.flatMap { family =>
        val scrutineeType = family.head.result
        val instantiated = family.toList.map(ctor => (ctor, ctor.args))
        val branchGensOpt = instantiated.traverse { case (ctor, argTypes) =>
          val pattern = Pattern.PositionalStruct(
            Pattern.StructKind.Named(
              ctor.cons,
              Pattern.StructKind.Style.TupleLike
            ),
            argTypes.map(_ => Pattern.WildCard)
          )
          genExpr(ctx, goal, branchDepth, innerCfg).map { bodyGen =>
            Gen.lzy(bodyGen)
              .map(body => Declaration.MatchBranch(pattern, None, OptIndent.SameLine(body)))
          }
        }
        val scrutineeGenOpt = genExpr(ctx, scrutineeType, scrutDepth, innerCfg)
        (for {
          scrutineeGen <- scrutineeGenOpt
          branchGens <- branchGensOpt
        } yield for {
          scrutinee <- Gen.lzy(scrutineeGen)
          branchList <- branchGens.sequence
          cases = NonEmptyList.fromListUnsafe(branchList)
        } yield Declaration.Match(
          RecursionKind.NonRecursive,
          scrutinee,
          OptIndent.SameLine(cases)
        )).toVector
      }
      oneOfGenerators(candidates)
    }

  private def genIntro(
      ctx: Ctx,
      goal: Type,
      depth: Int,
      cfg: Config
  ): Option[Gen[Declaration.NonBinding]] =
    val exact = ctx.vals.filter(v => v.sigma.sameAs(goal)).toVector
    val byExact = oneOfVector(exact).map(_.map(v => Declaration.Var(v.name)))
    goal match {
      case Type.Fun(args, res) =>
        genLambda(ctx, args, res, depth, cfg)
      case tc: Type.TyConst =>
        genLiteralFor(tc).orElse(genCtorIntro(ctx, goal, depth, cfg)).orElse(byExact)
      case _ =>
        genCtorIntro(ctx, goal, depth, cfg).orElse(byExact)
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
    val applyArgDepth = (depth - 2).max(0)
    val applyFns =
      if (depth <= 1 || isFunctionType(goal)) Vector.empty
      else ctx.vals.toVector.take(12)
    val byApplyCandidates = applyFns.flatMap { fn =>
      instantiatedFnArgsForGoal(fn.sigma, goal).toVector.flatMap { args =>
        args
          .traverse(t => genExpr(ctx, t, applyArgDepth, cfg))
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
    }
    val byApply = oneOfGenerators(byApplyCandidates)
    val byLambdaApply =
      if (depth > 2 && !isFunctionType(goal)) genLambdaApply(ctx, goal, depth, cfg)
      else None
    val byMatch =
      if (cfg.allowMatch && !isFunctionType(goal) && depth > 1)
        genTotalMatch(ctx, goal, depth - 1, cfg)
      else None

    if (depth <= 0) {
      oneOfGenerators(intro.toVector ++ byVar.toVector)
    } else {
      val introW = if (depth >= 3) 3 else 5
      val varW = if (depth >= 3) 1 else 3
      val applyW = if (depth >= 3) 4 else 2
      val lambdaApplyW = if (depth >= 3) 3 else 1
      val matchW = if (depth >= 4) 2 else 1
      val choices = Vector.newBuilder[(Int, Gen[Declaration.NonBinding])]
      intro.foreach(g => choices += ((introW, Gen.lzy(g))))
      byVar.foreach(g => choices += ((varW, g)))
      byApply.foreach(g => choices += ((applyW, g)))
      byLambdaApply.foreach(g => choices += ((lambdaApplyW, g)))
      byMatch.foreach(g => choices += ((matchW, g)))
      weightedOneOf(choices.result())
    }
  }

  private def fallbackExprPairs(
      ctx: Ctx,
      depth: Int,
      cfg: Config
  ): Vector[Gen[(Type, Declaration.NonBinding)]] = {
    val fallbackTypes =
      (baseTypes.map(tc => (tc: Type)) ++
        allTypeCtors(ctx).collect { case TypeCtorSig(tc, Kind.Type) => (tc: Type) } ++
        ctx.vals.map(v => dropForAll(v.sigma))).distinct
    fallbackTypes.flatMap { tpe =>
      genExpr(ctx, tpe, depth, cfg).map { exprGen =>
        Gen.lzy(exprGen).map(expr => (tpe, expr))
      }
    }
  }

  private def genGoalExprPair(
      ctx: Ctx,
      goalGen: Gen[Type],
      depth: Int,
      cfg: Config
  ): Gen[(Type, Declaration.NonBinding)] = {
    val fallback = oneOfGenerators(fallbackExprPairs(ctx, depth, cfg))
      .getOrElse(
        Gen.const(((Type.BoolType: Type), Declaration.Var(predefTrue)))
      )
    goalGen.flatMap { goal =>
      genExpr(ctx, goal, depth, cfg) match {
        case Some(exprGen) => Gen.lzy(exprGen).map(expr => (goal, expr))
        case None          => fallback
      }
    }
  }

  private def genBindStep(ctx: Ctx, cfg: Config): Gen[Step] = {
    val (name, ctx1) = freshValueName(ctx)
    for {
      pair <- genGoalExprPair(ctx1, genGoalType(ctx1, cfg.maxTypeDepth), cfg.maxExprDepth, cfg)
      (goal, expr) = pair
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
      argPatterns = NonEmptyList.fromListUnsafe(args.map(_ => Pattern.WildCard))
      localCtx = ctx1
      bodyPair <- genGoalExprPair(
        localCtx,
        Gen.const(result),
        cfg.maxExprDepth,
        cfg
      )
      (resultType, body) = bodyPair
      stmt = Statement.Def(
        DefStatement(
          name = name,
          typeArgs = None,
          args = NonEmptyList.one(argPatterns),
          retType = None,
          result = OptIndent.SameLine(body)
        )
      )(emptyRegion)
      fnType = Type.Fun(NonEmptyList.fromListUnsafe(args), resultType)
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
      argTypeGen = genTypeFromVarsOnlyOfKind(Kind.Type, cfg.maxTypeDepth, tvs)
        .getOrElse(Gen.const(Type.TyVar(bounds.head._1)))
      firstArg <- Gen.lzy(argTypeGen)
      extraArgCount <- Gen.choose(0, 2)
      extraArgs <- Gen.listOfN(extraArgCount, Gen.lzy(argTypeGen))
      args = firstArg :: extraArgs
      resultBaseGen = genTypeFromVarsOnlyOfKind(Kind.Type, cfg.maxTypeDepth, tvs)
        .getOrElse(Gen.const(firstArg))
      resultBase <- Gen.oneOf(
        Gen.const(firstArg),
        Gen.lzy(resultBaseGen)
      )
      result <- Gen.oneOf(
        Gen.const(resultBase),
        Gen.const(Type.Fun(NonEmptyList.one(firstArg), resultBase))
      )
      usedBounds = {
        val used = Type.freeBoundTyVars((result :: args)).toSet
        bounds.toList.filter { case (b, _) => used(b) }
      }
      argRefs = args.zipWithIndex.map { case (tpe, idx) =>
        (Identifier.Name(s"arg$idx"): Identifier.Bindable, typeRefOf(tpe))
      }
      typeArgsRef = NonEmptyList.fromList(
        usedBounds.map { case (b, k) =>
          (TypeRef.TypeVar(b.name), if (k.isType) None else Some(k))
        }
      )
      stmt = Statement.ExternalDef(
        name = name,
        typeArgs = typeArgsRef,
        params = argRefs,
        result = typeRefOf(result)
      )(emptyRegion)
      sigma = Type.forAll(usedBounds, Type.Fun(NonEmptyList.fromListUnsafe(args), result))
      next = ctx1.copy(vals = ctx1.vals :+ ValSig(name, sigma))
    } yield Step(stmt, next, None)
  }

  private def genStructStep(ctx: Ctx, cfg: Config): Gen[Step] = {
    val (cons, ctx1) = freshConstructor(ctx, "WtStruct")
    val root = Type.TyConst(Type.Const.Defined(ctx.packageName, TypeName(cons)))
    for {
      typeParams0 <- genTypeParams(ctx1, 2)
      localVars0 = typeParams0.map { case (b, k) =>
        KindedTypeVar(Type.TyVar(b), k)
      }.toVector
      fieldTypeGenOpt =
        if (typeParams0.nonEmpty)
          genTypeFromVarsOnlyOfKind(Kind.Type, cfg.maxTypeDepth, localVars0)
        else None
      argc <-
        if (typeParams0.isEmpty) Gen.choose(0, 2)
        else if (fieldTypeGenOpt.isDefined) Gen.choose(0, 2)
        else Gen.const(0)
      args <- typeParams0 match {
        case Nil =>
          Gen.listOfN(argc, genFieldType(ctx1, cfg.maxTypeDepth, localVars0))
        case _ =>
          fieldTypeGenOpt match {
            case Some(fieldTypeGen) =>
              Gen.listOfN(argc, Gen.lzy(fieldTypeGen))
            case None =>
              Gen.const(Nil)
          }
      }
      usedTypeParams = {
        val used = Type.freeBoundTyVars(args).toSet
        typeParams0.filter { case (b, _) => used(b) }
      }
      stmtArgs: List[Statement.ConstructorArg] =
        args.zipWithIndex.map {
          case (tpe, idx) =>
            Statement.ConstructorArg(
              name = Identifier.Name(s"f$idx"),
              tpe =
                if (usedTypeParams.nonEmpty) Some(typeRefOf(tpe))
                else None,
              default = None,
              region = emptyRegion
            )
        }
      stmt = Statement.Struct(cons, typeParamRefs(usedTypeParams), stmtArgs)(emptyRegion)
      resultType = resultTypeFor(root, usedTypeParams)
      ctor = CtorSig(ctx.packageName, cons, usedTypeParams, args, resultType)
      next = ctx1.copy(
        ctors = ctx1.ctors :+ ctor,
        vals = ctx1.vals :+ ValSig(cons, ctorType(ctor)),
        localTypeCtors =
          dedupeTypeCtors(
            ctx1.localTypeCtors :+ TypeCtorSig(root, kindFromParams(usedTypeParams))
          )
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

  private def genEnumStep(ctx: Ctx, cfg: Config): Gen[Step] = {
    val (enumName, ctx1) = freshConstructor(ctx, "WtEnum")
    val root = Type.TyConst(Type.Const.Defined(ctx.packageName, TypeName(enumName)))
    for {
      typeParams0 <- genTypeParams(ctx1, 2)
      localVars0 = typeParams0.map { case (b, k) =>
        KindedTypeVar(Type.TyVar(b), k)
      }.toVector
      branchCount <- Gen.choose(2, 3)
      allocated = allocateConstructors(ctx1, branchCount, s"${enumName.asString}C")
      (constructors, ctx2) = allocated
      branchFieldTypeGenOpt =
        if (typeParams0.nonEmpty)
          genTypeFromVarsOnlyOfKind(Kind.Type, cfg.maxTypeDepth, localVars0)
        else None
      branchArgs <- constructors.traverse { _ =>
        typeParams0 match {
          case Nil =>
            for {
              argc <- Gen.choose(0, 1)
              args <- Gen.listOfN(argc, genFieldType(ctx2, cfg.maxTypeDepth, localVars0))
            } yield args
          case _ =>
            branchFieldTypeGenOpt match {
              case Some(fieldTypeGen) =>
                for {
                  argc <- Gen.choose(0, 1)
                  args <- Gen.listOfN(argc, Gen.lzy(fieldTypeGen))
                } yield args
              case None =>
                Gen.const(Nil)
            }
        }
      }
      usedTypeParams = {
        val used = Type.freeBoundTyVars(branchArgs.flatten).toSet
        typeParams0.filter { case (b, _) => used(b) }
      }
      branches = constructors.zip(branchArgs).map { case (cons, args) =>
        Statement.EnumBranch(
          name = cons,
          typeArgs = None,
          args = args.zipWithIndex.map { case (tpe, idx) =>
            Statement.ConstructorArg(
              name = Identifier.Name(s"a$idx"),
              tpe =
                if (usedTypeParams.nonEmpty) Some(typeRefOf(tpe))
                else None,
              default = None,
              region = emptyRegion
            )
          },
          region = emptyRegion
        )
      }
      stmt = Statement.Enum(
        name = enumName,
        typeArgs = typeParamRefs(usedTypeParams),
        items = OptIndent.SameLine(NonEmptyList.fromListUnsafe(branches))
      )(emptyRegion)
      resultType = resultTypeFor(root, usedTypeParams)
      sigs = constructors.zip(branchArgs).map { case (cons, args) =>
        CtorSig(ctx.packageName, cons, usedTypeParams, args, resultType)
      }
      next = ctx2.copy(
        ctors = ctx2.ctors ++ sigs,
        vals = ctx2.vals ++ sigs.map(s => ValSig(s.cons, ctorType(s))),
        localTypeCtors =
          dedupeTypeCtors(
            ctx2.localTypeCtors :+ TypeCtorSig(root, kindFromParams(usedTypeParams))
          )
      )
    } yield Step(stmt, next, None)
  }

  private def genTypeDefStep(ctx: Ctx, cfg: Config): Gen[Step] =
    Gen.oneOf(genStructStep(ctx, cfg), genEnumStep(ctx, cfg))

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
        steps += ((2, genTypeDefStep(ctx, cfg)))
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
      statements0 = built.statementsRev.reverse
      // Mark generated top-level values as intentionally used so package
      // typechecking does not reject programs for unused declarations.
      useStatements = built.expected.keys.toList.map { name =>
        Statement.Bind(
          BindingStatement(
            Pattern.WildCard,
            Declaration.Var(name),
            ()
          )
        )(emptyRegion)
      }
      statements = statements0 ::: useStatements
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
