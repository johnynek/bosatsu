package dev.bosatsu

import cats.data.{Chain, NonEmptyList, Validated, ValidatedNec}
import dev.bosatsu.rankn._
import munit.Assertions.fail
import Identifier.Constructor

import cats.syntax.all._

object TypeValidator {

  object TypeValidation {
    type Path = Chain[String]
    final case class Error(path: Path, message: String) {
      override def toString: String = s"${path.iterator.mkString("/")}: $message"
    }
    type Result[+A] = ValidatedNec[Error, A]

    val pass: Result[Unit] =
      Validated.validNec(())

    def fail[A](path: Path, message: String): Result[A] =
      Validated.invalidNec(Error(path, message))
  }

  type TypeValidationError = TypeValidation.Error
  type TypeValidation[+A] = TypeValidation.Result[A]

  private implicit final class PathOps(private val path: TypeValidation.Path)
      extends AnyVal {
    def /(name: String): TypeValidation.Path = path ++ Chain.one(name)
  }

  private def isAppFnPath(path: TypeValidation.Path): Boolean =
    path.reverseIterator.take(2).toList == List("fn", "app")

  private case class LoopContext(argTypes: List[Type], recurType: Type)

  private val typeValidationPass: TypeValidation[Unit] =
    TypeValidation.pass

  private def typeValidationFail(
      path: TypeValidation.Path,
      message: String
  ): TypeValidation[Unit] =
    TypeValidation.fail(path, message)

  private def kindOfForValidation(
      env: TypeEnv[Kind.Arg],
      boundKinds: Map[Type.Var.Bound, Kind]
  ): Type => Option[Kind] = {
    case tc: Type.TyConst =>
      env.getType(tc).map(_.kindOf).orElse(Type.builtInKinds.get(tc.tpe.toDefined))
    case Type.TyVar(v: Type.Var.Bound) =>
      boundKinds.get(v)
    case _ =>
      None
  }

  /** Directional type-connectivity check used by validation:
    * `from` can be widened into `to`.
    * `freeBoundKinds` are the in-scope free bound vars and their kinds.
    */
  private type SolvedFreeBounds = Map[Type.Var, Type]

  private def canWidenInScopeAndSolve(
      from: Type,
      to: Type,
      kindOf: Type => Option[Kind],
      freeBoundKinds: Map[Type.Var.Bound, Kind],
      solvedFreeBounds: SolvedFreeBounds
  ): Either[List[Type.Var.Bound], (Boolean, SolvedFreeBounds)] = {
    val from0 = Type.substituteVar(from, solvedFreeBounds)
    val to0 = Type.substituteVar(to, solvedFreeBounds)

    if (from0.sameAs(to0)) Right((true, solvedFreeBounds))
    else {
      val from1 = Type.hoistForAllCovariant(from0, kindOf)
      val to1 = Type.hoistForAllCovariant(to0, kindOf)
      val unresolvedFrom =
        Type
          .freeBoundTyVars(from1 :: Nil)
          .distinct
          .filterNot(freeBoundKinds.contains)
      if (unresolvedFrom.nonEmpty) Left(unresolvedFrom)
      else {
        // Missing bound vars on the right are treated as local existentials to solve.
        // Missing vars on the left are not allowed (see unresolvedFrom above).
        val toVars =
          Type
            .freeBoundTyVars(to1 :: Nil)
            .distinct
            .filterNot(freeBoundKinds.contains)
            .map(_ -> Kind.Type)
            .toMap
        val envKinds = freeBoundKinds
        val (foralls, exists, fromRho) = Type.splitQuantifiers(from1)
        val scopedFrees =
          Type
            .freeBoundTyVars(fromRho :: Nil)
            .distinct
            .filter(envKinds.contains)
            .filterNot(solvedFreeBounds.contains)
        val vars =
          (
            foralls.iterator ++
              exists.iterator ++
              scopedFrees.iterator.map(b => b -> envKinds(b))
          ).toMap

        Type
          .instantiate(
            vars = vars,
            from = fromRho,
            toVars = toVars,
            to = to1,
            env = envKinds
          ) match {
          case None =>
            Right((false, solvedFreeBounds))
          case Some(inst) =>
            val freshSolutions =
              scopedFrees.flatMap { b =>
                inst
                  .subs
                  .get(b)
                  .map { case (_, tpe) => (b: Type.Var) -> tpe }
                  .orElse(
                    inst.frees.get(b).map { case (_, b1) =>
                      (b: Type.Var) -> (Type.TyVar(b1): Type)
                    }
                  )
              }.toMap

            val mergedOpt =
              freshSolutions.foldLeft(Option(solvedFreeBounds)) {
                case (None, _) => None
                case (Some(acc), (b, tpe0)) =>
                  val tpe = Type.substituteVar(tpe0, acc)
                  acc.get(b) match {
                    // This is not full unification: we require an already-solved
                    // variable to be definitionally equal (`sameAs`) to any later
                    // witness, rather than attempting another solve step.
                    // That is stricter than necessary in principle, but current
                    // tests and generated programs have not shown a false negative
                    // here, and this preserves the "single consistent solve" rule.
                    case Some(existing) if existing.sameAs(tpe) =>
                      Some(acc)
                    case Some(_) =>
                      None
                    case None =>
                      Some(acc.updated(b, tpe))
                  }
              }

            Right((mergedOpt.nonEmpty, mergedOpt.getOrElse(solvedFreeBounds)))
        }
      }
    }
  }

  private def canWidenInScope(
      from: Type,
      to: Type,
      kindOf: Type => Option[Kind],
      freeBoundKinds: Map[Type.Var.Bound, Kind]
  ): Either[List[Type.Var.Bound], Boolean] =
    canWidenInScopeAndSolve(
      from,
      to,
      kindOf,
      freeBoundKinds,
      Map.empty
    ).map(_._1)

  private def typeWidensToOrFailWithSolved(
      from: Type,
      to: Type,
      kindOf: Type => Option[Kind],
      freeBoundKinds: Map[Type.Var.Bound, Kind],
      solvedFreeBounds: SolvedFreeBounds,
      path: TypeValidation.Path,
      onMismatch: => String
  ): TypeValidation[SolvedFreeBounds] =
    canWidenInScopeAndSolve(from, to, kindOf, freeBoundKinds, solvedFreeBounds) match {
      case Right((true, solved1)) =>
        Validated.validNec(solved1)
      case Right((false, _)) =>
        TypeValidation.fail(path, onMismatch)
      case Left(unbound) =>
        TypeValidation.fail(
          path,
          s"$onMismatch; unresolved bound type vars (${unbound.iterator.map(_.name).mkString(", ")}) in widen check from ${from.show} to ${to.show}"
        )
    }

  private def typeWidensToOrFail(
      from: Type,
      to: Type,
      kindOf: Type => Option[Kind],
      freeBoundKinds: Map[Type.Var.Bound, Kind],
      path: TypeValidation.Path,
      onMismatch: => String
  ): TypeValidation[Unit] =
    canWidenInScope(from, to, kindOf, freeBoundKinds) match {
      case Right(true) =>
        typeValidationPass
      case Right(false) =>
        typeValidationFail(path, onMismatch)
      case Left(unbound) =>
        typeValidationFail(
          path,
          s"$onMismatch; unresolved bound type vars (${unbound.iterator.map(_.name).mkString(", ")}) in widen check from ${from.show} to ${to.show}"
        )
    }

  /** Make sure no illegal final types escaped into a TypedExpr
    */
  def assertValid[A](te: TypedExpr[A]): Unit = {
    def checkType[T <: Type](t: T, bound: Set[Type.Var.Bound]): T =
      t match {
        case t @ Type.TyVar(Type.Var.Skolem(_, _, _, _)) =>
          sys.error(s"illegal skolem ($t) escape in ${te.repr}")
        case Type.TyVar(Type.Var.Bound(_)) => t
        case t @ Type.TyMeta(_)            =>
          sys.error(s"illegal meta ($t) escape in ${te.repr}")
        case Type.TyApply(left, right) =>
          checkType(left, bound): Unit
          checkType(right, bound): Unit
          t
        case Type.ForAll(vars, in) =>
          checkType(in, bound ++ vars.toList.map(_._1)): Unit
          t
        case Type.Exists(vars, in) =>
          checkType(in, bound ++ vars.toList.map(_._1)): Unit
          t
        case Type.TyConst(_) => t
      }
    te.traverseType[cats.Id](checkType(_, Set.empty)): Unit

    def checkExpr(expr: TypedExpr[A]): Unit =
      expr match {
        case TypedExpr.Generic(_, in) =>
          checkExpr(in)
        case TypedExpr.Annotation(term, tpe, qev) =>
          qev.foreach { ev =>
            Require(
              ev.targetAtSolve.sameAs(tpe),
              s"quantifier evidence invariant violated: targetAtSolve ${ev.targetAtSolve} " +
                s"is not sameAs annotation type $tpe in ${te.repr}"
            )
          }
          checkExpr(term)
        case TypedExpr.AnnotatedLambda(_, body, _) =>
          checkExpr(body)
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          ()
        case TypedExpr.App(fn, args, _, _) =>
          checkExpr(fn)
          args.iterator.foreach(checkExpr)
        case TypedExpr.Let(_, expr0, in, _, _) =>
          checkExpr(expr0)
          checkExpr(in)
        case TypedExpr.Loop(args, body, _) =>
          args.iterator.foreach { case (_, init) =>
            checkExpr(init)
          }
          checkExpr(body)
        case TypedExpr.Recur(args, _, _) =>
          args.iterator.foreach(checkExpr)
        case TypedExpr.Match(arg, branches, _) =>
          checkExpr(arg)
          branches.iterator.foreach { branch =>
            branch.guard.foreach(checkExpr)
            checkExpr(branch.expr)
          }
      }

    checkExpr(te)

    val tp = te.getType
    lazy val teStr = Type.fullyResolvedDocument.document(tp).render(80)
    Require(
      Type.freeTyVars(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}"
    )

    Require(
      Type.metaTvs(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}"
    )
  }

  private def assertTypeValidationResult(
      stage: String,
      res: TypeValidation[Unit]
  ): Unit =
    res match {
      case Validated.Valid(_) =>
        ()
      case Validated.Invalid(errs) =>
        val msg = errs.iterator.map(_.toString).mkString("\n")
        fail(s"type connectivity validation failed at $stage:\n$msg")
    }

  private def validateTypedExprInvariants[A](
      te: TypedExpr[A],
      path: TypeValidation.Path
  ): TypeValidation[Unit] =
    Either
      .catchNonFatal(assertValid(te))
      .leftMap(err =>
        TypeValidation.Error(path, Option(err.getMessage).getOrElse(err.toString))
      )
      .toValidatedNec
      .void

  private def globalValuesFromTypeEnv(
      env: TypeEnv[Kind.Arg]
  ): Map[(PackageName, Identifier), Type] =
    env.referencedPackages.iterator
      .flatMap { p =>
        env.localValuesOf(p).iterator.map { case (n, t) =>
          ((p, n: Identifier), t)
        }
      }
      .toMap

  private def letGlobalValues[A](
      pack: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[A])]
  ): Map[(PackageName, Identifier), Type] =
    lets.iterator.map { case (n, _, te) =>
      ((pack, n: Identifier), te.getType)
    }.toMap

  private def patternBindingTypes(
      scrutineeType: Type,
      pat: Pattern[(PackageName, Constructor), Type],
      env: TypeEnv[Kind.Arg],
      boundKinds: Map[Type.Var.Bound, Kind],
      path: TypeValidation.Path
  ): TypeValidation[Map[Identifier.Bindable, Type]] = {
    type Bindings = Map[Identifier.Bindable, Type]
    val kindOf = kindOfForValidation(env, boundKinds)

    def mergeBindings(
        at: TypeValidation.Path,
        left: TypeValidation[Bindings],
        right: TypeValidation[Bindings]
    ): TypeValidation[Bindings] =
      left.product(right).andThen { case (lbindings, rbindings) =>
        val collisions = lbindings.keySet & rbindings.keySet
        val collisionValid = collisions.toList.sortBy(_.sourceCodeRepr).traverse_ {
          name =>
            val leftType = lbindings(name)
            val rightType = rbindings(name)
            if (leftType.sameAs(rightType)) typeValidationPass
            else
              TypeValidation.fail(
                at / name.sourceCodeRepr,
                s"pattern binding type mismatch: existing ${leftType.show}, new ${rightType.show}"
              )
        }

        collisionValid.as(lbindings ++ rbindings)
      }

    def mergeAll(
        at: TypeValidation.Path,
        checks: List[TypeValidation[Bindings]]
    ): TypeValidation[Bindings] =
      checks.foldLeft(Validated.validNec(Map.empty): TypeValidation[Bindings]) {
        (acc, next) =>
          mergeBindings(at, acc, next)
      }

    def listItemTypeOf(tpe: Type): Option[Type] =
      tpe match {
        case Type.ListT(item) => Some(item)
        case Type.ForAll(vars, Type.ListT(item)) if vars.tail.isEmpty =>
          Some(Type.forAll(vars, item))
        case _ => None
      }

    def constructorArgTypes(
        at: TypeValidation.Path,
        expectedType: Type,
        pack: PackageName,
        cons: Constructor
    ): TypeValidation[List[Type]] =
      env.getConstructor(pack, cons) match {
        case None =>
          TypeValidation.fail(
            at,
            s"unknown constructor ${pack.asString}::${cons.sourceCodeRepr}"
          )
        case Some((dt, cf)) =>
          val pushedExpected = Type.pushDownForAllCovariant(
            expectedType,
            {
              case tc: Type.TyConst if tc.sameAs(dt.toTypeTyConst) =>
                Some(dt.kindOf)
              case _ => None
            }
          )
          val (foralls, exists, rho) = Type.splitQuantifiers(pushedExpected)
          val (head, targs) = Type.unapplyAll(rho)
          if (!head.sameAs(dt.toTypeTyConst))
            TypeValidation.fail(
              at,
              s"constructor ${pack.asString}::${cons.sourceCodeRepr} does not match scrutinee type ${expectedType.show}"
            )
          else if (targs.length != dt.typeParams.length)
            TypeValidation.fail(
              at,
              s"constructor type argument arity mismatch: expected ${dt.typeParams.length}, got ${targs.length}"
            )
          else {
            val subMap: Map[Type.Var, Type] =
              dt.typeParams.iterator
                .zip(targs.iterator)
                .map { case (tv, ty) => (tv: Type.Var, ty) }
                .toMap
            Validated.validNec(
              cf.args.map { arg =>
                Type.quantify(foralls, exists, Type.substituteVar(arg.tpe, subMap))
              }
            )
          }
      }

    def loop(
        expectedType: Type,
        pattern: Pattern[(PackageName, Constructor), Type],
        at: TypeValidation.Path
    ): TypeValidation[Bindings] =
      pattern match {
        case Pattern.WildCard | Pattern.Literal(_) =>
          Validated.validNec(Map.empty)

        case Pattern.Var(name) =>
          Validated.validNec(Map(name -> expectedType))

        case Pattern.Named(name, inner) =>
          mergeAll(
            at,
            Validated.validNec(Map(name -> expectedType)) ::
              loop(expectedType, inner, at / "named") :: Nil
          )

        case Pattern.StrPat(parts) =>
          val strCheck =
            if (expectedType.sameAs(Type.StrType)) typeValidationPass
            else
              typeValidationFail(
                at,
                s"string pattern expects String scrutinee, got ${expectedType.show}"
              )
          val partBindings = parts.toList.collect {
            case Pattern.StrPart.NamedStr(name) =>
              Validated.validNec(Map(name -> Type.StrType))
            case Pattern.StrPart.NamedChar(name) =>
              Validated.validNec(Map(name -> Type.CharType))
          }
          val mergedParts = mergeAll(at, partBindings)
          strCheck *> mergedParts

        case Pattern.ListPat(parts) =>
          listItemTypeOf(expectedType) match {
            case None =>
              TypeValidation.fail(
                at,
                s"list pattern expects List scrutinee, got ${expectedType.show}"
              )
            case Some(itemType) =>
              val partChecks = parts.toList.zipWithIndex.map {
                case (Pattern.ListPart.WildList, _) =>
                  Validated.validNec(Map.empty)
                case (Pattern.ListPart.NamedList(name), _) =>
                  Validated.validNec(Map(name -> expectedType))
                case (Pattern.ListPart.Item(inner), idx) =>
                  loop(itemType, inner, at / "list" / s"item[$idx]")
              }
              mergeAll(at, partChecks)
          }

        case Pattern.Annotation(inner, annType: Type) =>
          typeWidensToOrFail(
            expectedType,
            annType,
            kindOf,
            boundKinds,
            at / "annotation",
            s"pattern annotation type mismatch: expected scrutinee ${expectedType.show} to widen into ${summon[cats.Show[Type]].show(annType)}"
          ) *> loop(annType, inner, at / "annotation")

        case Pattern.PositionalStruct((pack, cons), args) =>
          constructorArgTypes(at, expectedType, pack, cons).andThen { inferredArgs =>
            val arityCheck =
              if (args.lengthCompare(inferredArgs.length) == 0) typeValidationPass
              else
                typeValidationFail(
                  at,
                  s"constructor arg arity mismatch for ${pack.asString}::${cons.sourceCodeRepr}: expected ${inferredArgs.length}, got ${args.length}"
                )

            val argChecks = args.toList
              .zip(inferredArgs)
              .zipWithIndex
              .map { case ((innerPat, argType), idx) =>
                loop(argType, innerPat, at / s"arg[$idx]")
              }
            val mergedArgs = mergeAll(at, argChecks)

            arityCheck *> mergedArgs
          }

        case Pattern.Union(head, rest) =>
          val branchChecks = (head :: rest.toList).zipWithIndex.map {
            case (innerPat, idx) =>
              loop(expectedType, innerPat, at / s"union[$idx]")
          }
          branchChecks.sequence.andThen { maps =>
            val expectedNames = maps.headOption.map(_.keySet).getOrElse(Set.empty)
            val sameNameChecks = maps.zipWithIndex.drop(1).map { case (m, idx) =>
              if (m.keySet == expectedNames) typeValidationPass
              else
                typeValidationFail(
                  at / s"union[$idx]",
                  s"union branch binders differ: expected ${expectedNames.map(_.sourceCodeRepr).toList.sorted.mkString(", ")}, got ${m.keySet.map(_.sourceCodeRepr).toList.sorted.mkString(", ")}"
                )
            }
            val mergedMaps = mergeAll(at, maps.map(Validated.validNec(_)))
            sameNameChecks.sequence_ *> mergedMaps
          }
      }

    loop(scrutineeType, pat, path)
  }

  private def validateTypeConnections[A](
      te: TypedExpr[A],
      locals: Map[Identifier.Bindable, Type],
      globals: Map[(PackageName, Identifier), Type],
      env: TypeEnv[Kind.Arg],
      loopStack: List[LoopContext],
      boundKinds: Map[Type.Var.Bound, Kind],
      path: TypeValidation.Path
  ): TypeValidation[Unit] = {
    val (teForalls, teExists, _) = Type.splitQuantifiers(te.getType)
    val inScopeKinds = boundKinds ++ teForalls.iterator ++ teExists.iterator
    val kindOf = kindOfForValidation(env, inScopeKinds)
    // All checks are directional: left side widens into right side.
    te match {
      case TypedExpr.Generic(quant, in) =>
        val quantKinds = quant.vars.iterator.toMap
        val expected =
          Type.quantify(quant.forallList, quant.existList, in.getType)
        val quantCheck =
          if (te.getType.sameAs(expected)) typeValidationPass
          else
            typeValidationFail(
              path,
              s"generic quantification type mismatch: expected ${expected.show}, got ${te.getType.show}"
            )
        (
          quantCheck ::
            validateTypeConnections(
              in,
              locals,
              globals,
              env,
              loopStack,
              inScopeKinds ++ quantKinds,
              path / "generic"
            ) :: Nil
        ).sequence_

      case TypedExpr.Annotation(term, tpe, qev) =>
        val evidenceCheck = qev match {
          case Some(ev) if !ev.targetAtSolve.sameAs(tpe) =>
            typeValidationFail(
              path,
              s"annotation evidence target mismatch: evidence=${ev.targetAtSolve.show}, annotation=${tpe.show}"
            )
          case _ =>
            typeValidationPass
        }
        (
          evidenceCheck ::
            validateTypeConnections(
              term,
              locals,
              globals,
              env,
              loopStack,
              inScopeKinds,
              path / "annotation"
            ) :: Nil
        ).sequence_

      case TypedExpr.AnnotatedLambda(args, body, _) =>
        val argsMap = args.iterator.toMap
        validateTypeConnections(
          body,
          locals ++ argsMap,
          globals,
          env,
          loopStack,
          inScopeKinds,
          path / "lambda"
        )

      case TypedExpr.Local(name, tpe, _) =>
        locals.get(name) match {
          case Some(expected) =>
            if (isAppFnPath(path)) typeValidationPass
            else
              typeWidensToOrFail(
                tpe,
                expected,
                kindOf,
                inScopeKinds,
                path,
                s"local type mismatch for ${name.sourceCodeRepr}: expected ${expected.show}, got ${tpe.show}"
              )
          case None =>
            typeValidationFail(
              path,
              s"unbound local ${name.sourceCodeRepr} with type ${tpe.show}"
            )
        }

      case TypedExpr.Global(pack, name, tpe, _) =>
        globals.get((pack, name)).orElse(env.getValue(pack, name)) match {
          case Some(expected) =>
            typeWidensToOrFail(
              tpe,
              expected,
              kindOf,
              inScopeKinds,
              path,
              s"global type mismatch for ${pack.asString}::${name.sourceCodeRepr}: expected ${expected.show}, got ${tpe.show}"
            )
          case None =>
            typeValidationFail(
              path,
              s"unknown global ${pack.asString}::${name.sourceCodeRepr} with type ${tpe.show}"
            )
        }

      case TypedExpr.App(fn, args, _, _) =>
        // Application checking model:
        //   (app (fn : quant (t1, t2, ..., tn) -> r) a1, a2, ..., an : appT)
        //
        // We check these properties (where `<:<` means "widens to"):
        // P1: We can decompose `fn` into expected arg/result types plus app-local
        //     quantifier kinds, with quantifier shadowing handled safely.
        // P2: For some instantiation S, each argument type satisfies
        //       S(ti) <:< type(ai)
        //     and we thread S so repeated free vars are solved consistently.
        // P3: The application result type satisfies
        //       S(r) <:< appT
        //     including any in-scope free vars solved while checking args.
        @annotation.tailrec
        def consumeArgs(
            currentFnType: Type,
            remaining: Int,
            accRev: List[Type],
            accKinds: Map[Type.Var.Bound, Kind]
        ): Either[String, (List[Type], Type, Map[Type.Var.Bound, Kind])] =
          if (remaining <= 0) Right((accRev.reverse, currentFnType, accKinds))
          else {
            val (foralls0, exists0, rho0) = Type.splitQuantifiers(currentFnType)
            val avoid = accKinds.keySet ++ inScopeKinds.keySet
            val quantUnshadowed = Type
              .quantify(foralls0, exists0, rho0) match {
              case fa: Type.ForAll => fa.unshadow(avoid)
              case ex: Type.Exists => ex.unshadow(avoid)
              case tpe             => tpe
            }
            val (foralls, exists, rho) = Type.splitQuantifiers(quantUnshadowed)
            val nextKinds = accKinds ++ foralls.iterator ++ exists.iterator
            Type.Fun.unapply(rho) match {
              case Some((fnArgs, fnRes)) =>
                val argsList = fnArgs.toList
                if (argsList.lengthCompare(remaining) <= 0)
                  consumeArgs(
                    fnRes,
                    remaining - argsList.length,
                    argsList reverse_::: accRev,
                    nextKinds
                  )
                else {
                  val consumed = argsList.take(remaining)
                  val leftover = argsList.drop(remaining)
                  consumeArgs(
                    Type.quantify(
                      foralls,
                      exists,
                      Type.Fun(NonEmptyList.fromListUnsafe(leftover), fnRes)
                    ),
                    0,
                    consumed reverse_::: accRev,
                    nextKinds
                  )
                }
              case None =>
                Left(
                  s"application head is not a function type: ${currentFnType.show}"
                )
            }
          }

        // here we check P1 (using the annotated type on `fn` first)
        val appDecomposition0 = consumeArgs(fn.getType, args.length, Nil, Map.empty)
        val appDecomposition =
          appDecomposition0 match {
            case right @ Right(_) => right
            case Left(_) =>
              // here we check P1 fallback: prefer env/local/global fn type if needed
              fn match {
                case TypedExpr.Local(name, _, _) =>
                  locals
                    .get(name)
                    .map(tpe => consumeArgs(tpe, args.length, Nil, Map.empty))
                    .getOrElse(appDecomposition0)
                case TypedExpr.Global(pack, name, _, _) =>
                  globals
                    .get((pack, name))
                    .orElse(env.getValue(pack, name))
                    .map(tpe => consumeArgs(tpe, args.length, Nil, Map.empty))
                    .getOrElse(appDecomposition0)
                case _ =>
                  appDecomposition0
              }
          }

        val appKinds =
          inScopeKinds ++ appDecomposition.fold(_ => Map.empty, _._3)

        // here we check children recursively. This does not propagate solve
        // substitutions upward; it validates each subtree against its own
        // annotated type. P1/P2/P3 below then check the application-specific
        // relation between fn/arg/result boundary types at this node.
        val childChecks = (
          validateTypeConnections(
            fn,
            locals,
            globals,
            env,
            loopStack,
            inScopeKinds,
            path / "app" / "fn"
          ) :: args.toList.zipWithIndex.map { case (arg, idx) =>
            validateTypeConnections(
              arg,
              locals,
              globals,
              env,
              loopStack,
              appKinds,
              path / "app" / s"arg[$idx]"
            )
          }
        ).sequence_

        val appTypeChecks = appDecomposition match {
          case Left(message) =>
            typeValidationFail(path, message)
          case Right((expectedArgTypes0, expectedResult0, appBoundKinds)) =>
            val gotArgs = args.toList
            // here we check P3 pre-solve: use result type to seed substitution S
            val solveVars =
              appBoundKinds.iterator
                .filterNot { case (v, _) => inScopeKinds.contains(v) }
                .map { case (v, _) => v -> Kind.Type }
                .toMap
            val solveToVars =
              Type
                .freeBoundTyVars(te.getType :: Nil)
                .distinct
                .filterNot(inScopeKinds.contains)
                .map(_ -> Kind.Type)
                .toMap
            val solvedOpt =
              Type.instantiate(
                vars = solveVars,
                from = expectedResult0,
                toVars = solveToVars,
                to = te.getType,
                env = inScopeKinds
              )
            val solveSubMap: Map[Type.Var, Type] =
              solvedOpt
                .map { solved =>
                  val freeSubs: Map[Type.Var, Type] =
                    solved.frees.iterator.map { case (v, (_, b)) =>
                      (v: Type.Var) -> Type.TyVar(b)
                    }.toMap
                  val concreteSubs: Map[Type.Var, Type] =
                    solved.subs.iterator.map { case (v, (_, tpe)) =>
                      (v: Type.Var) -> tpe
                    }.toMap
                  freeSubs ++ concreteSubs
                }
                .getOrElse(Map.empty)
            val expectedArgTypes =
              expectedArgTypes0.map(Type.substituteVar(_, solveSubMap))
            val expectedResult = Type.substituteVar(expectedResult0, solveSubMap)
            val initialSolved: SolvedFreeBounds =
              solveSubMap

            // here we check P2: each S(ti) <:< type(ai), threading S across args
            val argSolvedChecks: TypeValidation[SolvedFreeBounds] =
              expectedArgTypes
                .zip(gotArgs)
                .zipWithIndex
                .foldLeft(Validated.validNec(initialSolved): TypeValidation[
                  SolvedFreeBounds
                ]) { case (acc, ((expected, got), idx)) =>
                  acc.andThen { solvedSoFar =>
                    typeWidensToOrFailWithSolved(
                      expected,
                      got.getType,
                      kindOf,
                      appKinds,
                      solvedSoFar,
                      path / "app" / s"arg[$idx]",
                      s"app arg type mismatch: expected ${expected.show}, got ${got.getType.show}"
                    )
                  }
                }

            argSolvedChecks.andThen { solvedAfterArgs =>
              val solvedResultSubMap: Map[Type.Var, Type] =
                solvedAfterArgs.iterator.collect {
                  case (b: Type.Var.Bound, tpe) if inScopeKinds.contains(b) =>
                    (b: Type.Var) -> tpe
                }.toMap
              val expectedResultSolved =
                Type.substituteVar(expectedResult, solvedResultSubMap)
              if (expectedArgTypes.lengthCompare(gotArgs.length) == 0)
                // here we check P3: S(r) <:< appT
                typeWidensToOrFail(
                  expectedResultSolved,
                  te.getType,
                  kindOf,
                  appKinds,
                  path,
                  s"application result type mismatch: expected ${expectedResultSolved.show}, got ${te.getType.show}"
                )
              else typeValidationPass
            }
        }

        (childChecks :: appTypeChecks :: Nil).sequence_

      case TypedExpr.Let(arg, expr, in, rec, _) =>
        val exprLocals =
          if (rec.isRecursive) locals.updated(arg, expr.getType)
          else locals
        val inLocals = locals.updated(arg, expr.getType)

        (
          validateTypeConnections(
            expr,
            exprLocals,
            globals,
            env,
            loopStack,
            inScopeKinds,
            path / "let" / arg.sourceCodeRepr / "expr"
          ) ::
            validateTypeConnections(
              in,
              inLocals,
              globals,
              env,
              loopStack,
              inScopeKinds,
              path / "let" / arg.sourceCodeRepr / "in"
            ) :: Nil
        ).sequence_

      case TypedExpr.Loop(args, body, _) =>
        val initChecks = args.toList.zipWithIndex.map { case ((_, init), idx) =>
          validateTypeConnections(
            init,
            locals,
            globals,
            env,
            loopStack,
            inScopeKinds,
            path / "loop" / s"init[$idx]"
          )
        }
        val argTypes = args.toList.map { case (n, init) => (n, init.getType) }
        val duplicateBinds = {
          val seen = scala.collection.mutable.HashSet.empty[Identifier.Bindable]
          argTypes.collect { case (n, _) if !seen.add(n) => n }
        }
        val duplicateCheck =
          if (duplicateBinds.isEmpty) typeValidationPass
          else
            typeValidationFail(
              path,
              s"loop has duplicate binders: ${duplicateBinds.map(_.sourceCodeRepr).mkString(", ")}"
            )
        val bodyChecks = validateTypeConnections(
          body,
          locals ++ argTypes,
          globals,
          env,
          LoopContext(argTypes.map(_._2), te.getType) :: loopStack,
          inScopeKinds,
          path / "loop" / "body"
        )

        (duplicateCheck :: bodyChecks :: initChecks).sequence_

      case TypedExpr.Recur(args, tpe, _) =>
        val argChecks = args.toList.zipWithIndex.map { case (arg, idx) =>
          validateTypeConnections(
            arg,
            locals,
            globals,
            env,
            loopStack,
            inScopeKinds,
            path / "recur" / s"arg[$idx]"
          )
        }
        val recurChecks =
          loopStack match {
            case Nil =>
              typeValidationFail(path, "recur found outside loop")
            case LoopContext(loopArgTypes, loopType) :: _ =>
              val arityCheck =
                if (loopArgTypes.length == args.length) typeValidationPass
                else
                  typeValidationFail(
                    path,
                    s"recur arity mismatch: loop expects ${loopArgTypes.length} args, got ${args.length}"
                  )
              val argTypeChecks =
                loopArgTypes
                  .zip(args.toList)
                  .zipWithIndex
                  .foldLeft(Validated.validNec(Map.empty): TypeValidation[
                    SolvedFreeBounds
                  ]) { case (acc, ((expected, got), idx)) =>
                    acc.andThen { solvedSoFar =>
                      typeWidensToOrFailWithSolved(
                        expected,
                        got.getType,
                        kindOf,
                        inScopeKinds,
                        solvedSoFar,
                        path / "recur" / s"arg[$idx]",
                        s"recur arg type mismatch: expected ${expected.show}, got ${got.getType.show}"
                      )
                    }
                  }
              val recurTypeCheck =
                argTypeChecks.andThen { solvedAfterArgs =>
                  val solvedResultSubMap: Map[Type.Var, Type] =
                    solvedAfterArgs.iterator.collect {
                      case (b: Type.Var.Bound, solved) if inScopeKinds.contains(b) =>
                        (b: Type.Var) -> solved
                    }.toMap
                  val solvedLoopType = Type.substituteVar(loopType, solvedResultSubMap)
                  typeWidensToOrFail(
                    solvedLoopType,
                    tpe,
                    kindOf,
                    inScopeKinds,
                    path,
                    s"recur result type mismatch: expected ${solvedLoopType.show}, got ${tpe.show}"
                  )
                }
              (arityCheck :: recurTypeCheck :: Nil).sequence_
          }

        (recurChecks :: argChecks).sequence_

      case TypedExpr.Literal(lit, tpe, _) =>
        val expected = Type.getTypeOf(lit)
        if (tpe.sameAs(expected)) typeValidationPass
        else
          typeValidationFail(
            path,
            s"literal type mismatch: expected ${expected.show}, got ${tpe.show}"
          )

      case TypedExpr.Match(arg, branches, _) =>
        val argCheck = validateTypeConnections(
          arg,
          locals,
          globals,
          env,
          loopStack,
          inScopeKinds,
          path / "match" / "arg"
        )

        val branchChecks = branches.toList.zipWithIndex.map { case (branch, idx) =>
          val branchPath = path / "match" / s"branch[$idx]"
          val branchLocals = patternBindingTypes(
            arg.getType,
            branch.pattern,
            env,
            inScopeKinds,
            branchPath
          )

          val branchResultTypeCheck =
            typeWidensToOrFail(
              te.getType,
              branch.expr.getType,
              kindOf,
              inScopeKinds,
              branchPath,
              s"branch result type mismatch: expected ${te.getType.show}, got ${branch.expr.getType.show}"
            )

          branchLocals.andThen { localsFromPattern =>
            val scope = locals ++ localsFromPattern
            val guardCheck =
              branch.guard match {
                case None =>
                  typeValidationPass
                case Some(guard) =>
                  val guardTypeCheck =
                    if (guard.getType.sameAs(Type.BoolType)) typeValidationPass
                    else
                      typeValidationFail(
                        branchPath / "guard",
                        s"guard must be Bool, got ${guard.getType.show}"
                      )
                  (
                    guardTypeCheck ::
                      validateTypeConnections(
                        guard,
                        scope,
                        globals,
                        env,
                        loopStack,
                        inScopeKinds,
                        branchPath / "guard"
                      ) :: Nil
                  ).sequence_
              }

            val branchExprCheck = validateTypeConnections(
              branch.expr,
              scope,
              globals,
              env,
              loopStack,
              inScopeKinds,
              branchPath / "body"
            )
            (branchResultTypeCheck :: guardCheck :: branchExprCheck :: Nil).sequence_
          }
        }

        (argCheck :: branchChecks).sequence_
    }
  }

  private def validateLetList[A](
      pack: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[A])],
      fullTypeEnv: TypeEnv[Kind.Arg],
      globalValues: Map[(PackageName, Identifier), Type],
      stage: String
  ): TypeValidation[Unit] = {
    val localNames = lets.iterator.map { case (n, _, te) => (n, te.getType) }.toMap

    lets.map { case (name, _, te) =>
      val path = Chain.one(stage) / pack.asString / name.sourceCodeRepr
      (
        validateTypedExprInvariants(te, path) ::
          validateTypeConnections(
            te,
            localNames,
            globalValues,
            fullTypeEnv,
            Nil,
            Map.empty,
            path
          ) :: Nil
      ).sequence_
    }.sequence_
  }

  private def allTypeEnvOfPackageMap[A](
      pm: PackageMap.Typed[A]
  ): TypeEnv[Kind.Arg] =
    pm.toMap.valuesIterator.foldLeft(TypeEnv.empty: TypeEnv[Kind.Arg]) {
      (acc, pack) =>
        acc ++ pack.types
    }

  private def lowerPackageMap[A](
      pm: PackageMap.Typed[A]
  ): PackageMap.Typed[A] =
    PackageMap.fromIterable(
      pm.toMap.valuesIterator.map { pack =>
        val (prog, impMap) = pack.program
        val loweredLets = TypedExprLoopRecurLowering.lowerAll(prog.lets)
        pack.copy(program = (prog.copy(lets = loweredLets), impMap))
      }.toList
    )

  private def normalizePackageMap[A: cats.Eq](
      pm: PackageMap.Typed[A]
  ): PackageMap.Typed[A] = {
    val fullTypeEnv = allTypeEnvOfPackageMap(pm)
    PackageMap.fromIterable(
      pm.toMap.valuesIterator.map { pack =>
        val (prog, impMap) = pack.program
        val normalizedLets =
          TypedExprNormalization.normalizeAll(pack.name, prog.lets, fullTypeEnv)
        pack.copy(program = (prog.copy(lets = normalizedLets), impMap))
      }.toList
    )
  }

  def validateTypes[A](
      te: TypedExpr[A],
      names: Map[TypedExpr.Name[A], Type],
      env: TypeEnv[Kind.Arg]
  ): ValidatedNec[TypeValidationError, Unit] = {
    val localNames =
      names.iterator.collect { case (TypedExpr.Local(n, _, _), t) =>
        (n, t)
      }.toMap
    val globalNames =
      names.iterator.collect { case (TypedExpr.Global(p, n, _, _), t) =>
        ((p, n), t)
      }.toMap

    val path = Chain.one("typed-expr")
    (
      validateTypedExprInvariants(te, path) ::
        validateTypeConnections(
          te,
          localNames,
          globalNames,
          env,
          Nil,
          Map.empty,
          path
        ) :: Nil
    ).sequence_
  }

  def validatePackageMap[A](
      pm: PackageMap.Typed[A],
      stage: String = "package-map"
  ): ValidatedNec[TypeValidationError, Unit] = {
    val fullTypeEnv = allTypeEnvOfPackageMap(pm)
    val globalValues =
      globalValuesFromTypeEnv(fullTypeEnv) ++ pm.toMap.valuesIterator.flatMap {
        pack =>
          letGlobalValues(pack.name, pack.lets)
      }.toMap

    pm.toMap.valuesIterator.map { pack =>
      validateLetList(pack.name, pack.lets, fullTypeEnv, globalValues, stage)
    }.toList.sequence_
  }

  def assertPackageMapTypeConnections[A](
      pm: PackageMap.Typed[A],
      stage: String
  ): Unit =
    assertTypeValidationResult(stage, validatePackageMap(pm, stage))

  def assertRewritePipelineTypeConnections[A: cats.Eq](
      unoptimized: PackageMap.Typed[A]
  ): Unit = {
    assertPackageMapTypeConnections(unoptimized, "unoptimized")
    val lowered = lowerPackageMap(unoptimized)
    assertPackageMapTypeConnections(lowered, "loop/recur lowered")
    val normalized = normalizePackageMap(lowered)
    assertPackageMapTypeConnections(normalized, "typed expr normalization")
  }

  def normalizeWithRewriteValidation(
      pack: PackageName,
      fullTypeEnv: TypeEnv[Kind.Arg],
      unoptimized: Program[
        TypeEnv[Kind.Arg],
        TypedExpr[Declaration],
        List[Statement]
      ]
  ): Program[TypeEnv[Kind.Arg], TypedExpr[Declaration], List[Statement]] = {
    val unoptGlobals =
      globalValuesFromTypeEnv(fullTypeEnv) ++ letGlobalValues(pack, unoptimized.lets)
    assertTypeValidationResult(
      "unoptimized",
      validateLetList(pack, unoptimized.lets, fullTypeEnv, unoptGlobals, "unoptimized")
    )

    val loweredLets = TypedExprLoopRecurLowering.lowerAll(unoptimized.lets)
    val loweredGlobals =
      globalValuesFromTypeEnv(fullTypeEnv) ++ letGlobalValues(pack, loweredLets)
    assertTypeValidationResult(
      "loop/recur lowered",
      validateLetList(
        pack,
        loweredLets,
        fullTypeEnv,
        loweredGlobals,
        "loop/recur lowered"
      )
    )

    val normalizedLets =
      TypedExprNormalization.normalizeAll(pack, loweredLets, fullTypeEnv)
    val normalizedGlobals =
      globalValuesFromTypeEnv(fullTypeEnv) ++ letGlobalValues(pack, normalizedLets)
    assertTypeValidationResult(
      "typed expr normalization",
      validateLetList(
        pack,
        normalizedLets,
        fullTypeEnv,
        normalizedGlobals,
        "typed expr normalization"
      )
    )

    unoptimized.copy(lets = normalizedLets)
  }

}
