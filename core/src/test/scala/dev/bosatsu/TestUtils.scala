package dev.bosatsu

import cats.Order
import cats.data.{Chain, Ior, NonEmptyList, Validated, ValidatedNec}
import java.nio.file.{Files, Paths}
import dev.bosatsu.rankn._
import dev.bosatsu.tool.Output
import munit.Assertions.{assertEquals, fail}
import Identifier.Constructor
import IorMethods.IorExtension

import cats.syntax.all._

object TestUtils {

  final case class TypeValidationError(path: String, message: String) {
    override def toString: String = s"$path: $message"
  }

  private type TypeValidation[+A] = ValidatedNec[TypeValidationError, A]
  private case class LoopContext(argTypes: List[Type], recurType: Type)

  private val typeValidationPass: TypeValidation[Unit] =
    Validated.validNec(())

  private def typeValidationFail(
      path: String,
      message: String
  ): TypeValidation[Unit] =
    Validated.invalidNec(TypeValidationError(path, message))

  private def showType(t: Type): String =
    Type.fullyResolvedDocument.document(t).render(80)

  private def typesConnectInScope(
      expected: Type,
      actual: Type,
      boundKinds: Map[Type.Var.Bound, Kind]
  ): Boolean =
    expected.sameAs(actual) || {
      def hoistResultForAll(t: Type): Type = {
        val (outerForalls, rho0) = Type.liftUniversals(t)

        @annotation.tailrec
        def loop(
            rho: Type.Rho,
            accRev: List[(Type.Var.Bound, Kind)]
        ): (List[(Type.Var.Bound, Kind)], Type.Rho) =
          Type.Fun.unapply(rho) match {
            case Some((args, Type.ForAll(vars, res))) =>
              val argFrees = Type.freeBoundTyVars(args.toList).toSet
              val (movable, blocked) =
                vars.toList.partition { case (b, _) => !argFrees(b) }
              NonEmptyList.fromList(movable) match {
                case None =>
                  (accRev.reverse, rho)
                case Some(movableNel) =>
                  val nextRes = NonEmptyList.fromList(blocked) match {
                    case Some(blockedNel) => Type.ForAll(blockedNel, res)
                    case None             => res
                  }
                  loop(
                    Type.Fun(args, nextRes),
                    movableNel.toList reverse_::: accRev
                  )
              }
            case _ =>
              (accRev.reverse, rho)
          }

        val (hoisted, rho1) = loop(rho0, Nil)
        Type.forAll(outerForalls ::: hoisted, rho1)
      }

      val expected1 = hoistResultForAll(expected)
      val actual1 = hoistResultForAll(actual)
      val inferredKinds =
        Type
          .freeBoundTyVars(expected1 :: actual1 :: Nil)
          .iterator
          .filterNot(boundKinds.contains)
          .map(_ -> Kind.Type)
          .toMap
      val envKinds = boundKinds ++ inferredKinds
      val (foralls, exists, fromRho) = Type.splitQuantifiers(expected1)
      val scopedFrees =
        Type.freeBoundTyVars(fromRho :: Nil).distinct.filter(envKinds.contains)
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
          toVars = Map.empty,
          to = actual1,
          env = envKinds
        )
        .isDefined
    }

  private def functionTailTypes(t: Type): List[Type] = {
    val (foralls, fromRho) = Type.liftUniversals(t)
    Type.Fun.unapply(fromRho) match {
      case None =>
        Nil
      case Some((args, result)) =>
        val argsList = args.toList
        (1 to argsList.length).toList.map { dropped =>
          val remaining = argsList.drop(dropped)
          val tailType = NonEmptyList.fromList(remaining) match {
            case Some(nel) => Type.Fun(nel, result)
            case None      => result
          }
          Type.forAll(foralls, tailType)
        }
    }
  }

  type SourceConvertedProgram = Program[
    (TypeEnv[Kind.Arg], ParsedTypeEnv[Option[Kind.Arg]]),
    Expr[Declaration],
    List[Statement]
  ]

  def sourceConvertedProgramOf(
      pack: PackageName,
      str: String
  ): SourceConvertedProgram = {
    val stmt = statementsOf(str)
    SourceConverter.toProgram(pack, Nil, stmt) match {
      case Ior.Right(prog)   => prog
      case Ior.Both(_, prog) => prog
      case Ior.Left(err)     => fail(s"source conversion failed: $err")
    }
  }

  def sourceConvertedProgramOf(str: String): SourceConvertedProgram =
    sourceConvertedProgramOf(testPackage, str)

  def parsedTypeEnvOf(
      pack: PackageName,
      str: String
  ): ParsedTypeEnv[Option[Kind.Arg]] =

    sourceConvertedProgramOf(pack, str).types._2

  val predefParsedTypeEnv: ParsedTypeEnv[Option[Kind.Arg]] = {
    val p = Package.predefPackage
    val prog = SourceConverter.toProgram(p.name, Nil, p.program) match {
      case Ior.Right(prog)   => prog
      case Ior.Both(_, prog) => prog
      case Ior.Left(err)     => sys.error(err.toString)
    }
    prog.types._2
  }

  def typeEnvOf(pack: PackageName, str: String): TypeEnv[Option[Kind.Arg]] =
    TypeEnv.fromParsed(parsedTypeEnvOf(pack, str))

  def statementsOf(str: String): List[Statement] =
    Parser.unsafeParse(Statement.parser, str)

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
        val msg = errs.toNonEmptyList.toList.map(_.toString).mkString("\n")
        fail(s"type connectivity validation failed at $stage:\n$msg")
    }

  private def validateTypedExprInvariants[A](
      te: TypedExpr[A],
      path: String
  ): TypeValidation[Unit] =
    Either
      .catchNonFatal(assertValid(te))
      .leftMap(err =>
        TypeValidationError(path, Option(err.getMessage).getOrElse(err.toString))
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
      path: String
  ): TypeValidation[Map[Identifier.Bindable, Type]] = {
    type Bindings = Map[Identifier.Bindable, Type]

    def failAt[A](at: String, message: String): TypeValidation[A] =
      Validated.invalidNec(TypeValidationError(at, message))

    def mergeBindings(
        at: String,
        left: Bindings,
        right: Bindings
    ): TypeValidation[Bindings] =
      right.toList.foldLeft(Validated.validNec(left): TypeValidation[Bindings]) {
        case (acc, (name, tpe)) =>
          acc.andThen { current =>
            current.get(name) match {
              case None =>
                Validated.validNec(current.updated(name, tpe))
              case Some(existing) if existing.sameAs(tpe) =>
                Validated.validNec(current)
              case Some(existing) =>
                failAt(
                  s"$at/${name.sourceCodeRepr}",
                  s"pattern binding type mismatch: existing ${showType(existing)}, new ${showType(tpe)}"
                )
            }
          }
      }

    def mergeAll(
        at: String,
        checks: List[TypeValidation[Bindings]]
    ): TypeValidation[Bindings] =
      checks.sequence.andThen { maps =>
        maps.foldLeft(Validated.validNec(Map.empty): TypeValidation[Bindings]) {
          case (acc, item) =>
            acc.andThen(mergeBindings(at, _, item))
        }
      }

    def listItemTypeOf(tpe: Type): Option[Type] =
      tpe match {
        case Type.ListT(item) => Some(item)
        case Type.ForAll(vars, Type.ListT(item)) if vars.tail.isEmpty =>
          Some(Type.forAll(vars, item))
        case _ => None
      }

    def constructorArgTypes(
        at: String,
        expectedType: Type,
        pack: PackageName,
        cons: Constructor
    ): TypeValidation[List[Type]] =
      env.getConstructor(pack, cons) match {
        case None =>
          failAt(
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
          val (_, _, rho) = Type.splitQuantifiers(pushedExpected)
          val (head, targs) = Type.unapplyAll(rho)
          if (!head.sameAs(dt.toTypeTyConst))
            failAt(
              at,
              s"constructor ${pack.asString}::${cons.sourceCodeRepr} does not match scrutinee type ${showType(expectedType)}"
            )
          else if (targs.length != dt.typeParams.length)
            failAt(
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
              cf.args.map(arg => Type.substituteVar(arg.tpe, subMap))
            )
          }
      }

    def loop(
        expectedType: Type,
        pattern: Pattern[(PackageName, Constructor), Type],
        at: String
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
              loop(expectedType, inner, s"$at/named") :: Nil
          )

        case Pattern.StrPat(parts) =>
          val strCheck =
            if (expectedType.sameAs(Type.StrType)) typeValidationPass
            else
              typeValidationFail(
                at,
                s"string pattern expects String scrutinee, got ${showType(expectedType)}"
              )
          val partBindings = parts.toList.collect {
            case Pattern.StrPart.NamedStr(name) =>
              Validated.validNec(Map(name -> Type.StrType))
            case Pattern.StrPart.NamedChar(name) =>
              Validated.validNec(Map(name -> Type.CharType))
          }
          val mergedParts = mergeAll(at, partBindings)
          (
            strCheck ::
              mergedParts.void :: Nil
          ).sequence_.andThen(_ => mergedParts)

        case Pattern.ListPat(parts) =>
          listItemTypeOf(expectedType) match {
            case None =>
              failAt(
                at,
                s"list pattern expects List scrutinee, got ${showType(expectedType)}"
              )
            case Some(itemType) =>
              val partChecks = parts.toList.zipWithIndex.map {
                case (Pattern.ListPart.WildList, _) =>
                  Validated.validNec(Map.empty)
                case (Pattern.ListPart.NamedList(name), _) =>
                  Validated.validNec(Map(name -> expectedType))
                case (Pattern.ListPart.Item(inner), idx) =>
                  loop(itemType, inner, s"$at/list/item[$idx]")
              }
              mergeAll(at, partChecks)
          }

        case Pattern.Annotation(inner, annType) =>
          loop(annType, inner, s"$at/annotation")

        case Pattern.PositionalStruct((pack, cons), args) =>
          constructorArgTypes(at, expectedType, pack, cons).andThen { inferredArgs =>
            val arityCheck =
              if (args.lengthCompare(inferredArgs.length) == 0) typeValidationPass
              else
                typeValidationFail(
                  at,
                  s"constructor arg arity mismatch for ${pack.asString}::${cons.sourceCodeRepr}: expected ${inferredArgs.length}, got ${args.length}"
                )

            val argChecks = args.toList.zipWithIndex.map { case (innerPat, idx) =>
              val argType = innerPat match {
                case Pattern.Annotation(_, annType) => annType
                case _                              =>
                  inferredArgs.lift(idx).getOrElse(expectedType)
              }
              loop(argType, innerPat, s"$at/arg[$idx]")
            }
            val mergedArgs = mergeAll(at, argChecks)

            (arityCheck :: mergedArgs.void :: Nil)
              .sequence_
              .andThen(_ => mergedArgs)
          }

        case Pattern.Union(head, rest) =>
          val branchChecks = (head :: rest.toList).zipWithIndex.map {
            case (innerPat, idx) =>
              loop(expectedType, innerPat, s"$at/union[$idx]")
          }
          branchChecks.sequence.andThen { maps =>
            val expectedNames = maps.headOption.map(_.keySet).getOrElse(Set.empty)
            val sameNameChecks = maps.zipWithIndex.drop(1).map { case (m, idx) =>
              if (m.keySet == expectedNames) typeValidationPass
              else
                typeValidationFail(
                  s"$at/union[$idx]",
                  s"union branch binders differ: expected ${expectedNames.map(_.sourceCodeRepr).toList.sorted.mkString(", ")}, got ${m.keySet.map(_.sourceCodeRepr).toList.sorted.mkString(", ")}"
                )
            }
            val mergedMaps =
              maps.foldLeft(Validated.validNec(Map.empty): TypeValidation[Bindings]) {
                case (acc, m) => acc.andThen(mergeBindings(at, _, m))
              }
            (sameNameChecks.sequence_ :: mergedMaps.void :: Nil)
              .sequence_
              .andThen(_ => mergedMaps)
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
      recursiveLocals: Set[Identifier.Bindable],
      path: String
  ): TypeValidation[Unit] =
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
              s"generic quantification type mismatch: expected ${showType(expected)}, got ${showType(te.getType)}"
            )
        (
          quantCheck ::
            validateTypeConnections(
              in,
              locals,
              globals,
              env,
              loopStack,
              boundKinds ++ quantKinds,
              recursiveLocals,
              s"$path/generic"
            ) :: Nil
        ).sequence_

      case TypedExpr.Annotation(term, tpe, qev) =>
        val evidenceCheck = qev match {
          case Some(ev) if !ev.targetAtSolve.sameAs(tpe) =>
            typeValidationFail(
              path,
              s"annotation evidence target mismatch: evidence=${showType(ev.targetAtSolve)}, annotation=${showType(tpe)}"
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
              boundKinds,
              recursiveLocals,
              s"$path/annotation"
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
          boundKinds,
          recursiveLocals,
          s"$path/lambda"
        )

      case TypedExpr.Local(name, tpe, _) =>
        locals.get(name) match {
          case Some(expected) =>
            val recursiveTailOk =
              recursiveLocals(name) &&
                functionTailTypes(expected)
                  .exists(typesConnectInScope(_, tpe, boundKinds))
            if (
              typesConnectInScope(expected, tpe, boundKinds) || recursiveTailOk
            ) typeValidationPass
            else
              typeValidationFail(
                path,
                s"local type mismatch for ${name.sourceCodeRepr}: expected ${showType(expected)}, got ${showType(tpe)}"
              )
          case None =>
            typeValidationFail(
              path,
              s"unbound local ${name.sourceCodeRepr} with type ${showType(tpe)}"
            )
        }

      case TypedExpr.Global(pack, name, tpe, _) =>
        globals.get((pack, name)).orElse(env.getValue(pack, name)) match {
          case Some(expected) =>
            if (typesConnectInScope(expected, tpe, boundKinds)) typeValidationPass
            else
              typeValidationFail(
                path,
                s"global type mismatch for ${pack.asString}::${name.sourceCodeRepr}: expected ${showType(expected)}, got ${showType(tpe)}"
              )
          case None =>
            typeValidationFail(
              path,
              s"unknown global ${pack.asString}::${name.sourceCodeRepr} with type ${showType(tpe)}"
            )
        }

      case TypedExpr.App(fn, args, _, _) =>
        val childChecks = (
          validateTypeConnections(
            fn,
            locals,
            globals,
            env,
            loopStack,
            boundKinds,
            recursiveLocals,
            s"$path/app/fn"
          ) :: args.toList.zipWithIndex.map { case (arg, idx) =>
            validateTypeConnections(
              arg,
              locals,
              globals,
              env,
              loopStack,
              boundKinds,
              recursiveLocals,
              s"$path/app/arg[$idx]"
            )
          }
        ).sequence_

        val fnTypeForCheck = fn match {
          case TypedExpr.Local(name, _, _) =>
            locals.get(name).getOrElse(fn.getType)
          case TypedExpr.Global(pack, name, _, _) =>
            globals
              .get((pack, name))
              .orElse(env.getValue(pack, name))
              .getOrElse(fn.getType)
          case _ =>
            fn.getType
        }

        val appTypeChecks: TypeValidation[Unit] = {
          @annotation.tailrec
          def consumeArgs(
              currentFnType: Type,
              remaining: Int,
              accRev: List[Type]
          ): Either[String, (List[Type], Type)] =
            if (remaining <= 0) Right((accRev.reverse, currentFnType))
            else {
              val (_, _, rho) = Type.splitQuantifiers(currentFnType)
              Type.Fun.unapply(rho) match {
                case Some((fnArgs, fnRes)) =>
                  val argsList = fnArgs.toList
                  if (argsList.lengthCompare(remaining) <= 0)
                    consumeArgs(
                      fnRes,
                      remaining - argsList.length,
                      argsList reverse_::: accRev
                    )
                  else {
                    val consumed = argsList.take(remaining)
                    val leftover = argsList.drop(remaining)
                    consumeArgs(
                      Type.Fun(NonEmptyList.fromListUnsafe(leftover), fnRes),
                      0,
                      consumed reverse_::: accRev
                    )
                  }
                case None =>
                  Left(
                    s"application head is not a function type: ${showType(currentFnType)}"
                  )
              }
            }

          consumeArgs(fnTypeForCheck, args.length, Nil) match {
            case Left(message) =>
              typeValidationFail(path, message)
            case Right((expectedArgTypes, expectedResult)) =>
              val gotArgs = args.toList
              val argTypeChecks =
                expectedArgTypes
                  .zip(gotArgs)
                  .zipWithIndex
                  .map { case ((expected, got), idx) =>
                    if (typesConnectInScope(expected, got.getType, boundKinds))
                      typeValidationPass
                    else
                      typeValidationFail(
                        s"$path/app/arg[$idx]",
                        s"app arg type mismatch: expected ${showType(expected)}, got ${showType(got.getType)}"
                      )
                  }
              val resultCheck =
                if (
                  expectedArgTypes.lengthCompare(gotArgs.length) == 0 &&
                    !typesConnectInScope(expectedResult, te.getType, boundKinds)
                )
                  typeValidationFail(
                    path,
                    s"application result type mismatch: expected ${showType(expectedResult)}, got ${showType(te.getType)}"
                  )
                else typeValidationPass

              (resultCheck :: argTypeChecks).sequence_
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
            boundKinds,
            if (rec.isRecursive) recursiveLocals + arg else recursiveLocals,
            s"$path/let/$arg/expr"
          ) ::
            validateTypeConnections(
              in,
              inLocals,
              globals,
              env,
              loopStack,
              boundKinds,
              if (rec.isRecursive) recursiveLocals + arg else recursiveLocals,
              s"$path/let/$arg/in"
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
            boundKinds,
            recursiveLocals,
            s"$path/loop/init[$idx]"
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
          boundKinds,
          recursiveLocals,
          s"$path/loop/body"
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
            boundKinds,
            recursiveLocals,
            s"$path/recur/arg[$idx]"
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
                  .map { case ((expected, got), idx) =>
                    if (typesConnectInScope(expected, got.getType, boundKinds))
                      typeValidationPass
                    else
                      typeValidationFail(
                        s"$path/recur/arg[$idx]",
                        s"recur arg type mismatch: expected ${showType(expected)}, got ${showType(got.getType)}"
                      )
                  }
              val recurTypeCheck =
                if (typesConnectInScope(loopType, tpe, boundKinds)) typeValidationPass
                else
                  typeValidationFail(
                    path,
                    s"recur result type mismatch: expected ${showType(loopType)}, got ${showType(tpe)}"
                  )
              (arityCheck :: recurTypeCheck :: argTypeChecks).sequence_
          }

        (recurChecks :: argChecks).sequence_

      case TypedExpr.Literal(lit, tpe, _) =>
        val expected = Type.getTypeOf(lit)
        if (tpe.sameAs(expected)) typeValidationPass
        else
          typeValidationFail(
            path,
            s"literal type mismatch: expected ${showType(expected)}, got ${showType(tpe)}"
          )

      case TypedExpr.Match(arg, branches, _) =>
        val argCheck = validateTypeConnections(
          arg,
          locals,
          globals,
          env,
          loopStack,
          boundKinds,
          recursiveLocals,
          s"$path/match/arg"
        )

        val branchChecks = branches.toList.zipWithIndex.map { case (branch, idx) =>
          val branchPath = s"$path/match/branch[$idx]"
          val branchLocals = patternBindingTypes(
            arg.getType,
            branch.pattern,
            env,
            branchPath
          )

          val branchResultTypeCheck =
            if (typesConnectInScope(te.getType, branch.expr.getType, boundKinds))
              typeValidationPass
            else
              typeValidationFail(
                branchPath,
                s"branch result type mismatch: expected ${showType(te.getType)}, got ${showType(branch.expr.getType)}"
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
                        s"$branchPath/guard",
                        s"guard must be Bool, got ${showType(guard.getType)}"
                      )
                  (
                    guardTypeCheck ::
                      validateTypeConnections(
                        guard,
                        scope,
                        globals,
                        env,
                        loopStack,
                        boundKinds,
                        recursiveLocals,
                        s"$branchPath/guard"
                      ) :: Nil
                  ).sequence_
              }

            val branchExprCheck = validateTypeConnections(
              branch.expr,
              scope,
              globals,
              env,
              loopStack,
              boundKinds,
              recursiveLocals,
              s"$branchPath/body"
            )
            (branchResultTypeCheck :: guardCheck :: branchExprCheck :: Nil).sequence_
          }
        }

        (argCheck :: branchChecks).sequence_
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
      val path = s"$stage/${pack.asString}/${name.sourceCodeRepr}"
      (
        validateTypedExprInvariants(te, path) ::
          validateTypeConnections(
            te,
            localNames,
            globalValues,
            fullTypeEnv,
            Nil,
            Map.empty,
            Set.empty,
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

    (
      validateTypedExprInvariants(te, "typed-expr") ::
        validateTypeConnections(
          te,
          localNames,
          globalNames,
          env,
          Nil,
          Map.empty,
          Set.empty,
          "typed-expr"
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

  private def normalizeWithRewriteValidation(
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

  val testPackage: PackageName = PackageName.parts("Test")

  def checkLast[A](
      statement: String
  )(fn: TypedExpr[Declaration] => A): A = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBodyUnopt(testPackage, Nil, stmts).strictToValidated match {
      case Validated.Invalid(errs) =>
        val lm = LocationMap(statement)
        val packMap = Map((testPackage, (lm, statement)))
        val msg = errs.toList
          .map { err =>
            err.message(packMap, LocationMap.Colorize.None)
          }
          .mkString("", "\n==========\n", "\n")
        sys.error("inference failure: " + msg)
      case Validated.Valid((fullTypeEnv, program)) =>
        val normalized =
          normalizeWithRewriteValidation(testPackage, fullTypeEnv, program)
        normalized.lets.foreach { case (_, _, te) => assertValid(te) }
        fn(normalized.lets.last._3)
    }
  }

  def checkPackageMap[A](
      statement: String
  )(
      fn: PackageMap.Typed[Declaration] => A
  ): A = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBodyUnopt(testPackage, Nil, stmts).strictToValidated match {
      case Validated.Invalid(errs) =>
        val lm = LocationMap(statement)
        val packMap = Map((testPackage, (lm, statement)))
        val msg = errs.toList
          .map { err =>
            err.message(packMap, LocationMap.Colorize.None)
          }
          .mkString("", "\n==========\n", "\n")
        sys.error("inference failure: " + msg)
      case Validated.Valid((fullTypeEnv, program)) =>
        val normalized =
          normalizeWithRewriteValidation(testPackage, fullTypeEnv, program)
        normalized.lets.foreach { case (_, _, te) => assertValid(te) }
        val pack: Package.Typed[Declaration] =
          Package(testPackage, Nil, Nil, (normalized, ImportMap.empty))
        val pm: PackageMap.Typed[Declaration] =
          PackageMap.empty + pack + PackageMap.predefCompiled
        assertPackageMapTypeConnections(pm, "optimized")
        fn(pm)
    }
  }

  def checkMatchless[A](
      statement: String
  )(
      fn: Map[
        PackageName,
        List[(Identifier.Bindable, Matchless.Expr[Unit])]
      ] => A
  ): A =
    checkPackageMap(statement) { pm =>
      Par.withEC {
        given Order[Unit] = Order.fromOrdering
        val comp = MatchlessFromTypedExpr.compile((), pm)
        fn(comp)
      }
    }

  def compileFile(path: String, rest: String*)(implicit
      ec: Par.EC
  ): PackageMap.Typed[Any] = {
    def toS(s: String): String =
      new String(Files.readAllBytes(Paths.get(s)), "UTF-8")

    val packNEL =
      NonEmptyList(path, rest.toList)
        .map { s =>
          val str = toS(s)
          val pack = Parser.unsafeParse(Package.parser(None), str)
          (("", LocationMap(str)), pack)
        }

    val res =
      PackageMap.typeCheckParsed(packNEL, Nil, "", CompileOptions.Default)
    res.left match {
      case Some(err) => sys.error(err.toString)
      case None      => ()
    }

    val pm = res.right.get
    assertPackageMapTypeConnections(pm, "optimized")
    pm
  }

  def makeInputArgs(files: List[(Chain[String], Any)]): List[String] =
    ("--package_root" :: "" :: Nil) ::: files.flatMap { case (idx, _) =>
      "--input" :: idx.iterator.mkString("/") :: Nil
    }

  private type ErrorOr[A] = Either[Throwable, A]
  private val module = MemoryMain[ErrorOr]

  def evalTest(packages: List[String], mainPackS: String, expected: Value) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "tool" :: "eval" :: "--main" :: mainPackS :: makeInputArgs(files)
    ) match {
      case Right(Output.EvaluationResult(got, _, gotDoc)) =>
        val gv = got.value
        assertEquals(
          gv,
          expected,
          s"${gotDoc.value.render(80)}\n\n$gv != $expected"
        )
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        module.mainExceptionToString(err) match {
          case Some(msg) => fail(msg)
          case None      => fail(s"got an exception: $err")
        }
    }
  }

  def evalTestJson(
      packages: List[String],
      mainPackS: String,
      expected: Json
  ) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "tool" :: "json" :: "write" :: "--main" :: mainPackS :: "--output" :: "-1" :: makeInputArgs(
        files
      )
    ) match {
      case Right(Output.JsonOutput(got, _)) =>
        assertEquals(got, expected, s"$got != $expected")
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        fail(s"got an exception: $err")
    }
  }

  def runBosatsuTest(
      packages: List[String],
      mainPackS: String,
      assertionCount: Int
  ) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "tool" :: "test" :: "--test_package" :: mainPackS :: makeInputArgs(
        files
      )
    ) match {
      case Right(Output.TestOutput(results, _)) =>
        results.collect { case (_, Some(t)) => t.value } match {
          case t :: Nil =>
            assertEquals(
              t.assertions,
              assertionCount,
              s"${t.assertions} != $assertionCount"
            )
            val Test.Report(_, failcount, message) =
              Test.report(t, LocationMap.Colorize.None)
            assertEquals(t.failures.map(_.assertions).getOrElse(0), failcount)
            if (failcount > 0) fail(message.render(80))
            else ()
          case other =>
            fail(s"expected exactly one test result, got: $other")
        }
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        module.mainExceptionToString(err) match {
          case Some(err) =>
            fail(err)
          case None =>
            err.printStackTrace
            fail(err.toString)
        }
    }
  }

  def testInferred(
      packages: List[String],
      mainPackS: String,
      inferredHandler: (PackageMap.Inferred, PackageName) => Unit
  )(implicit ec: Par.EC) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs)     => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          System.err.println(
            p.showContext(LocationMap.Colorize.None).render(80)
          )
        }
        sys.error("failed to parse") // errs.toString)
    }

    val fullParsed =
      PackageMap
        .withPredefA(("predef", LocationMap("")), parsedPaths)
        .map { case ((path, _), p) => (path, p) }

    PackageMap
      .resolveThenInfer(fullParsed, Nil, CompileOptions.NoOptimize)
      .strictToValidated match {
      case Validated.Valid(unoptimizedPackMap) =>
        assertRewritePipelineTypeConnections(unoptimizedPackMap)
      case Validated.Invalid(errs) =>
        val tes = errs.toList
          .collect { case te: PackageError.TypeErrorIn =>
            te.tpeErr.toString
          }
          .mkString("\n")
        fail(tes + "\n" + errs.toString)
    }

    PackageMap
      .resolveThenInfer(fullParsed, Nil, CompileOptions.Default)
      .strictToValidated match {
      case Validated.Valid(packMap) =>
        assertPackageMapTypeConnections(packMap, "optimized")
        inferredHandler(packMap, mainPack)

      case Validated.Invalid(errs) =>
        val tes = errs.toList
          .collect { case te: PackageError.TypeErrorIn =>
            te.tpeErr.toString
          }
          .mkString("\n")
        fail(tes + "\n" + errs.toString)
    }
  }

  def evalFail(
      packages: List[String]
  )(errFn: PartialFunction[PackageError, Unit])(implicit ec: Par.EC) = {

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs)     => vs
      case Validated.Invalid(errs) =>
        sys.error(s"parse fail: ${errs}")
    }

    // use parallelism to typecheck
    val withPre =
      PackageMap.withPredefA(("predef", LocationMap("")), parsedPaths)

    val withPrePaths = withPre.map { case ((path, _), p) => (path, p) }
    PackageMap
      .resolveThenInfer(withPrePaths, Nil, CompileOptions.Default)
      .left match {
      case None =>
        fail("expected to fail type checking")

      case Some(errs) if errs.collect(errFn).nonEmpty =>
        // make sure we can print the messages:
        val sm = PackageMap.buildSourceMap(withPre)
        errs.toList.foreach(_.message(sm, LocationMap.Colorize.None))
        assert(true)
      case Some(errs) =>
        fail(s"failed, but no type errors: $errs")
    }
  }

}
