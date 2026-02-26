package dev.bosatsu

import cats.data.ValidatedNec
import cats.data.Validated
import cats.syntax.all._

import Identifier.Bindable
import dev.bosatsu.rankn.Type

object ShadowedBindingTypeCheck {

  sealed abstract class BindingSite(val label: String)
  object BindingSite {
    case object TopLevel extends BindingSite("top-level binding")
    case object LambdaArg extends BindingSite("lambda argument")
    case object LetBinding extends BindingSite("let binding")
    case object LoopBinding extends BindingSite("loop binding")
    case object PatternBinding extends BindingSite("pattern binding")
  }

  final case class BoundInfo(
      tpe: Type,
      region: Region,
      site: BindingSite
  )

  final case class Error(
      name: Bindable,
      previous: BoundInfo,
      current: BoundInfo
  )

  type Res[+A] = ValidatedNec[Error, A]

  private type Env = Map[Bindable, BoundInfo]

  private val unitValid: Res[Unit] = Validated.valid(())

  private def addBinding(
      env: Env,
      name: Bindable,
      current: BoundInfo
  ): Env =
    env.updated(name, current)

  private def checkBinding(
      env: Env,
      name: Bindable,
      current: BoundInfo
  ): Res[Unit] =
    env.get(name) match {
      case Some(previous) if !previous.tpe.sameAs(current.tpe) =>
        Validated.invalidNec(Error(name, previous, current))
      case _ =>
        unitValid
    }

  private def patternEnv(
      pattern: Pattern[(PackageName, Identifier.Constructor), Type]
  ): Map[Bindable, Type] = {
    val fromPattern: Map[Identifier, Type] =
      Pattern.envOf(pattern, Map.empty[Identifier, Type])(identity)
    fromPattern.collect { case (b: Bindable, tpe) => (b, tpe) }
  }

  private def directLambdaParamName(
      pattern: Pattern.Parsed
  ): Option[Bindable] =
    pattern match {
      case Pattern.Var(name)           => Some(name)
      case Pattern.Annotation(inner, _) => directLambdaParamName(inner)
      case _                           => None
    }

  private def checkExpr(
      expr: TypedExpr[Declaration],
      env: Env
  ): Res[Unit] =
    expr match {
      case TypedExpr.Generic(_, in) =>
        checkExpr(in, env)
      case TypedExpr.Annotation(term, _, _) =>
        checkExpr(term, env)
      case TypedExpr.AnnotatedLambda(args, body, tag) =>
        // Pattern lambdas are desugared with synthetic lambda parameters that are
        // not source-visible names. Only direct variable parameters should
        // participate in shadow checks at the lambda-arg site.
        val checkableArgs: List[(Bindable, Type)] =
          tag match {
            case Declaration.Lambda(sourceArgs, _) =>
              args.toList.zip(sourceArgs.toList).collect {
                case ((name, tpe), sourceArg)
                    if directLambdaParamName(sourceArg).contains(name) =>
                  (name, tpe)
              }
            case _ =>
              args.toList
          }
        val (argCheck, envWithArgs) =
          checkableArgs.foldLeft((unitValid, env)) {
            case ((acc, envAcc), (name, tpe)) =>
            val current = BoundInfo(tpe, tag.region, BindingSite.LambdaArg)
            val nextAcc = (acc, checkBinding(envAcc, name, current)).mapN((_, _) => ())
            (nextAcc, addBinding(envAcc, name, current))
          }
        (argCheck, checkExpr(body, envWithArgs)).mapN((_, _) => ())
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        unitValid
      case TypedExpr.App(fn, args, _, _) =>
        (checkExpr(fn, env), args.traverse_(checkExpr(_, env))).mapN((_, _) => ())
      case TypedExpr.Let(arg, rhs, body, recursive, tag) =>
        val current = BoundInfo(rhs.getType, tag.region, BindingSite.LetBinding)
        val bindCheck = checkBinding(env, arg, current)
        val rhsEnv =
          if (recursive.isRecursive) addBinding(env, arg, current)
          else env
        val rhsCheck = checkExpr(rhs, rhsEnv)
        val bodyCheck = checkExpr(body, addBinding(env, arg, current))
        (bindCheck, rhsCheck, bodyCheck).mapN((_, _, _) => ())
      case TypedExpr.Loop(args, body, tag) =>
        val argChecks = args.traverse_ { case (_, init) => checkExpr(init, env) }
        val loopBinds = args.toList.map { case (name, init) =>
          (name, BoundInfo(init.getType, tag.region, BindingSite.LoopBinding))
        }
        val bindCheck =
          loopBinds.traverse_ { case (name, current) =>
            checkBinding(env, name, current)
          }
        val bodyEnv =
          loopBinds.foldLeft(env) { case (acc, (name, current)) =>
            addBinding(acc, name, current)
          }
        (argChecks, bindCheck, checkExpr(body, bodyEnv)).mapN((_, _, _) => ())
      case TypedExpr.Recur(args, _, _) =>
        args.traverse_(checkExpr(_, env))
      case TypedExpr.Match(arg, branches, _) =>
        val argCheck = checkExpr(arg, env)
        val branchCheck = branches.traverse_ { branch =>
          val bindRegion = branch.expr.tag.region
          val patternBinds = patternEnv(branch.pattern).toList.sortBy(_._1.asString)
          val currentBinds = patternBinds.map { case (name, tpe) =>
            (name, BoundInfo(tpe, bindRegion, BindingSite.PatternBinding))
          }
          val bindCheck = currentBinds.traverse_ { case (name, current) =>
            checkBinding(env, name, current)
          }
          val branchEnv =
            currentBinds.foldLeft(env) { case (acc, (name, current)) =>
              addBinding(acc, name, current)
            }
          val guardCheck = branch.guard.traverse_(checkExpr(_, branchEnv))
          val bodyCheck = checkExpr(branch.expr, branchEnv)
          (bindCheck, guardCheck, bodyCheck).mapN((_, _, _) => ())
        }
        (argCheck, branchCheck).mapN((_, _) => ())
    }

  def checkLets(
      pack: PackageName,
      lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])]
  ): Res[Unit] = {
    val _ = pack
    lets
      .foldLeft((Map.empty[Bindable, BoundInfo], unitValid)) {
        case ((env, acc), (name, recursive, expr)) =>
          val current =
            BoundInfo(expr.getType, expr.tag.region, BindingSite.TopLevel)
          val bindCheck = checkBinding(env, name, current)
          val exprEnv =
            if (recursive.isRecursive) addBinding(env, name, current)
            else env
          val nextAcc =
            (acc, bindCheck, checkExpr(expr, exprEnv)).mapN((_, _, _) => ())
          (addBinding(env, name, current), nextAcc)
      }
      ._2
  }
}
