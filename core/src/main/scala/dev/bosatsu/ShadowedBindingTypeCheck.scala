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
      canonicalTpe: Type,
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

  private final case class TypeContext(
      renames: Map[Type.Var, Type.TyVar],
      nextId: Long
  ) {
    def pushQuantification(
        quant: TypedExpr.Quantification
    ): TypeContext = {
      val vars = quant.forallList ::: quant.existList
      vars.foldLeft(this) { case (ctx, (bound, _)) =>
        val fresh = Type.Var.Bound(s"_shadow_t${ctx.nextId}")
        TypeContext(
          ctx.renames.updated(bound, Type.TyVar(fresh)),
          ctx.nextId + 1L
        )
      }
    }

    def canonicalize(tpe: Type): Type =
      Type.substituteVar(tpe, renames)
  }
  private object TypeContext {
    val empty: TypeContext = TypeContext(Map.empty, 0L)
  }

  private val unitValid: Res[Unit] = Validated.valid(())

  private def addBinding(
      env: Env,
      name: Bindable,
      current: BoundInfo
  ): Env =
    if (Identifier.isSynthetic(name)) env
    else env.updated(name, current)

  private def checkBinding(
      env: Env,
      name: Bindable,
      current: BoundInfo
  ): Res[Unit] =
    if (Identifier.isSynthetic(name)) unitValid
    else
      env.get(name) match {
        case Some(previous) if !previous.canonicalTpe.sameAs(current.canonicalTpe) =>
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
      env: Env,
      tctx: TypeContext
  ): Res[Unit] =
    expr match {
      case TypedExpr.Generic(quant, in) =>
        checkExpr(in, env, tctx.pushQuantification(quant))
      case TypedExpr.Annotation(term, _, _) =>
        checkExpr(term, env, tctx)
      case TypedExpr.AnnotatedLambda(args, body, tag) =>
        // Pattern lambdas are desugared with synthetic lambda parameters that are
        // not source-visible names. Only direct variable parameters should
        // participate in shadow checks at the lambda-arg site.
        val sourceArgs = TypedExpr.sourceLambdaArgs(expr)
        val checkableArgs: List[(Bindable, Type)] =
          args.toList.zip(sourceArgs).collect {
            case ((name, tpe), sourceArg)
                if directLambdaParamName(sourceArg).contains(name) => (name, tpe)
          }
        val (argCheck, envWithArgs) =
          checkableArgs.foldLeft((unitValid, env)) {
            case ((acc, envAcc), (name, tpe)) =>
              val current = BoundInfo(
                tpe = tpe,
                canonicalTpe = tctx.canonicalize(tpe),
                region = tag.region,
                site = BindingSite.LambdaArg
              )
              val nextAcc = acc *> checkBinding(envAcc, name, current)
              (nextAcc, addBinding(envAcc, name, current))
          }
        argCheck *> checkExpr(body, envWithArgs, tctx)
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        unitValid
      case TypedExpr.App(fn, args, _, _) =>
        checkExpr(fn, env, tctx) *> args.traverse_(checkExpr(_, env, tctx))
      case TypedExpr.Let(arg, rhs, body, recursive, tag) =>
        val tpe = rhs.getType
        val current = BoundInfo(
          tpe = tpe,
          canonicalTpe = tctx.canonicalize(tpe),
          region = tag.region,
          site = BindingSite.LetBinding
        )
        val bindCheck = checkBinding(env, arg, current)
        val rhsEnv =
          if (recursive.isRecursive) addBinding(env, arg, current)
          else env
        val rhsCheck = checkExpr(rhs, rhsEnv, tctx)
        val bodyCheck = checkExpr(body, addBinding(env, arg, current), tctx)
        bindCheck *> rhsCheck *> bodyCheck
      case TypedExpr.Loop(args, body, tag) =>
        val argChecks = args.traverse_ { case (_, init) => checkExpr(init, env, tctx) }
        val loopBinds = args.toList.map { case (name, init) =>
          val tpe = init.getType
          (
            name,
            BoundInfo(
              tpe = tpe,
              canonicalTpe = tctx.canonicalize(tpe),
              region = tag.region,
              site = BindingSite.LoopBinding
            )
          )
        }
        val bindCheck =
          loopBinds.traverse_ { case (name, current) =>
            checkBinding(env, name, current)
          }
        val bodyEnv =
          loopBinds.foldLeft(env) { case (acc, (name, current)) =>
            addBinding(acc, name, current)
          }
        argChecks *> bindCheck *> checkExpr(body, bodyEnv, tctx)
      case TypedExpr.Recur(args, _, _) =>
        args.traverse_(checkExpr(_, env, tctx))
      case TypedExpr.Match(arg, branches, _) =>
        val argCheck = checkExpr(arg, env, tctx)
        val branchCheck = branches.traverse_ { branch =>
          val bindRegion = branch.expr.tag.region
          val patternBinds = patternEnv(branch.pattern).toList.sortBy(_._1.asString)
          val currentBinds = patternBinds.map { case (name, tpe) =>
            (
              name,
              BoundInfo(
                tpe = tpe,
                canonicalTpe = tctx.canonicalize(tpe),
                region = bindRegion,
                site = BindingSite.PatternBinding
              )
            )
          }
          val bindCheck = currentBinds.traverse_ { case (name, current) =>
            checkBinding(env, name, current)
          }
          val branchEnv =
            currentBinds.foldLeft(env) { case (acc, (name, current)) =>
              addBinding(acc, name, current)
            }
          val guardCheck = branch.guard.traverse_(checkExpr(_, branchEnv, tctx))
          val bodyCheck = checkExpr(branch.expr, branchEnv, tctx)
          bindCheck *> guardCheck *> bodyCheck
        }
        argCheck *> branchCheck
    }

  def checkLets(
      pack: PackageName,
      lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])]
  ): Res[Unit] = {
    val _ = pack
    lets.traverse_ { case (name, recursive, expr) =>
      val recursiveEnv =
        if (recursive.isRecursive) {
          val tpe = expr.getType
          val top = BoundInfo(
            tpe = tpe,
            canonicalTpe = TypeContext.empty.canonicalize(tpe),
            region = expr.tag.region,
            site = BindingSite.TopLevel
          )
          addBinding(Map.empty, name, top)
        } else Map.empty[Bindable, BoundInfo]
      checkExpr(expr, recursiveEnv, TypeContext.empty)
    }
  }
}
