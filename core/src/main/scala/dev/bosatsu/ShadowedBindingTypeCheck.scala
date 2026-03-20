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

  private def checkExpr[A](
      expr: TypedExpr[A],
      env: Env,
      tctx: TypeContext,
      regionOf: A => Region
  ): Res[Unit] =
    expr match {
      case TypedExpr.Generic(quant, in) =>
        checkExpr(in, env, tctx.pushQuantification(quant), regionOf)
      case TypedExpr.Annotation(term, _, _) =>
        checkExpr(term, env, tctx, regionOf)
      case TypedExpr.AnnotatedLambda(args, body, tag) =>
        // Lambda/def boundaries start a fresh local scope for this lint.
        // Parameters are tracked inside the function body so local let/match/loop
        // rebinding is still checked against argument types.
        // Pattern-argument lowering introduces synthetic binders, so the
        // non-synthetic arguments are the user-facing lambda parameters even
        // on region-only compiled artifacts.
        val checkableArgs: List[(Bindable, Type)] =
          args.toList.filterNot { case (name, _) => Identifier.isSynthetic(name) }
        val (argCheck, lambdaEnv) =
          checkableArgs.foldLeft((unitValid, Map.empty[Bindable, BoundInfo])) {
            case ((acc, envAcc), (name, tpe)) =>
              val current = BoundInfo(
                tpe = tpe,
                canonicalTpe = tctx.canonicalize(tpe),
                region = regionOf(tag),
                site = BindingSite.LambdaArg
              )
              val nextAcc = acc *> checkBinding(envAcc, name, current)
              (nextAcc, addBinding(envAcc, name, current))
          }
        argCheck *> checkExpr(body, lambdaEnv, tctx, regionOf)
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        unitValid
      case TypedExpr.App(fn, args, _, _) =>
        checkExpr(fn, env, tctx, regionOf) *> args.traverse_(
          checkExpr(_, env, tctx, regionOf)
        )
      case TypedExpr.Let(arg, rhs, body, recursive, tag) =>
        val tpe = rhs.getType
        val current = BoundInfo(
          tpe = tpe,
          canonicalTpe = tctx.canonicalize(tpe),
          region = regionOf(tag),
          site = BindingSite.LetBinding
        )
        val bindCheck = checkBinding(env, arg, current)
        val rhsEnv =
          if (recursive.isRecursive) addBinding(env, arg, current)
          else env
        val rhsCheck = checkExpr(rhs, rhsEnv, tctx, regionOf)
        val bodyCheck =
          checkExpr(body, addBinding(env, arg, current), tctx, regionOf)
        bindCheck *> rhsCheck *> bodyCheck
      case TypedExpr.Loop(args, body, tag) =>
        val argChecks =
          args.traverse_ { case (_, init) => checkExpr(init, env, tctx, regionOf) }
        val loopBinds = args.toList.map { case (name, init) =>
          val tpe = init.getType
          (
            name,
            BoundInfo(
              tpe = tpe,
              canonicalTpe = tctx.canonicalize(tpe),
              region = regionOf(tag),
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
        argChecks *> bindCheck *> checkExpr(body, bodyEnv, tctx, regionOf)
      case TypedExpr.Recur(args, _, _) =>
        args.traverse_(checkExpr(_, env, tctx, regionOf))
      case TypedExpr.Match(arg, branches, _) =>
        val argCheck = checkExpr(arg, env, tctx, regionOf)
        val branchCheck = branches.traverse_ { branch =>
          val bindRegion = branch.patternRegion
          val patternBinds =
            patternEnv(branch.pattern).toList.sortBy(_._1.sourceCodeRepr)
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
          val guardCheck =
            branch.guard.traverse_(checkExpr(_, branchEnv, tctx, regionOf))
          val bodyCheck = checkExpr(branch.expr, branchEnv, tctx, regionOf)
          bindCheck *> guardCheck *> bodyCheck
        }
        argCheck *> branchCheck
    }

  def checkLets[A: HasRegion](
      pack: PackageName,
      lets: List[(Bindable, RecursionKind, TypedExpr[A])]
  ): Res[Unit] = {
    val _ = pack
    lets.traverse_ { case (name, recursive, expr) =>
      val recursiveEnv =
        if (recursive.isRecursive) {
          val tpe = expr.getType
          val top = BoundInfo(
            tpe = tpe,
            canonicalTpe = TypeContext.empty.canonicalize(tpe),
            region = HasRegion.region(expr.tag),
            site = BindingSite.TopLevel
          )
          addBinding(Map.empty, name, top)
        } else Map.empty[Bindable, BoundInfo]
      checkExpr(expr, recursiveEnv, TypeContext.empty, HasRegion.region[A])
    }
  }
}
