package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import com.stripe.dagon.Memoize
import cats.Id
import cats.implicits._

sealed abstract class NormalExpression {

}

object NormalExpression {
  case class App(fn: NormalExpression, arg: NormalExpression) extends NormalExpression
  case class ExternalVar(pack: PackageName, defName: String) extends NormalExpression
  case class Match(arg: NormalExpression, branches: NonEmptyList[(Int, NormalExpression)]) extends NormalExpression
  case class LambdaVar(index: Int) extends NormalExpression
  case class Lambda(expr: NormalExpression) extends NormalExpression
  case class Struct(enum: Int, args: List[NormalExpression]) extends NormalExpression
  case class Literal(lit: Lit) extends NormalExpression
}

case class Normalization(pm: PackageMap.Inferred) {

  def normalizeLast(p: PackageName): Option[ResultingRef] = ???
  //  (for {
  //    pack <- pm.toMap.get(p)
  //    (_, expr) <- pack.program.lets.lastOption
  //  } yield {
  //    normalizeExpression(Package.asInferred(pack), expr)
  //  })

  // def normalizeProgram(p: Program[Int]) = {
  //  val newLets = 

  def normalizeExpression(pack: Package.Inferred, expr: Expr[(Declaration, Scheme)]): State[Map[(PackageName, String), ResultingRef], ResultingRef] = ???
  //  norm((pack, Right(expr), (Map.empty, Nil)))

  def normalizePackageMap(pm: PackageMap.Inferred): PackageMap.Normalized = {
    val emptyLetsMap = Map[(PackageName, String), ResultingRef]()
    val letsMap: Map[(PackageName, String), ResultingRef] = (pm.toMap.toList.map  { case (pn, pack) =>
      (pack.program.lets.map { case (name, expr) =>
        normalizeExpression(Package.asInferred(pack), expr).flatMap(ne => State.modify( lets => lets + ((pn,name) ->ne)))
      }).sequence
    }).sequence.run(emptyLetsMap).value._1
    type PackInf = Package.PackageF[Referant, Program[(Declaration, Scheme), Statement]]
    type PackNorm = Package.PackageF[Referant, Program[(Declaration, Scheme, NormalExpression), Statement]]
    def normalizePack(pack: PackInf): PackNorm = {
      val Package(pn, imports, _, program) = pack
      val lets = program.lets.map { case (name, _) =>
        name -> letsMap((pn, name))
      }
      val mappedImports = imports.map { case Import(iPack, items) =>
        val p = Fix[Lambda[a => Package[a, Referant, Referant, Program[(Declaration, Scheme, NormalExpression), Statement]]]](
          normalizePack(iPack.unfix)
        )
        Import(p, items)
      }
      pack.copy(program = program.copy(lets = lets), imports = mappedImports)
    }
    val toMap = pm.toMap.map { case (pn, pack) => (pn, normalizePack(pack)) }
    PackageMap(toMap)
  }

  private type Ref = Either[(String, (Declaration, Scheme)), Expr[(Declaration, Scheme)]]
  private type ResultingRef = Expr[(Declaration, Scheme, NormalExpression)]
  private type Env = (Map[String, NormalExpression], List[Option[String]])
  private def addNEToTag(tag: (Declaration, Scheme), ne: NormalExpression) = tag match { case(t1, t2) => (t1, t2, ne) }

  private def normExpr(
    p: Package.Inferred,
    expr: Expr[(Declaration, Scheme)],
    env: Env,
    recurse: ((Package.Inferred, Ref, Env)) => State[Map[(PackageName, String), ResultingRef], ResultingRef]
  ): State[Map[(PackageName, String), ResultingRef], ResultingRef] = {

    import NormalExpression._

    expr match {
      case Expr.Var(v, t) =>
        env._1.get(v) match {
          case Some(ne) => State.pure(Expr.Var(v, addNEToTag(t, ne)))
          case None => recurse((p, Left((v, t)), env))
        }
      case Expr.App(fn, arg, t) => {
        for {
          efn <- recurse((p, Right(fn), env))
          earg <- recurse((p, Right(arg), env))
        } yield {
          val ne = normalOrderReduction(App(efn.tag._3, earg.tag._3))
          Expr.App(efn, earg, addNEToTag(t, ne))
        }
      }
      case Expr.Lambda(name, expr, t) => {
        val lambdaVars = Some(name) :: env._2
        val nextEnv = (env._1 ++ lambdaVars.zipWithIndex.collect { case(Some(n), i) => (n,i) }.toMap.mapValues(LambdaVar(_)), lambdaVars)
        for {
          eExpr <- recurse((p, Right(expr), nextEnv))
        } yield {
          val ne = Lambda(eExpr.tag._3)
          Expr.Lambda(name, eExpr, addNEToTag(t, ne))
        }
      }
      case Expr.Let(arg, e, in, t) => {
        for {
          ee <- recurse((p, Right(e), env))
          nextEnv = (env._1 ++ Map(arg -> ee.tag._3), env._2)
          eIn <- recurse((p, Right(in), nextEnv)) // need a better solution here because eIn cannot be represented with an NE because it might contain unsubstituted variables
        } yield {
          val ne = eIn.tag._3
          Expr.Let(arg, ee, eIn, addNEToTag(t, ne)) 
        }
      }
      case Expr.Literal(lit, t) => State.pure(Expr.Literal(lit, addNEToTag(t, Literal(lit))))
      case Expr.Match(arg, branches, t) => {
        for {
          eArg <- recurse((p, Right(arg), env))
          scheme = arg.tag._2
          dtName = Type.rootDeclared(scheme.result).get
          dt = p.unfix.program.types.definedTypes
            .collectFirst { case (_, dtValue) if dtValue.name.asString == dtName.name => dtValue }.get
          enumLookup = dt.constructors.map(_._1.asString).zipWithIndex.toMap

          eBranches <- branches.map { case(pattern, e) => {
            val enum = enumLookup(pattern.typeName._2.asString)
            val lambdaVars = pattern.bindings.reverse ++ env._2
            val nextEnv = (env._1 ++ lambdaVars.zipWithIndex.collect { case(Some(n), i) => (n,i) }.toMap.mapValues(LambdaVar(_)), lambdaVars)
            for(ee <- recurse((p, Right(e), nextEnv))) yield (pattern, enum, ee)
          }}.sequence
        } yield {
          val ne = Match(eArg.tag._3, eBranches.map { case (p,i,b) => (i,b.tag._3) })
          Expr.Match(eArg, eBranches.map {case (p,i,b) => (p,b) }, addNEToTag(t, ne))
        }
      }
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val norm: ((Package.Inferred, Ref, Env)) => State[Map[(PackageName, String), ResultingRef], ResultingRef] =
    Memoize.function[(Package.Inferred, Ref, Env), State[Map[(PackageName, String), ResultingRef], ResultingRef]] {
      case ((pack, Right(expr), env), recurse) =>
        normExpr(pack, expr, env, recurse)
      case ((pack, Left((item, t)), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) => for {
            lookup <- State.inspect { lets: Map[(PackageName, String), ResultingRef] =>
              lets.get((pack.unfix.name, item))
            }
            res <- lookup match {
              case Some(res) => State.pure(res): State[Map[(PackageName, String), ResultingRef], ResultingRef]
              case None => for {
                res <- recurse((pack, Right(expr), env))
                _ <- State.modify { lets: Map[(PackageName, String), ResultingRef] => lets + ((pack.unfix.name, item) -> res)}
              } yield res

            }
          } yield Expr.Var(item, addNEToTag(t, res.tag._3))
          case NameKind.Constructor(cn, dt, schm) => {
            val ne = constructor(cn, dt)
            State.pure(Expr.Var(item, addNEToTag(t, ne)))
          }
          case NameKind.Import(from, orig) => {
            // we reset the environment in the other package
            recurse((from, Left((orig, t)), (Map.empty, Nil)))
          }
          case NameKind.ExternalDef(pn, n, scheme) => {
            val ne = NormalExpression.ExternalVar(pn, n)
            State.pure(Expr.Var(item, addNEToTag(t, ne)))
          }
        }
    }

  private def constructor(c: ConstructorName, dt: DefinedType): NormalExpression = {
    val (enum, arity) = dt.constructors
      .toList
      .iterator
      .zipWithIndex
      .collectFirst { case ((ctor, params), idx) if ctor == c => (idx, params.size) }
      .get

    import NormalExpression._

    def loop(params: Int, expr: NormalExpression): NormalExpression =
      if (params == 0) expr
      else loop(params - 1, Lambda(expr))

    loop(arity, Struct(enum, ((arity - 1) to 0 by -1).map(LambdaVar(_)).toList))
  }

  private def normalOrderReduction(expr: NormalExpression): NormalExpression = {
    import NormalExpression._
    val nextExpr = expr match {
      case App(Lambda(nextExpr), arg) => {
        applyLambdaSubstituion(nextExpr, arg, 0)
      }
      case App(fn, arg) => {
        val nextFn = normalOrderReduction(fn)
        App(nextFn, arg)
      }
      case _ => expr
    }
    nextExpr match {
      case al @ App(Lambda(_), _) => normalOrderReduction(al)
      case App(fn, arg) => {
        normalOrderReduction(fn) match {
          case l @ Lambda(_) => normalOrderReduction(App(l, arg))
          case nfn @ _ => App(nfn, normalOrderReduction(arg))
        }
      }
      case extVar @ ExternalVar(_, _) => extVar
      case Match(arg, branches) => Match(normalOrderReduction(arg), branches.map { case(enum, expr) => (enum, normalOrderReduction(expr)) })
      case lv @ LambdaVar(_) => lv
      case Lambda(expr) => Lambda(normalOrderReduction(expr))
      case Struct(enum, args) => Struct(enum, args.map(normalOrderReduction(_)))
      case l @ Literal(_) => l
    }
  }

  private def applyLambdaSubstituion(expr: NormalExpression, subst: NormalExpression, idx: Int): NormalExpression = {
    import NormalExpression._
    expr match {
      case App(fn, arg) => App(applyLambdaSubstituion(fn, subst, idx), applyLambdaSubstituion(arg, subst, idx))
      case ext @ ExternalVar(_, _) => ext
      case Match(arg, branches) => Match(applyLambdaSubstituion(arg, subst, idx), branches.map { case(enum, expr) => (enum, applyLambdaSubstituion(arg, subst, idx)) })
      case LambdaVar(varIndex) if varIndex == idx => subst
      case LambdaVar(varIndex) if varIndex > idx => LambdaVar(varIndex - 1)
      case lv @ LambdaVar(_) => lv
      case Lambda(fn) => Lambda(applyLambdaSubstituion(fn, subst, idx + 1))
      case Struct(enum, args) => Struct(enum, args.map(applyLambdaSubstituion(_, subst, idx)))
      case l @ Literal(_) => l
    }
  }
}
