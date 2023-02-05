package org.bykn.bosatsu.rankn

import cats.Monad
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.implicits._

import org.bykn.bosatsu.{
  Expr,
  HasRegion,
  Identifier,
  Kind,
  PackageName,
  Pattern => GenPattern,
  Region,
  RecursionKind,
  TypedExpr,
  Variance}

import HasRegion.region

import Identifier.{Bindable, Constructor}

sealed abstract class Infer[+A] {
  import Infer.Error

  def run(env: Infer.Env): RefSpace[Either[Error, A]]

  final def flatMap[B](fn: A => Infer[B]): Infer[B] =
    Infer.Impl.FlatMap(this, fn)

  final def runVar(v: Map[Infer.Name, Type], tpes: Map[(PackageName, Constructor), Infer.Cons]): RefSpace[Either[Error, A]] =
    Infer.Env.init(v, tpes).flatMap(run(_))

  final def runFully(v: Map[Infer.Name, Type], tpes: Map[(PackageName, Constructor), Infer.Cons]): Either[Error, A] =
    runVar(v, tpes).run.value
}

object Infer {

  type Pattern = GenPattern[(PackageName, Constructor), Type]

  // Import our private implementation functions
  import Impl._

  implicit val inferMonad: Monad[Infer] =
    new Monad[Infer] {
      def pure[A](a: A) = Infer.pure(a)
      def flatMap[A, B](fa: Infer[A])(fn: A => Infer[B]): Infer[B] =
        fa.flatMap(fn)
      def tailRecM[A, B](a: A)(fn: A => Infer[Either[A, B]]): Infer[B] =
        TailRecM(a, fn)
    }


  /**
   * The first element of the tuple are the the bound type
   * vars for this type.
   * the next are the types of the args of the constructor
   * the final is the defined type this creates
   */
  type Cons = (List[(Type.Var, Kind.Arg)], List[Type], Type.Const.Defined)
  type Name = (Option[PackageName], Identifier)

  case class Env(
    uniq: Ref[Long],
    vars: Map[Name, Type],
    typeCons: Map[(PackageName, Constructor), Cons],
    variances: Map[Type.Const.Defined, List[Kind.Arg]]) {

    def addVars(vt: List[(Name, Type)]): Env =
      copy(vars = vt.foldLeft(vars)(_ + _))
  }

  object Env {
    def apply(uniq: Ref[Long],
      vars: Map[Name, Type],
      typeCons: Map[(PackageName, Constructor), Cons]): Env = {

      val variances = typeCons
        .iterator
        .map { case (_, (vs, _, c)) =>
          (c, vs.map(_._2))
        }
        .toMap
        // TODO, the typeCons don't require all these variances to match
      Env(uniq, vars, typeCons, variances)
    }

    def init(vars: Map[Name, Type], tpes: Map[(PackageName, Constructor), Cons]): RefSpace[Env] =
      RefSpace.newRef(0L).map(Env(_, vars, tpes))
  }

  def getEnv: Infer[Map[Name, Type]] = GetEnv

  def lift[A](rs: RefSpace[A]): Infer[A] =
    Lift(rs.map(Right(_)))

  def fail(err: Error): Infer[Nothing] =
    Lift(RefSpace.pure(Left(err)))

  def pure[A](a: A): Infer[A] =
    Lift(RefSpace.pure(Right(a)))

  val unit: Infer[Unit] = pure(())

  def require(b: Boolean, err: => Error): Infer[Unit] =
    if (b) unit else fail(err)

  // Fails if v is not in the env
  def lookupVarType(v: Name, reg: Region): Infer[Type] =
    getEnv.flatMap { env =>
      env.get(v) match {
        case None => fail(Error.VarNotInScope(v, env, reg))
        case Some(t) => pure(t)
      }
    }

  sealed abstract class Error {
    def message: String
  }
  object Error {

    /**
     * These are errors in the ability to type the code
     * Generally these cannot be caught by other phases
     */
    sealed abstract class TypeError extends Error

    case class NotUnifiable(left: Type, right: Type, leftRegion: Region, rightRegion: Region) extends TypeError {
      def message = {
        def tStr(t: Type): String = Type.fullyResolvedDocument.document(t).render(80)
        s"${tStr(left)} ($leftRegion) cannot be unified with ${tStr(right)} ($rightRegion)"
      }
    }

    case class NotPolymorphicEnough(tpe: Type, in: Expr[_], badTvs: NonEmptyList[Type.Var.Skolem], reg: Region) extends TypeError {
      def message = {
        val bads = badTvs.map(Type.TyVar(_))
        def tStr(t: Type): String = Type.fullyResolvedDocument.document(t).render(80)
        s"type ${tStr(tpe)} not polymorphic enough in $in, bad type variables: ${bads.map(tStr).toList.mkString(", ")}, at $reg"
      }
    }

    case class SubsumptionCheckFailure(inferred: Type, declared: Type, infRegion: Region, decRegion: Region, badTvs: NonEmptyList[Type.Var]) extends TypeError {
      def message = {
        val bads = badTvs.map(Type.TyVar(_))
        def tStr(t: Type): String = Type.fullyResolvedDocument.document(t).render(80)
        s"subsumption check failed: ${tStr(inferred)} ${tStr(declared)}, bad types: ${bads.map(tStr).toList.mkString(", ")}"
      }
    }

    /**
     * These are errors that prevent typing due to unknown names,
     * They could be caught in a phase that collects all the naming errors
     */
    sealed abstract class NameError extends Error

    // This could be a user error if we don't check scoping before typing
    case class VarNotInScope(varName: Name, vars: Map[Name, Type], region: Region) extends NameError {
      def message = s"$varName not in scope: ${vars.keys.toList.sorted}"
    }

    // This could be a user error if we don't check scoping before typing
    case class UnexpectedBound(v: Type.Var.Bound, in: Type, rb: Region, rt: Region) extends NameError {
      def message = s"unexpected bound ${v.name} at $rb in unification with $in at $rt"
    }

    case class UnknownConstructor(name: (PackageName, Constructor), region: Region, env: Env) extends NameError {
      def knownConstructors: List[(PackageName, Constructor)] = env.typeCons.keys.toList.sorted

      def message = s"unknown Constructor $name. Known: $knownConstructors"
    }

    case class UnionPatternBindMismatch(pattern: Pattern, names: List[List[Identifier.Bindable]]) extends NameError {
      def message = s"$pattern doesn't bind the same names in all union branches: $names"
    }

    /**
     * These can only happen if the compiler has bugs at some point
     */
    sealed abstract class InternalError extends Error
    case class UnexpectedMeta(m: Type.Meta, in: Type, left: Region, right: Region) extends InternalError {
      def message = s"meta $m at $left occurs in $in at $right and should not. This implies an infinite type."
    }

    // This is a logic error which should never happen
    case class InferIncomplete(method: String, term: Expr[_]) extends InternalError {
      def message = s"$method not complete for $term"
    }
    case class ExpectedRho(tpe: Type, context: String) extends InternalError {
      def message = s"expected $tpe to be a Type.Rho, at $context"
    }
  }


  /**
   * This is where the internal implementation goes.
   * It is here to make it easy to make one block private
   * and not do so on every little helper function
   */
  private object Impl {
    sealed abstract class Expected[A]
    object Expected {
      case class Inf[A](ref: Ref[Either[Error, A]]) extends Expected[A] {
        def set(a: A): Infer[Unit] =
          Infer.lift(ref.set(Right(a)))
      }
      case class Check[A](value: A) extends Expected[A]
    }

    case class FlatMap[A, B](fa: Infer[A], fn: A => Infer[B]) extends Infer[B] {
      def run(env: Env) =
        fa.run(env).flatMap {
          case Left(msg) => RefSpace.pure(Left(msg))
          case Right(a) => fn(a).run(env)
        }
    }
    case class TailRecM[A, B](init: A, fn: A => Infer[Either[A, B]]) extends Infer[B] {
      def run(env: Env) = {
        // RefSpace uses Eval so this is fine, if not maybe the fastest thing ever
        def loop(a: A): RefSpace[Either[Error, B]] =
          fn(a).run(env).flatMap {
            case Left(err) => RefSpace.pure(Left(err))
            case Right(Left(a)) => loop(a)
            case Right(Right(b)) => RefSpace.pure(Right(b))
          }
        loop(init)
      }
    }

    case object GetEnv extends Infer[Map[Name, Type]] {
      def run(env: Env) = RefSpace.pure(Right(env.vars))
    }

    case class GetDataCons(fqn: (PackageName, Constructor), reg: Region) extends Infer[Cons] {
      def run(env: Env) =
        RefSpace.pure(
          env.typeCons.get(fqn) match {
            case None =>
              Left(Error.UnknownConstructor(fqn, reg, env))
            case Some(res) =>
              Right(res)
          })
    }

    case object GetVarianceMap extends Infer[Map[Type.Const.Defined, List[Kind.Arg]]] {
      def run(env: Env) = RefSpace.pure(Right(env.variances))
    }

    case class ExtendEnvs[A](vt: List[(Name, Type)], in: Infer[A]) extends Infer[A] {
      def run(env: Env) = in.run(env.addVars(vt))
    }

    case class Lift[A](res: RefSpace[Either[Error, A]]) extends Infer[A] {
      def run(env: Env) = res
    }

    case object NextId extends Infer[Long] {
      def run(env: Env) =
        for {
          thisId <- env.uniq.get
          _ <- env.uniq.set(thisId + 1L)
        } yield Right(thisId)
    }

    def nextId: Infer[Long] = NextId

    def varianceOf(t: Type): Infer[Option[Variance]] = {
      import Type._
      def variances(vs: Map[Type.Const.Defined, List[Kind.Arg]], t: Type): Option[List[Variance]] =
        t match {
          case FnType => Some(Variance.contra :: Variance.co :: Nil)
          case TyApply(left, _) =>
            variances(vs, left).map(_.drop(1))
          case TyConst(defined@Const.Defined(_, _)) =>
            // TODO: don't ignore Kind
            vs.get(defined).map(_.map(_.variance))
          case TyVar(_) => None
          case TyMeta(_) =>
            // this is almost certainly a bug in this approach.
            // we will probably need a meta var for Variance as well
            // since inorder to infer this TyMeta, we may need to use
            // the variance
            None
          case ForAll(_, r) => variances(vs, r)
        }

      for {
        vs <- GetVarianceMap
        t1 <- zonkType(t) // fill in any known variances
      } yield (variances(vs, t1).flatMap(_.headOption))
    }

    // For two types that unify, check both variances
    def varianceOf2(t1: Type, t2: Type): Infer[Option[Variance]] =
      varianceOf(t1).flatMap {
        case s@Some(_) => pure(s)
        case None => varianceOf(t2)
      }
    /**
     * Skolemize on a function just recurses on the result type.
     *
     * Skolemize replaces ForAll parameters with skolem variables
     * and then skolemizes recurses on the substituted value
     *
     * otherwise we return the type.
     *
     * The returned type is in weak-prenex form: all ForAlls have
     * been floated up over covariant parameters
     */
    def skolemize(t: Type): Infer[(List[Type.Var.Skolem], Type.Rho)] =
      t match {
        case Type.ForAll(tvs, ty) =>
          // Rule PRPOLY
          for {
            sks1 <- tvs.traverse(newSkolemTyVar)
            sksT = sks1.map(Type.TyVar(_))
            sks2ty <- skolemize(substTyRho(tvs, sksT)(ty))
            (sks2, ty2) = sks2ty
          } yield (sks1.toList ::: sks2, ty2)
        case Type.TyApply(left, right) =>
          // Rule PRFUN
          varianceOf(left)
            .product(skolemize(left))
            .flatMap {
              case (Some(Variance.Covariant), (sksl, sl)) =>
                for {
                  skr <- skolemize(right)
                  (sksr, sr) = skr
                } yield (sksl ::: sksr, Type.TyApply(sl, sr))
              case (_, (sksl, sl)) =>
                // otherwise, we don't skolemize the right
                pure((sksl, Type.TyApply(sl, right)))
            }
        case other: Type.Rho =>
          // Rule PRMONO
          pure((Nil, other))
      }

    def getFreeTyVars(ts: List[Type]): Infer[Set[Type.Var]] =
      ts.traverse(zonkType).map(Type.freeTyVars(_).toSet)

    def zonk(m: Type.Meta): Infer[Option[Type.Rho]] =
      readMeta(m).flatMap {
        case None => pure(None)
        case Some(ty) =>
          Type.zonkRhoMeta(ty)(zonk(_)).flatMap { ty1 =>
            // short out multiple hops (I guess an optimization?)
            writeMeta(m, ty1).as(Some(ty1))
          }
      }

    /**
     * This fills in any meta vars that have been
     * quantified and replaces them with what they point to
     */
    def zonkType(t: Type): Infer[Type] =
      Type.zonkMeta(t)(zonk(_))

    def zonkTypedExpr[A](e: TypedExpr[A]): Infer[TypedExpr[A]] =
      TypedExpr.zonkMeta(e)(zonk(_))

    def initRef[A](err: Error): Infer[Ref[Either[Error, A]]] =
      lift(RefSpace.newRef[Either[Error, A]](Left(err)))

    def substTyRho(keys: NonEmptyList[Type.Var], vals: NonEmptyList[Type.Rho]): Type.Rho => Type.Rho = {
      val env = keys.toList.iterator.zip(vals.toList.iterator).toMap

      { t => Type.substituteRhoVar(t, env) }
    }

    def substTyExpr[A](keys: NonEmptyList[Type.Var], vals: NonEmptyList[Type.Rho], expr: TypedExpr[A]): TypedExpr[A] = {
      val fn = Type.substTy(keys, vals)
      expr.traverseType[cats.Id](fn)
    }

    /*
     * This asserts that a given Type must be a Rho. We have some
     * invariants we can't track with the type system, so we dynamically
     * check those here
     *
     * An alternative to calling this method is instantiate, which assigns
     * new meta variables for each bound variable in ForAll or skolemize
     * which replaces the ForAll variables with skolem variables
     */
    def assertRho(t: Type, context: => String): Infer[Type.Rho] =
      t match {
        case r: Type.Rho => pure(r)
        case _ => fail(Error.ExpectedRho(t, context))
      }

    /*
     * Return a Rho type (not a Forall), by assigning
     * new meta variables for each of the outer ForAll variables
     */
    def instantiate(t: Type): Infer[Type.Rho] =
      t match {
        case Type.ForAll(vars, ty) =>
          vars.traverse(_ => newMetaType)
            .map { vars1T =>
              substTyRho(vars, vars1T)(ty)
            }
        case rho: Type.Rho => pure(rho)
      }

    /*
     * Invariant: r2 needs to be in weak prenex form
     */
    def subsCheckFn(a1: Type, r1: Type, a2: Type, r2: Type.Rho, left: Region, right: Region): Infer[TypedExpr.Coerce] =
      // note due to contravariance in input, we reverse the order there
      for {
        coarg <- subsCheck(a2, a1, left, right)
        // r2 is already in weak-prenex form
        cores <- subsCheckRho(r1, r2, left, right)
      } yield TypedExpr.coerceFn(a1, r2, coarg, cores)

    /*
     * invariant: second argument is in weak prenex form, which means that all
     * the covariant positions have lifted the ForAlls out, e.g.
     * forall a. a -> (forall b. b -> b)
     * was rewritten to:
     * forall a, b. a -> (b -> b)
     */
    def subsCheckRho(t: Type, rho: Type.Rho, left: Region, right: Region): Infer[TypedExpr.Coerce] =
      (t, rho) match {
        case (fa@Type.ForAll(_, _), rho) =>
          // Rule SPEC
          instantiate(fa).flatMap(subsCheckRho(_, rho, left, right))
        case (rho1: Type.Rho, Type.Fun(a2, r2)) =>
          // Rule FUN
          for {
            a1r1 <- unifyFn(rho1, left, right)
            (a1, r1) = a1r1
            // since rho is in weak prenex form, and Fun is covariant on r2, we know
            // r2 is in weak-prenex form and a rho type
            rhor2 <- assertRho(r2, s"subsCheckRho($t, $rho, $left, $right), line 462")
            coerce <- subsCheckFn(a1, r1, a2, rhor2, left, right)
          } yield coerce
        case (Type.Fun(a1, r1), rho2) =>
          // Rule FUN
          for {
            a2r2 <- unifyFn(rho2, right, left)
            (a2, r2) = a2r2
            // since rho is in weak prenex form, and Fun is covariant on r2, we know
            // r2 is in weak-prenex form
            rhor2 <- assertRho(r2, s"subsCheckRho($t, $rho, $left, $right), line 471")
            coerce <- subsCheckFn(a1, r1, a2, rhor2, left, right)
          } yield coerce
        case (rho1: Type.Rho, Type.TyApply(l2, r2)) =>
          unifyTyApp(rho1, left, right).flatMap {
            case (l1, r1) =>
              val check2 = varianceOf2(l1, l2).flatMap {
                case Some(Variance.Covariant) =>
                  subsCheck(r1, r2, left, right).void
                case Some(Variance.Contravariant) =>
                  subsCheck(r2, r1, right, left).void
                case Some(Variance.Phantom) =>
                  // this doesn't matter
                  unit
                case None | Some(Variance.Invariant) =>
                  unifyType(r1, r2, left, right)
              }
              // should we coerce to t2? Seems like... but copying previous code
              (subsCheck(l1, l2, left, right) *> check2).as(TypedExpr.coerceRho(rho1))
          }
        case (ta@Type.TyApply(l1, r1), rho2) =>
          unifyTyApp(rho2, left, right).flatMap {
            case (l2, r2) =>
              val check2 = varianceOf2(l1, l2).flatMap {
                case Some(Variance.Covariant) =>
                  subsCheck(r1, r2, left, right).void
                case Some(Variance.Contravariant) =>
                  subsCheck(r2, r1, right, left).void
                case Some(Variance.Phantom) =>
                  // this doesn't matter
                  unit
                case None | Some(Variance.Invariant) =>
                  unifyType(r1, r2, left, right)
              }
              // should we coerce to t2? Seems like... but copying previous code
              (subsCheck(l1, l2, left, right) *> check2).as(TypedExpr.coerceRho(ta))
          }
        case (t1: Type.Rho, t2) =>
          // rule: MONO
          unify(t1, t2, left, right).as(TypedExpr.coerceRho(t1)) // TODO this coerce seems right, since we have unified
      }

    /*
     * Invariant: if the second argument is (Check rho) then rho is in weak prenex form
     */
    def instSigma(sigma: Type, expect: Expected[(Type.Rho, Region)], r: Region): Infer[TypedExpr.Coerce] =
      expect match {
        case Expected.Check((t, tr)) =>
          // note t is in weak-prenex form
          subsCheckRho(sigma, t, r, tr)
        case infer@Expected.Inf(_) =>
          for {
            rho <- instantiate(sigma)
            _ <- infer.set((rho, r))
          } yield TypedExpr.coerceRho(rho)
      }

    def unifyFn(fnType: Type.Rho, fnRegion: Region, evidenceRegion: Region): Infer[(Type, Type)] =
      fnType match {
        case Type.Fun(arg, res) => pure((arg, res))
        case tau =>
          for {
            argT <- newMetaType
            resT <- newMetaType
            _ <- unify(tau, Type.Fun(argT, resT), fnRegion, evidenceRegion)
          } yield (argT, resT)
      }

    def unifyTyApp(apType: Type.Rho, apRegion: Region, evidenceRegion: Region): Infer[(Type, Type)] =
      apType match {
        case Type.TyApply(left, right) => pure((left, right))
        case notApply =>
          for {
            leftT <- newMetaType
            rightT <- newMetaType
            _ <- unify(notApply, Type.TyApply(leftT, rightT), apRegion, evidenceRegion)
          } yield (leftT, rightT)
      }

    // invariant the flexible type variable tv1 is not bound
    def unifyUnboundVar(m: Type.Meta, ty2: Type.Tau, left: Region, right: Region): Infer[Unit] =
      ty2 match {
        case Type.TyMeta(m2) =>
          readMeta(m2).flatMap {
            case Some(ty2) => unify(Type.TyMeta(m), ty2, left, right)
            case None => writeMeta(m, ty2)
          }
        case nonMeta =>
          zonkType(nonMeta)
            .flatMap { nm2 =>
              val tvs2 = Type.metaTvs(nm2 :: Nil)
              if (tvs2(m)) fail(Error.UnexpectedMeta(m, nonMeta, left, right))
              else writeMeta(m, nonMeta)
            }
      }

    def unifyVar(tv: Type.Meta, t: Type.Tau, left: Region, right: Region): Infer[Unit] =
      readMeta(tv).flatMap {
        case None => unifyUnboundVar(tv, t, left, right)
        case Some(ty1) => unify(ty1, t, left, right)
      }

    def unify(t1: Type.Tau, t2: Type.Tau, r1: Region, r2: Region): Infer[Unit] =
      (t1, t2) match {
        case (Type.TyVar(b@Type.Var.Bound(_)), _) =>
          fail(Error.UnexpectedBound(b, t2, r1, r2))
        case (_, Type.TyVar(b@Type.Var.Bound(_))) =>
          fail(Error.UnexpectedBound(b, t1, r2, r1))
        // the only vars that should appear are skolem variables, we check here
        case (Type.TyVar(v1), Type.TyVar(v2)) if v1 == v2 => unit
        case (Type.TyMeta(m1), Type.TyMeta(m2)) if m1.id == m2.id => unit
        case (Type.TyMeta(m), tpe) => unifyVar(m, tpe, r1, r2)
        case (tpe, Type.TyMeta(m)) => unifyVar(m, tpe, r2, r1)
        case (Type.TyApply(a1, b1), Type.TyApply(a2, b2)) =>
          unifyType(a1, a2, r1, r2) *> unifyType(b1, b2, r1, r2)
        case (Type.TyConst(c1), Type.TyConst(c2)) if c1 == c2 => unit
        case (left, right) =>
          fail(Error.NotUnifiable(left, right, r1, r2))
      }

    /**
     * for a type to be unified, we mean we can substitute in either
     * direction
     */
    def unifyType(t1: Type, t2: Type, r1: Region, r2: Region): Infer[Unit] =
      (t1, t2) match {
        case (rho1: Type.Rho, rho2: Type.Rho) =>
          unify(rho1, rho2, r1, r2)
        case (t1, t2) =>
          subsCheck(t1, t2, r1, r2) *> subsCheck(t2, t1, r2, r1).void
      }

    /**
     * Allocate a new Meta variable which
     * will point to a Tau (no forall anywhere) type
     */
    def newMetaType: Infer[Type.TyMeta] =
      for {
        id <- nextId
        ref <- lift(RefSpace.newRef[Option[Type.Tau]](None))
      } yield Type.TyMeta(Type.Meta(id, ref))

    def newSkolemTyVar(tv: Type.Var.Bound): Infer[Type.Var.Skolem] =
      nextId.map(Type.Var.Skolem(tv.name, _))

    /**
     * See if the meta variable has been set with a Tau
     * type
     */
    def readMeta(m: Type.Meta): Infer[Option[Type.Tau]] =
      lift(m.ref.get)

    /**
     * Set the meta variable to point to a Tau type
     */
    def writeMeta(m: Type.Meta, v: Type.Tau): Infer[Unit] =
      lift(m.ref.set(Some(v)))

    // DEEP-SKOL rule
    // note, this is identical to subsCheckRho when declared is a Rho type
    def subsCheck(inferred: Type, declared: Type, left: Region, right: Region): Infer[TypedExpr.Coerce] =
      for {
        skolRho <- skolemize(declared)
        (skolTvs, rho2) = skolRho
        // note: we need rho2 in weak prenex form, but skolemize does this
        coerce <- subsCheckRho(inferred, rho2, left, right)
        // if there are no skolem variables, we can shortcut here, because empty.filter(fn) == empty
        res <- NonEmptyList.fromList(skolTvs) match {
          case None => pure(coerce)
          case Some(nel) =>
             getFreeTyVars(inferred :: declared :: Nil).flatMap { escTvs =>
               NonEmptyList.fromList(skolTvs.filter(escTvs)) match {
                 case None => pure(coerce.andThen(unskolemize(nel)))
                 case Some(badTvs) => fail(Error.SubsumptionCheckFailure(inferred, declared, left, right, badTvs))
               }
             }
          }
      } yield res

    /**
     * Invariant: if the second argument is (Check rho) then rho is in weak prenex form
     */
    def typeCheckRho[A: HasRegion](term: Expr[A], expect: Expected[(Type.Rho, Region)]): Infer[TypedExpr.Rho[A]] = {
      import Expr._

      term match {
        case Literal(lit, t) =>
          val tpe = Type.getTypeOf(lit)
          instSigma(tpe, expect, region(term)).map(_(TypedExpr.Literal(lit, tpe, t)))
        case Local(name, tag) =>
          for {
            vSigma <- lookupVarType((None, name), region(term))
            coerce <- instSigma(vSigma, expect, region(term))
           } yield coerce(TypedExpr.Local(name, vSigma, tag))
        case Global(pack, name, tag) =>
          for {
            vSigma <- lookupVarType((Some(pack), name), region(term))
            coerce <- instSigma(vSigma, expect, region(term))
           } yield coerce(TypedExpr.Global(pack, name, vSigma, tag))
        case App(fn, arg, tag) =>
           for {
             typedFn <- inferRho(fn)
             fnT = typedFn.getType
             fnTRho <- assertRho(fnT, s"must be rho since we inferRho($fn): on $typedFn")
             argRes <- unifyFn(fnTRho, region(fn), region(term))
             (argT, resT) = argRes
             typedArg <- checkSigma(arg, argT)
             coerce <- instSigma(resT, expect, region(term))
           } yield coerce(TypedExpr.App(typedFn, typedArg, resT, tag))
        case Lambda(name, result, tag) =>
          expect match {
            case Expected.Check((expTy, rr)) =>
              for {
                vb <- unifyFn(expTy, rr, region(term))
                // we know expTy is in weak-prenex form, and since Fn is covariant, bodyT must be
                // in weak prenex form
                (varT, bodyT) = vb
                bodyTRho <- assertRho(bodyT, s"expect a rho type in $vb from $expTy at $rr")
                typedBody <- extendEnv(name, varT) {
                    checkRho(result, bodyTRho)
                  }
              } yield TypedExpr.AnnotatedLambda(name, varT, typedBody, tag)
            case infer@Expected.Inf(_) =>
              for {
                varT <- newMetaType
                typedBody <- extendEnv(name, varT)(inferRho(result))
                bodyT = typedBody.getType
                _ <- infer.set((Type.Fun(varT, bodyT), region(term)))
              } yield TypedExpr.AnnotatedLambda(name, varT, typedBody, tag)
          }
        case AnnotatedLambda(name, tpe, result, tag) =>
          expect match {
            case Expected.Check((expTy, rr)) =>
              for {
                vb <- unifyFn(expTy, rr, region(term))
                // we know expTy is in weak-prenex form, and since Fn is covariant, bodyT must be
                // in weak prenex form
                (varT, bodyT) = vb
                bodyTRho <- assertRho(bodyT, s"expect a rho type in $vb from $expTy at $rr")
                typedBody <- extendEnv(name, varT) {
                    // TODO we are ignoring the result of subsCheck here
                    // should we be coercing a var?
                    //
                    // this comes from page 54 of the paper, but I can't seem to find examples
                    // where this will fail if we reverse (as we had for a long time), which
                    // indicates the testing coverage is incomplete
                    subsCheck(varT, tpe, region(term), rr) *>
                      // bodyTRho is in reach prenex form due to above
                      checkRho(result, bodyTRho)
                  }
              } yield TypedExpr.AnnotatedLambda(name, varT /* or tpe? */, typedBody, tag)
            case infer@Expected.Inf(_) =>
              for { // TODO do we need to narrow or instantiate tpe?
                typedBody <- extendEnv(name, tpe)(inferRho(result))
                bodyT = typedBody.getType
                _ <- infer.set((Type.Fun(tpe, bodyT), region(term)))
              } yield TypedExpr.AnnotatedLambda(name, tpe, typedBody, tag)
          }
        case Let(name, rhs, body, isRecursive, tag) =>
          if (isRecursive.isRecursive) {
            // all defs are marked at potentially recursive.
            // We have elsewhere checked that this is legitimate recursion,
            // here we are only typechecking. To typecheck a recursive let,
            // first allocate a metavariable to extend the environment before
            // typechecking the rhs.
            //
            // After we typecheck we see if this is truly recursive so
            // compilers/evaluation can possibly optimize non-recursive
            // cases differently
            newMetaType
              .flatMap { rhsTpe =>
                extendEnv(name, rhsTpe) {
                  for {
                    // the type variable needs to be unified with varT
                    // note, varT could be a sigma type, it is not a Tau or Rho
                    typedRhs <- inferSigmaMeta(rhs, Some((name, rhsTpe, region(rhs))))
                    varT = typedRhs.getType
                    // we need to overwrite the metavariable now with the full type
                    typedBody <- extendEnv(name, varT)(typeCheckRho(body, expect))
                    // TODO: a more efficient algorithm would do this top down
                    // for each top level TypedExpr and build it bottom up.
                    // we could do this after all typechecking is done
                    frees = TypedExpr.freeVars(typedRhs :: Nil)
                    isRecursive = RecursionKind.recursive(frees.contains(name))
                  } yield TypedExpr.Let(name, typedRhs, typedBody, isRecursive, tag)
                }
              }
          }
          else {
            // In this branch, we typecheck the rhs *without* name in the environment
            // so any recursion in this case won't typecheck, and shadowing rules are
            // in place
            for {
              typedRhs <- inferSigma(rhs)
              varT = typedRhs.getType
              typedBody <- extendEnv(name, varT)(typeCheckRho(body, expect))
            } yield TypedExpr.Let(name, typedRhs, typedBody, isRecursive, tag)
          }
        case Annotation(term, tpe, tag) =>
          for {
            typedTerm <- checkSigma(term, tpe)
            coerce <- instSigma(tpe, expect, region(term))
          } yield coerce(TypedExpr.Annotation(typedTerm, tpe, tag))
        case Match(term, branches, tag) =>
          // all of the branches must return the same type:

          // TODO: it's fishy that both branches have to
          // infer the type of term and then push it down
          // as a Check, if we only ever call check there, why accept an Expect?
          // The paper is not clear on this (they didn't implement this
          // method as far as I can see)
          // on the other hand, the patterns in some cases should be enough
          // to see the type of matching term, but we are only accessing that
          // via check currently.
          //
          // It feels like there should be another inference rule, which we
          // are missing here.

          expect match {
            case Expected.Check((resT, _)) =>
              for {
                tsigma <- inferSigma(term)
                tbranches <- branches.traverse { case (p, r) =>
                  // note, resT is in weak-prenex form, so this call is permitted
                  checkBranch(p, Expected.Check((tsigma.getType, region(term))), r, resT)
                }
              } yield TypedExpr.Match(tsigma, tbranches, tag)
            case infer@Expected.Inf(_) =>
              for {
                tsigma <- inferSigma(term)
                tbranches <- branches.traverse { case (p, r) =>
                  inferBranch(p, Expected.Check((tsigma.getType, region(term))), r)
                }
                resT = tbranches.map { case (_, te) => (te.getType, region(te)) }
                _ <- resT.flatMap { t0 => resT.map((t0, _)) }.traverse_ {
                  case (t0, t1) if t0 eq t1 => Infer.unit
                  // TODO
                  // we do N^2 subsCheck, which to coerce with, composed?
                  case ((t0, r0), (t1, r1)) => subsCheck(t0, t1, r0, r1)
                }
                // inferBranch returns TypedExpr.Rho, so this should be a rho type
                resTRho <- assertRho(resT.head._1, s"infer on match ${tbranches.head}")
                _ <- infer.set((resTRho, resT.head._2))
              } yield TypedExpr.Match(tsigma, tbranches, tag)
          }
      }
    }

    /*
     * we require resT in weak prenex form because we call checkRho with it
     */
    def checkBranch[A: HasRegion](p: Pattern, sigma: Expected.Check[(Type, Region)], res: Expr[A], resT: Type.Rho): Infer[(Pattern, TypedExpr[A])] =
      for {
        patBind <- typeCheckPattern(p, sigma, region(res))
        (pattern, bindings) = patBind
        tres <- extendEnvList(bindings)(checkRho(res, resT))
      } yield (pattern, tres)

    def inferBranch[A: HasRegion](p: Pattern, sigma: Expected.Check[(Type, Region)], res: Expr[A]): Infer[(Pattern, TypedExpr.Rho[A])] =
      for {
        patBind <- typeCheckPattern(p, sigma, region(res))
        (pattern, bindings) = patBind
        // inferRho returns a TypedExpr.Rho (which is only an alias)
        res <- extendEnvList(bindings)(inferRho(res))
      } yield (pattern, res)

    /**
     * patterns can be a sigma type, not neccesarily a rho/tau
     * return a list of bound names and their (sigma) types
     *
     * TODO: Pattern needs to have a region for each part
     */
    def typeCheckPattern(pat: Pattern, sigma: Expected.Check[(Type, Region)], reg: Region): Infer[(Pattern, List[(Bindable, Type)])] =
      pat match {
        case GenPattern.WildCard => Infer.pure((pat, Nil))
        case GenPattern.Literal(lit) =>
          val tpe = Type.getTypeOf(lit)
          val check = sigma match {
            case Expected.Check((t, tr)) => subsCheck(tpe, t, reg, tr)
          }
          check.as((pat, Nil))
        case GenPattern.Var(n) =>
          // We always return an annotation here, which is the only
          // place we need to be careful
          sigma match {
            case Expected.Check((t, _)) =>
              Infer.pure((GenPattern.Annotation(pat, t), List((n, t))))
          }
        case GenPattern.Named(n, p) =>
          def inner(pat: Pattern) =
            sigma match {
              case Expected.Check((t, _)) =>
                val res = (GenPattern.Annotation(GenPattern.Named(n, pat), t), t)
                Infer.pure(res)
            }
          // We always return an annotation here, which is the only
          // place we need to be careful
          for {
            pair0 <- typeCheckPattern(p, sigma, reg)
            (p0, ts0) = pair0
            pair1 <- inner(p0)
            (p1, t1) = pair1
          } yield (p1, (n, t1) :: ts0)
        case GenPattern.StrPat(items) =>
          val tpe = Type.StrType
          val check = sigma match {
            case Expected.Check((t, tr)) => subsCheck(tpe, t, reg, tr)
          }
          val names = items.collect {
            case GenPattern.StrPart.NamedStr(n) => (n, tpe)
          }
          // we need to apply the type so the names are well typed
          val anpat = GenPattern.Annotation(pat, tpe)
          check.as((anpat, names))
        case GenPattern.ListPat(items) =>
          import GenPattern.ListPart
          /*
           * Here we unify the sigma with List[A] for some type A
           * any *a patterns have type List[A], all the rest
           * of them have type A.
           */
          def checkItem(
            inner: Type,
            lst: Type,
            e: ListPart[Pattern]): Infer[(ListPart[Pattern], List[(Bindable, Type)])] =
              e match {
                case l@ListPart.WildList =>
                  // this is *a pattern that has list type, and binds that type to the name
                  Infer.pure((l, Nil))
                case l@ListPart.NamedList(splice) =>
                  // this is *a pattern that has list type, and binds that type to the name
                  Infer.pure((l, (splice, lst) :: Nil))
                case ListPart.Item(p) =>
                  // This is a non-splice
                  checkPat(p, inner, reg).map { case (p, l) => (ListPart.Item(p), l) }
              }
          for {
            tpeA <- newMetaType
            listA = Type.TyApply(Type.ListType, tpeA)
            _ <- instPatSigma(listA, sigma, reg)
            inners <- items.traverse(checkItem(tpeA, listA, _))
            innerPat = inners.map(_._1)
            innerBinds = inners.flatMap(_._2)
          } yield (GenPattern.Annotation(GenPattern.ListPat(innerPat), listA), innerBinds)

        case GenPattern.Annotation(p, tpe) =>
          // like in the case of an annotation, we check the type, then
          // instantiate a sigma type
          // checkSigma(term, tpe) *> instSigma(tpe, expect)
          for {
            patBind <- checkPat(p, tpe, reg)
            (p1, binds) = patBind
            _ <- instPatSigma(tpe, sigma, reg)
          } yield (p1, binds)
        case GenPattern.PositionalStruct(nm, args) =>
          for {
            paramRes <- instDataCon(nm, reg)
            (params, res) = paramRes
            // we need to do a pattern linting phase and probably error
            // if the pattern arity does not match the arity of the constructor
            // but we don't want to error type-checking since we want to show
            // the maximimum number of errors to the user
            envs <- args.zip(params).traverse { case (p, t) => checkPat(p, t, reg) }
            pats = envs.map(_._1)
            bindings = envs.map(_._2)
            _ <- instPatSigma(res, sigma, reg)
          } yield (GenPattern.PositionalStruct(nm, pats), bindings.flatten)
        case u@GenPattern.Union(h, t) =>
          (typeCheckPattern(h, sigma, reg), t.traverse(typeCheckPattern(_, sigma, reg)))
            .mapN { case ((h, binds), neList) =>
              val pat = GenPattern.Union(h, neList.map(_._1))
              val allBinds = NonEmptyList(binds, (neList.map(_._2).toList))
              identicalBinds(u, allBinds, reg).as((pat, binds))
            }
            .flatten
      }

    // Unions have to have identical bindings in all branches
    def identicalBinds(u: Pattern, binds: NonEmptyList[List[(Bindable, Type)]], reg: Region): Infer[Unit] =
      binds.map(_.map(_._1)) match {
        case NonEmptyList(h, t) =>
          val bs = h.toSet
          val rest = t.map(_.toSet)
          if (rest.forall(_ == bs)) {
            val bm = binds.map(_.toMap)
            bs.toList.traverse_ { v =>
              val bmh = bm.head
              val bmt = bm.tail
              val tpe = bmh(v)
              bmt.traverse_ { m2 =>
                val tpe2 = m2(v)
                unifyType(tpe, tpe2, reg, reg)
              }
            }
          }
          else fail(Error.UnionPatternBindMismatch(u, h :: t))
      }

    // TODO: we should be able to derive a region for any pattern
    def checkPat(pat: Pattern, sigma: Type, reg: Region): Infer[(Pattern, List[(Bindable, Type)])] =
      typeCheckPattern(pat, Expected.Check((sigma, reg)), reg)

    def instPatSigma(sigma: Type, exp: Expected.Check[(Type, Region)], sRegion: Region): Infer[Unit] =
      exp match {
        case Expected.Check((texp, tr)) => subsCheck(texp, sigma, tr, sRegion).void // this unit does not seem right
      }

    /**
     * To do this, Infer will need to know the names of the type
     * constructors in scope.
     *
     * Instantiation fills in all
     */
    def instDataCon(consName: (PackageName, Constructor), reg: Region): Infer[(List[Type], Type.Tau)] =
      GetDataCons(consName, reg).flatMap {
        case (Nil, consParams, tpeName) =>
          Infer.pure((consParams, Type.TyConst(tpeName)))
        case (v0 :: vs, consParams, tpeName) =>
          val vars0 = NonEmptyList(v0, vs)
          val vars = vars0.map(_._1) // TODO actually use the variance
          vars.traverse(_ => newMetaType)
            .map { vars1T =>
              val params1 = consParams.map(Type.substTy(vars, vars1T))
              val res = vars1T.foldLeft(Type.TyConst(tpeName): Type.Tau)(Type.TyApply(_, _))
              (params1, res)
            }
      }

    def inferSigma[A: HasRegion](e: Expr[A]): Infer[TypedExpr[A]] =
      inferSigmaMeta(e, None)

    def inferSigmaMeta[A: HasRegion](e: Expr[A], meta: Option[(Identifier, Type.TyMeta, Region)]): Infer[TypedExpr[A]] = {
      def unifySelf(tpe: Type.Rho): Infer[Map[Name, Type]] =
        meta match {
          case None => getEnv
          case Some((nm, m, r)) =>
            (unify(tpe, m, region(e), r) *> getEnv).map { envTys =>
              // we have to remove the recursive binding from the environment
              envTys - ((None, nm))
            }
        }

      for {
        rho <- inferRho(e)
        expTy = rho.getType
        expTyRho <- assertRho(expTy, s"must be rho since $rho is a TypedExpr.Rho")
        envTys <- unifySelf(expTyRho)
        q <- TypedExpr.quantify(envTys, rho, zonk(_), { (m, n) => writeMeta(m, Type.TyVar(n)) })
      } yield q
    }

    def checkSigma[A: HasRegion](t: Expr[A], tpe: Type): Infer[TypedExpr[A]] =
      for {
        skolRho <- skolemize(tpe)
        (skols, rho) = skolRho
        // we need rho in weak-prenex form, but skolemize does this
        te <- checkRho(t, rho)
        te1 <- NonEmptyList.fromList(skols) match {
          case None =>
            // if skols.isEmpty, skols.filter(fn).isEmpty, so we can skip the rest
            pure(te)
          case Some(neskols) =>
            for {
              envTys <- getEnv
              escTvs <- getFreeTyVars(tpe :: envTys.values.toList)
              badTvs = skols.filter(escTvs)
              _ <- require(badTvs.isEmpty, Error.NotPolymorphicEnough(tpe, t, NonEmptyList.fromListUnsafe(badTvs), region(t)))
              // we need to zonk before we unskolemize because some of the metas could be skolems
              zte <- zonkTypedExpr(te)
            } yield unskolemize(neskols)(zte)
        }
      } yield te1 // should be fine since the everything after te is just checking

    /**
     * invariant: rho needs to be in weak-prenex form
     */
    def checkRho[A: HasRegion](t: Expr[A], rho: Type.Rho): Infer[TypedExpr.Rho[A]] =
      typeCheckRho(t, Expected.Check((rho, region(t))))

    /**
     * recall a rho type never has a top level Forall
     */
    def inferRho[A: HasRegion](t: Expr[A]): Infer[TypedExpr.Rho[A]] =
      for {
        ref <- initRef[(Type.Rho, Region)](Error.InferIncomplete("inferRho", t))
        expr <- typeCheckRho(t, Expected.Inf(ref))
        _ <- lift(ref.reset) // we don't need this ref, and it does not escape, so reset
      } yield expr
  }

  def recursiveTypeCheck[A: HasRegion](name: Bindable, expr: Expr[A]): Infer[TypedExpr[A]] =
    newMetaType.flatMap { tpe =>
      extendEnv(name, tpe)(typeCheckMeta(expr, Some((name, tpe, region(expr)))))
    }


  def typeCheck[A: HasRegion](t: Expr[A]): Infer[TypedExpr[A]] =
    typeCheckMeta(t, None)

  private def unskolemize(skols: NonEmptyList[Type.Var.Skolem]): TypedExpr.Coerce =
    new FunctionK[TypedExpr, TypedExpr] {
      def apply[A](te: TypedExpr[A]) = {
        // now replace the skols with generics
        val used = Type.tyVarBinders(te.getType :: Nil)
        val aligned = Type.alignBinders(skols, used)
        val newVars = aligned.map(_._2)
        val te2 = substTyExpr(skols, newVars.map(Type.TyVar(_)), te)
        TypedExpr.forAll(newVars, te2)
      }
    }

  private def typeCheckMeta[A: HasRegion](t: Expr[A], optMeta: Option[(Identifier, Type.TyMeta, Region)]): Infer[TypedExpr[A]] = {
    def run(t: Expr[A]) = inferSigmaMeta(t, optMeta).flatMap(zonkTypedExpr _)
    /*
     * This is a deviation from the paper.
     * We are allowing a syntax like:
     *
     * def identity(x: a) -> a:
     *   x
     *
     * or:
     *
     * def foo(x: a): x
     *
     * We handle this by converting a to a skolem variable,
     * running inference, then quantifying over that skolem
     * variable.
     */
    Expr.skolemizeFreeVars(t)(newSkolemTyVar) match {
      case None => run(t)
      case Some(replace) =>
        for {
          mt <- replace
          (skols, t1) = mt
          te <- run(t1)
        } yield unskolemize(skols)(te)
    }
  }

  def extendEnv[A](varName: Bindable, tpe: Type)(of: Infer[A]): Infer[A] =
    extendEnvList(List((varName, tpe)))(of)

  def extendEnvList[A](bindings: List[(Bindable, Type)])(of: Infer[A]): Infer[A] =
    Infer.Impl.ExtendEnvs(bindings.map { case (n, t) => ((None, n), t) }, of)

  private def extendEnvPack[A](pack: PackageName, name: Bindable, tpe: Type)(of: Infer[A]): Infer[A] =
    Infer.Impl.ExtendEnvs(((Some(pack), name), tpe) :: Nil, of)

  /**
   * Packages are generally just lists of lets, this allows you to infer
   * the scheme for each in the context of the list
   */
  def typeCheckLets[A: HasRegion](pack: PackageName, ls: List[(Bindable, RecursionKind, Expr[A])]): Infer[List[(Bindable, RecursionKind, TypedExpr[A])]] =
    ls match {
      case Nil => Infer.pure(Nil)
      case (nm, rec, expr) :: tail =>
        for {
          te <- if (rec.isRecursive) recursiveTypeCheck(nm, expr) else typeCheck(expr)
          rest <- extendEnvPack(pack, nm, te.getType)(typeCheckLets(pack, tail))
        } yield (nm, rec, te) :: rest
    }

  /**
   * This is useful to testing purposes.
   *
   * Given types a and b, can we substitute
   * a for for b
   */
  def substitutionCheck(a: Type, b: Type, ra: Region, rb: Region): Infer[Unit] =
    subsCheck(a, b, ra, rb).void
}
