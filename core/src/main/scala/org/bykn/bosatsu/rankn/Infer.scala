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
  Variance
}

import scala.collection.mutable.{Map => MMap}

import HasRegion.region

import Identifier.{Bindable, Constructor}

sealed abstract class Infer[+A] {
  import Infer.Error

  def run(env: Infer.Env): RefSpace[Either[Error, A]]

  final def flatMap[B](fn: A => Infer[B]): Infer[B] =
    Infer.Impl.FlatMap(this, fn)

  // Run but don't change any state
  def peek: Infer[Either[Error, A]] =
    Infer.Impl.Peek(this)

  final def mapEither[B](fn: A => Either[Error, B]): Infer[B] =
    Infer.Impl.MapEither(this, fn)

  final def runVar(
      v: Map[Infer.Name, Type],
      tpes: Map[(PackageName, Constructor), Infer.Cons],
      kinds: Map[Type.Const.Defined, Kind]
  ): RefSpace[Either[Error, A]] =
    Infer.Env.init(v, tpes, kinds).flatMap(run(_))

  final def runFully(
      v: Map[Infer.Name, Type],
      tpes: Map[(PackageName, Constructor), Infer.Cons],
      kinds: Map[Type.Const.Defined, Kind]
  ): Either[Error, A] =
    runVar(v, tpes, kinds).run.value
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

  /** The first element of the tuple are the the bound type vars for this type.
    * the next are the types of the args of the constructor the final is the
    * defined type this creates
    */
  type Cons = (List[(Type.Var.Bound, Kind.Arg)], List[Type], Type.Const.Defined)
  type Name = (Option[PackageName], Identifier)

  class Env(
      val uniq: Ref[Long],
      val vars: Map[Name, Type],
      val typeCons: Map[(PackageName, Constructor), Cons],
      val variances: Map[Type.Const.Defined, Kind]
  ) {

    def addVars(vt: List[(Name, Type)]): Env =
      new Env(uniq, vars = vars ++ vt, typeCons, variances)

    private[this] val kindCache: MMap[(Type, Map[Type.Var.Bound, Kind]), Either[
      Region => Error,
      Kind
    ]] =
      MMap()

    def getKind(t: Type, region: Region): Either[Error, Kind] = {
      def loop(
          item: Type,
          locals: Map[Type.Var.Bound, Kind]
      ): Either[Region => Error, Kind] =
        item match {
          case Type.TyVar(b @ Type.Var.Bound(_)) =>
            // don't cache locals, there is no point
            locals.get(b) match {
              case Some(k) => Right(k)
              case None    =>
                // $COVERAGE-OFF$ this should be unreachable because all vars should have a known kind
                Left({ region =>
                  Error.UnknownKindOfVar(t, region, s"unbound var: $b")
                })
              // $COVERAGE-ON$ this should be unreachable
            }
          case Type.TyVar(Type.Var.Skolem(_, kind, _)) => Right(kind)
          case Type.TyMeta(Type.Meta(kind, _, _))      => Right(kind)
          case Type.TyConst(const) =>
            val d = const.toDefined
            // some tests rely on syntax without importing
            // TODO remove this
            variances.get(d).orElse(Type.builtInKinds.get(d)) match {
              case Some(ks) => Right(ks)
              case None => Left({ region => Error.UnknownDefined(d, region) })
            }
          case _ =>
            kindCache.getOrElseUpdate(
              (item, locals),
              item match {
                case Type.ForAll(bound, t) =>
                  loop(t, locals ++ bound.toList)
                case ap @ Type.TyApply(left, right) =>
                  loop(left, locals)
                    .product(loop(right, locals))
                    .flatMap { case (leftKind, rhs) =>
                      Kind.validApply[Region => Error](
                        leftKind,
                        rhs,
                        { region =>
                          Error.KindCannotTyApply(ap, region)
                        }
                      ) { cons =>
                        { region =>
                          Error.KindInvalidApply(ap, cons, rhs, region)
                        }
                      }
                    }
                // $COVERAGE-OFF$ this should be unreachable because we handle Var above
                case _ =>
                  sys.error(s"reached unreachable: $item")
                // $COVERAGE-ON$
              }
            )
        }

      loop(t, Map.empty).leftMap(_(region))
    }
  }

  object Env {
    def init(
        vars: Map[Name, Type],
        tpes: Map[(PackageName, Constructor), Cons],
        kinds: Map[Type.Const.Defined, Kind]
    ): RefSpace[Env] =
      RefSpace.newRef(0L).map(new Env(_, vars, tpes, kinds))
  }

  def getEnv: Infer[Map[Name, Type]] = GetEnv.map(_.vars)

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
        case None    => fail(Error.VarNotInScope(v, env, reg))
        case Some(t) => pure(t)
      }
    }

  sealed abstract class Error

  object Error {

    /** These are errors in the ability to type the code Generally these cannot
      * be caught by other phases
      */
    sealed abstract class TypeError extends Error

    case class NotUnifiable(
        left: Type,
        right: Type,
        leftRegion: Region,
        rightRegion: Region
    ) extends TypeError
    case class KindNotUnifiable(
        leftK: Kind,
        leftT: Type,
        rightK: Kind,
        rightT: Type,
        leftRegion: Region,
        rightRegion: Region
    ) extends TypeError
    case class KindInvalidApply(
        typeApply: Type.TyApply,
        leftK: Kind.Cons,
        rightK: Kind,
        region: Region
    ) extends TypeError
    case class KindMetaMismatch(
        meta: Type.TyMeta,
        inferred: Type.Tau,
        inferredKind: Kind,
        metaRegion: Region,
        inferredRegion: Region
    ) extends TypeError
    case class KindCannotTyApply(ap: Type.TyApply, region: Region)
        extends TypeError
    case class UnknownDefined(tpe: Type.Const.Defined, region: Region)
        extends TypeError
    case class NotPolymorphicEnough(
        tpe: Type,
        in: Expr[_],
        badTvs: NonEmptyList[Type.Var.Skolem],
        reg: Region
    ) extends TypeError
    case class SubsumptionCheckFailure(
        inferred: Type,
        declared: Type,
        infRegion: Region,
        decRegion: Region,
        badTvs: NonEmptyList[Type.Var]
    ) extends TypeError
    // this sounds internal but can be due to an infinite type attempted to be defined
    case class UnexpectedMeta(
        m: Type.Meta,
        in: Type,
        left: Region,
        right: Region
    ) extends TypeError

    /** These are errors that prevent typing due to unknown names, They could be
      * caught in a phase that collects all the naming errors
      */
    sealed abstract class NameError extends Error

    // This could be a user error if we don't check scoping before typing
    case class VarNotInScope(
        varName: Name,
        vars: Map[Name, Type],
        region: Region
    ) extends NameError
    // This could be a user error if we don't check scoping before typing
    case class UnexpectedBound(
        v: Type.Var.Bound,
        in: Type,
        rb: Region,
        rt: Region
    ) extends NameError
    case class UnknownConstructor(
        name: (PackageName, Constructor),
        region: Region,
        env: Env
    ) extends NameError {
      def knownConstructors: List[(PackageName, Constructor)] =
        env.typeCons.keys.toList.sorted
    }
    case class UnionPatternBindMismatch(
        pattern: Pattern,
        names: NonEmptyList[List[Identifier.Bindable]],
        region: Region
    ) extends NameError

    /** These can only happen if the compiler has bugs at some point
      */
    sealed abstract class InternalError extends Error {
      def message: String
      def region: Region
    }
    // This is a logic error which should never happen
    case class InferIncomplete(method: String, term: Expr[_], region: Region)
        extends InternalError {
      // $COVERAGE-OFF$ we don't test these messages, maybe they should be removed
      def message = s"$method not complete for $term"
      // $COVERAGE-ON$ we don't test these messages, maybe they should be removed
    }
    case class ExpectedRho(tpe: Type, context: String, region: Region)
        extends InternalError {
      // $COVERAGE-OFF$ we don't test these messages, maybe they should be removed
      def message = s"expected $tpe to be a Type.Rho, at $context"
      // $COVERAGE-ON$ we don't test these messages, maybe they should be removed
    }

    case class UnknownKindOfVar(tpe: Type, region: Region, mess: String)
        extends InternalError {
      // $COVERAGE-OFF$ we don't test these messages, maybe they should be removed
      def message = s"unknown var in $tpe: $mess at $region"
      // $COVERAGE-ON$ we don't test these messages, maybe they should be removed
    }
  }

  /** This is where the internal implementation goes. It is here to make it easy
    * to make one block private and not do so on every little helper function
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
          case Right(a)  => fn(a).run(env)
        }
    }
    case class Peek[A](fa: Infer[A]) extends Infer[Either[Error, A]] {
      def run(env: Env) =
        fa.run(env).resetOnLeft(Left[Either[Error, A], Nothing](_)).map {
          case Left(res) => Right(res)
          // $COVERAGE-OFF$ this should be unreachable
          case Right(unreach) => unreach
          // $COVERAGE-ON$ this should be unreachable
        }
    }
    case class MapEither[A, B](fa: Infer[A], fn: A => Either[Error, B])
        extends Infer[B] {
      def run(env: Env) =
        fa.run(env).flatMap {
          case Left(msg) => RefSpace.pure(Left(msg))
          case Right(a)  => RefSpace.pure(fn(a))
        }
    }

    // $COVERAGE-OFF$ needed for Monad, but not actually used
    case class TailRecM[A, B](init: A, fn: A => Infer[Either[A, B]])
        extends Infer[B] {
      def run(env: Env) = {
        // RefSpace uses Eval so this is fine, if not maybe the fastest thing ever
        def loop(a: A): RefSpace[Either[Error, B]] =
          fn(a).run(env).flatMap {
            case Left(err)       => RefSpace.pure(Left(err))
            case Right(Left(a))  => loop(a)
            case Right(Right(b)) => RefSpace.pure(Right(b))
          }
        loop(init)
      }
    }
    // $COVERAGE-ON$

    case object GetEnv extends Infer[Env] {
      def run(env: Env) = RefSpace.pure(Right(env))
    }

    def GetDataCons(fqn: (PackageName, Constructor), reg: Region): Infer[Cons] =
      GetEnv.mapEither { env =>
        env.typeCons.get(fqn) match {
          case None =>
            Left(Error.UnknownConstructor(fqn, reg, env))
          case Some(res) =>
            Right(res)
        }
      }

    case class ExtendEnvs[A](vt: List[(Name, Type)], in: Infer[A])
        extends Infer[A] {
      def run(env: Env) = in.run(env.addVars(vt))
    }

    case class Lift[A](res: RefSpace[Either[Error, A]]) extends Infer[A] {
      def run(env: Env) = res
    }

    val nextId: Infer[Long] =
      GetEnv.flatMap { env =>
        Lift(
          for {
            thisId <- env.uniq.get
            _ <- env.uniq.set(thisId + 1L)
          } yield Right(thisId)
        )
      }

    def kindOf(t: Type, r: Region): Infer[Kind] =
      GetEnv.mapEither { env =>
        env.getKind(t, r)
      }

    private val checkedKinds: Infer[Type => Option[Kind]] = {
      val emptyRegion = Region(0, 0)
      GetEnv.map { env =>
        { tpe => env.getKind(tpe, emptyRegion).toOption }
      }
    }

    // on t[a] we know t: k -> *, what is the variance
    // in the arg a
    def varianceOfCons(ta: Type.TyApply, region: Region): Infer[Variance] =
      kindOf(ta.on, region)
        .flatMap(varianceOfConsKind(ta, _, region))

    def varianceOfConsKind(
        ta: Type.TyApply,
        k: Kind,
        region: Region
    ): Infer[Variance] =
      k match {
        case Kind.Cons(Kind.Arg(v, _), _) => pure(v)
        case Kind.Type =>
          fail(Error.KindCannotTyApply(ta, region))
      }

    /** Skolemize on a function just recurses on the result type.
      *
      * Skolemize replaces ForAll parameters with skolem variables and then
      * skolemizes recurses on the substituted value
      *
      * otherwise we return the type.
      *
      * The returned type is in weak-prenex form: all ForAlls have been floated
      * up over covariant parameters
      */
    private def skolemize(
        t: Type,
        region: Region
    ): Infer[(List[Type.Var.Skolem], Type.Rho)] =
      t match {
        case Type.ForAll(tvs, ty) =>
          // Rule PRPOLY
          for {
            sks1 <- tvs.traverse { case (b, k) => newSkolemTyVar(b, k) }
            sksT = sks1.map(Type.TyVar(_))
            sks2ty <- skolemize(substTyRho(tvs.map(_._1), sksT)(ty), region)
            (sks2, ty2) = sks2ty
          } yield (sks1.toList ::: sks2, ty2)
        case ta @ Type.TyApply(left, right) =>
          // Rule PRFUN
          // we know the kind of left is k -> x, and right has kind k
          varianceOfCons(ta, region)
            .product(skolemize(left, region))
            .flatMap {
              case (Variance.Covariant, (sksl, sl)) =>
                for {
                  skr <- skolemize(right, region)
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
            // note: this meta was already written, so we know
            // the kind must match
            writeMeta(m, ty1).as(Some(ty1))
          }
      }

    /** This fills in any meta vars that have been quantified and replaces them
      * with what they point to
      */
    def zonkType(t: Type): Infer[Type] =
      Type.zonkMeta(t)(zonk(_))

    def zonkTypedExpr[A](e: TypedExpr[A]): Infer[TypedExpr[A]] =
      TypedExpr.zonkMeta(e)(zonk(_))

    def initRef[A](err: Error): Infer[Ref[Either[Error, A]]] =
      lift(RefSpace.newRef[Either[Error, A]](Left(err)))

    def substTyRho(
        keys: NonEmptyList[Type.Var],
        vals: NonEmptyList[Type.Rho]
    ): Type.Rho => Type.Rho = {
      val env = keys.toList.iterator.zip(vals.toList.iterator).toMap

      { t => Type.substituteRhoVar(t, env) }
    }

    def substTyExpr[A](
        keys: NonEmptyList[Type.Var],
        vals: NonEmptyList[Type.Rho],
        expr: TypedExpr[A]
    ): TypedExpr[A] = {
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
    def assertRho(
        t: Type,
        context: => String,
        region: Region
    ): Infer[Type.Rho] =
      t match {
        case r: Type.Rho => pure(r)
        // $COVERAGE-OFF$ this should be unreachable
        case _ => fail(Error.ExpectedRho(t, context, region))
        // $COVERAGE-ON$ this should be unreachable
      }

    /*
     * Return a Rho type (not a Forall), by assigning
     * new meta variables for each of the outer ForAll variables
     */
    def instantiate(t: Type, region: Region): Infer[Type.Rho] =
      t match {
        case Type.ForAll(vars, rho) =>
          // TODO: it may be possible to improve type checking
          // by pushing foralls into covariant constructors
          // but it's not trivial
          vars
            .traverse { case (_, k) => newMetaType(k) }
            .map { vars1T =>
              substTyRho(vars.map(_._1), vars1T)(rho)
            }
        case rho: Type.Rho => pure(rho)
      }

    /*
     * Invariant: r2 needs to be in weak prenex form
     */
    def subsCheckFn(
        a1: Type,
        r1: Type,
        a2: Type,
        r2: Type.Rho,
        left: Region,
        right: Region
    ): Infer[TypedExpr.Coerce] =
      // note due to contravariance in input, we reverse the order there
      for {
        coarg <- subsCheck(a2, a1, left, right)
        // r2 is already in weak-prenex form
        cores <- subsCheckRho(r1, r2, left, right)
        ks <- checkedKinds
      } yield TypedExpr.coerceFn(a1, r2, coarg, cores, ks)

    /*
     * invariant: second argument is in weak prenex form, which means that all
     * the covariant positions have lifted the ForAlls out, e.g.
     * forall a. a -> (forall b. b -> b)
     * was rewritten to:
     * forall a, b. a -> (b -> b)
     */
    def subsCheckRho(
        t: Type,
        rho: Type.Rho,
        left: Region,
        right: Region
    ): Infer[TypedExpr.Coerce] =
      (t, rho) match {
        case (fa @ Type.ForAll(_, _), rho) =>
          // Rule SPEC
          instantiate(fa, left).flatMap(subsCheckRho(_, rho, left, right))
        case (rhot: Type.Rho, rho) =>
          subsCheckRho2(rhot, rho, left, right)
      }

    def subsCheckRho2(
        t: Type.Rho,
        rho: Type.Rho,
        left: Region,
        right: Region
    ): Infer[TypedExpr.Coerce] =
      // get the kinds to make sure they are well kindinded
      kindOf(t, left).product(kindOf(rho, right)) *>
        ((t, rho) match {
          case (rho1, Type.Fun(a2, r2)) =>
            // Rule FUN
            for {
              a1r1 <- unifyFn(rho1, left, right)
              (a1, r1) = a1r1
              // since rho is in weak prenex form, and Fun is covariant on r2, we know
              // r2 is in weak-prenex form and a rho type
              rhor2 <- assertRho(
                r2,
                s"subsCheckRho($t, $rho, $left, $right), line 462",
                right
              )
              coerce <- subsCheckFn(a1, r1, a2, rhor2, left, right)
            } yield coerce
          case (Type.Fun(a1, r1), rho2) =>
            // Rule FUN
            for {
              a2r2 <- unifyFn(rho2, right, left)
              (a2, r2) = a2r2
              // since rho is in weak prenex form, and Fun is covariant on r2, we know
              // r2 is in weak-prenex form
              rhor2 <- assertRho(
                r2,
                s"subsCheckRho($t, $rho, $left, $right), line 471",
                right
              )
              coerce <- subsCheckFn(a1, r1, a2, rhor2, left, right)
            } yield coerce
          case (rho1, ta @ Type.TyApply(l2, r2)) =>
            for {
              kl <- kindOf(l2, right)
              kr <- kindOf(r2, right)
              l1r1 <- unifyTyApp(rho1, kl, kr, left, right)
              (l1, r1) = l1r1
              _ <- varianceOfConsKind(ta, kl, right).flatMap {
                case Variance.Covariant =>
                  subsCheck(r1, r2, left, right).void
                case Variance.Contravariant =>
                  subsCheck(r2, r1, right, left).void
                case Variance.Phantom =>
                  // this doesn't matter
                  unit
                case Variance.Invariant =>
                  unifyType(r1, r2, left, right)
              }
              // should we coerce to t2? Seems like... but copying previous code
              _ <- subsCheck(l1, l2, left, right)
              ks <- checkedKinds
            } yield TypedExpr.coerceRho(rho1, ks)
          case (ta @ Type.TyApply(l1, r1), rho2) =>
            for {
              kl <- kindOf(l1, left)
              kr <- kindOf(r1, left)
              l2r2 <- unifyTyApp(rho2, kl, kr, left, right)
              (l2, r2) = l2r2
              _ <- varianceOfConsKind(ta, kl, left).flatMap {
                case Variance.Covariant =>
                  subsCheck(r1, r2, left, right).void
                case Variance.Contravariant =>
                  subsCheck(r2, r1, right, left).void
                case Variance.Phantom =>
                  // this doesn't matter
                  unit
                case Variance.Invariant =>
                  unifyType(r1, r2, left, right)
              }
              _ <- subsCheck(l1, l2, left, right)
              ks <- checkedKinds
              // should we coerce to t2? Seems like... but copying previous code
            } yield TypedExpr.coerceRho(ta, ks)
          case (t1, t2) =>
            // rule: MONO
            unify(t1, t2, left, right) *> checkedKinds.map(
              TypedExpr.coerceRho(t1, _)
            ) // TODO this coerce seems right, since we have unified
        })

    /*
     * Invariant: if the second argument is (Check rho) then rho is in weak prenex form
     */
    def instSigma(
        sigma: Type,
        expect: Expected[(Type.Rho, Region)],
        r: Region
    ): Infer[TypedExpr.Coerce] =
      expect match {
        case Expected.Check((t, tr)) =>
          // note t is in weak-prenex form
          subsCheckRho(sigma, t, r, tr)
        case infer @ Expected.Inf(_) =>
          for {
            rho <- instantiate(sigma, r)
            _ <- infer.set((rho, r))
            ks <- checkedKinds
          } yield TypedExpr.coerceRho(rho, ks)
      }

    def unifyFn(
        fnType: Type.Rho,
        fnRegion: Region,
        evidenceRegion: Region
    ): Infer[(Type, Type)] =
      fnType match {
        case Type.Fun(arg, res) => pure((arg, res))
        case tau =>
          for {
            argT <- newMetaType(Kind.Type)
            resT <- newMetaType(Kind.Type)
            _ <- unify(tau, Type.Fun(argT, resT), fnRegion, evidenceRegion)
          } yield (argT, resT)
      }

    def unifyKind(
        kind1: Kind,
        tpe1: Type,
        kind2: Kind,
        tpe2: Type,
        region1: Region,
        region2: Region
    ): Infer[Unit] =
      // we may need to be tracking kinds and widen them...
      if (
        Kind.leftSubsumesRight(kind1, kind2) || Kind.leftSubsumesRight(
          kind2,
          kind2
        )
      ) unit
      else
        fail(Error.KindNotUnifiable(kind1, tpe1, kind2, tpe2, region1, region2))

    private def checkApply[A](
        apType: Type.TyApply,
        lKind: Kind,
        rKind: Kind,
        apRegion: Region
    )(next: Infer[A]): Infer[A] =
      Kind.validApply[Error](
        lKind,
        rKind,
        Error.KindCannotTyApply(apType, apRegion)
      ) { cons =>
        Error.KindInvalidApply(apType, cons, rKind, apRegion)
      } match {
        case Right(_)  => next
        case Left(err) => fail(err)
      }

    def unifyTyApp(
        apType: Type.Rho,
        lKind: Kind,
        rKind: Kind,
        apRegion: Region,
        evidenceRegion: Region
    ): Infer[(Type, Type)] =
      apType match {
        case ap @ Type.TyApply(left, right) =>
          checkApply(ap, lKind, rKind, apRegion)(pure((left, right)))
        case notApply =>
          for {
            leftT <- newMetaType(lKind)
            rightT <- newMetaType(rKind)
            ap = Type.TyApply(leftT, rightT)
            _ <- checkApply(ap, lKind, rKind, apRegion) {
              unify(notApply, ap, apRegion, evidenceRegion)
            }
          } yield (leftT, rightT)
      }

    // invariant the flexible type variable tv1 is not bound
    def unifyUnboundVar(
        m: Type.Meta,
        ty2: Type.Tau,
        left: Region,
        right: Region
    ): Infer[Unit] =
      ty2 match {
        case meta2 @ Type.TyMeta(m2) =>
          readMeta(m2).flatMap {
            case Some(ty2) => unify(Type.TyMeta(m), ty2, left, right)
            case None =>
              if (Kind.leftSubsumesRight(m.kind, m2.kind)) {
                // we have to check that the kind matches before writing to a meta
                writeMeta(m, ty2)
              } else {
                fail(
                  Error.KindMetaMismatch(
                    Type.TyMeta(m),
                    meta2,
                    m2.kind,
                    left,
                    right
                  )
                )
              }
          }
        case nonMeta =>
          zonkType(nonMeta)
            .flatMap { nm2 =>
              val tvs2 = Type.metaTvs(nm2 :: Nil)
              if (tvs2(m)) fail(Error.UnexpectedMeta(m, nonMeta, left, right))
              else {
                kindOf(nonMeta, right)
                  .flatMap { nmk =>
                    if (Kind.leftSubsumesRight(m.kind, nmk)) {
                      // we have to check that the kind matches before writing to a meta
                      writeMeta(m, nonMeta)
                    } else {
                      fail(
                        Error.KindMetaMismatch(
                          Type.TyMeta(m),
                          nonMeta,
                          nmk,
                          left,
                          right
                        )
                      )
                    }
                  }
              }
            }
      }

    def unifyVar(
        tv: Type.Meta,
        t: Type.Tau,
        left: Region,
        right: Region
    ): Infer[Unit] =
      readMeta(tv).flatMap {
        case None      => unifyUnboundVar(tv, t, left, right)
        case Some(ty1) => unify(ty1, t, left, right)
      }

    def unify(t1: Type.Tau, t2: Type.Tau, r1: Region, r2: Region): Infer[Unit] =
      (t1, t2) match {
        case (Type.TyMeta(m1), Type.TyMeta(m2)) if m1.id == m2.id => unit
        case (Type.TyMeta(m), tpe) => unifyVar(m, tpe, r1, r2)
        case (tpe, Type.TyMeta(m)) => unifyVar(m, tpe, r2, r1)
        case (Type.TyApply(a1, b1), Type.TyApply(a2, b2)) =>
          unifyType(a1, a2, r1, r2) *> unifyType(b1, b2, r1, r2)
        case (Type.TyConst(c1), Type.TyConst(c2)) if c1 == c2 => unit
        case (Type.TyVar(v1), Type.TyVar(v2)) if v1 == v2     => unit
        case (Type.TyVar(b @ Type.Var.Bound(_)), _) =>
          fail(Error.UnexpectedBound(b, t2, r1, r2))
        case (_, Type.TyVar(b @ Type.Var.Bound(_))) =>
          fail(Error.UnexpectedBound(b, t1, r2, r1))
        case (left, right) =>
          fail(Error.NotUnifiable(left, right, r1, r2))
      }

    /** for a type to be unified, we mean we can substitute in either direction
      */
    def unifyType(t1: Type, t2: Type, r1: Region, r2: Region): Infer[Unit] =
      (t1, t2) match {
        case (rho1: Type.Rho, rho2: Type.Rho) =>
          unify(rho1, rho2, r1, r2)
        case (t1, t2) =>
          subsCheck(t1, t2, r1, r2) *> subsCheck(t2, t1, r2, r1).void
      }

    /** Allocate a new Meta variable which will point to a Tau (no forall
      * anywhere) type
      */
    def newMetaType(kind: Kind): Infer[Type.TyMeta] =
      for {
        id <- nextId
        ref <- lift(RefSpace.newRef[Option[Type.Tau]](None))
        meta = Type.Meta(kind, id, ref)
      } yield Type.TyMeta(meta)

    // TODO: it would be nice to support kind inference on skolem variables
    def newSkolemTyVar(tv: Type.Var.Bound, kind: Kind): Infer[Type.Var.Skolem] =
      nextId.map(Type.Var.Skolem(tv.name, kind, _))

    /** See if the meta variable has been set with a Tau type
      */
    def readMeta(m: Type.Meta): Infer[Option[Type.Tau]] =
      lift(m.ref.get)

    /** Set the meta variable to point to a Tau type
      */
    private def writeMeta(m: Type.Meta, v: Type.Tau): Infer[Unit] =
      lift(m.ref.set(Some(v)))

    // DEEP-SKOL rule
    // note, this is identical to subsCheckRho when declared is a Rho type
    def subsCheck(
        inferred: Type,
        declared: Type,
        left: Region,
        right: Region
    ): Infer[TypedExpr.Coerce] =
      for {
        skolRho <- skolemize(declared, right)
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
                case Some(badTvs) =>
                  fail(
                    Error.SubsumptionCheckFailure(
                      inferred,
                      declared,
                      left,
                      right,
                      badTvs
                    )
                  )
              }
            }
        }
      } yield res

    /** Invariant: if the second argument is (Check rho) then rho is in weak
      * prenex form
      */
    def typeCheckRho[A: HasRegion](
        term: Expr[A],
        expect: Expected[(Type.Rho, Region)]
    ): Infer[TypedExpr.Rho[A]] = {
      import Expr._

      term match {
        case Literal(lit, t) =>
          val tpe = Type.getTypeOf(lit)
          instSigma(tpe, expect, region(term)).map(
            _(TypedExpr.Literal(lit, tpe, t))
          )
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
            fnTRho <- assertRho(
              fnT,
              s"must be rho since we inferRho($fn): on $typedFn",
              region(fn)
            )
            argRes <- unifyFn(fnTRho, region(fn), region(term))
            (argT, resT) = argRes
            typedArg <- checkSigma(arg, argT)
            coerce <- instSigma(resT, expect, region(term))
          } yield coerce(TypedExpr.App(typedFn, typedArg, resT, tag))
        case Generic(tpes, in) =>
          for {
            (skols, t1) <- Expr.skolemizeVars(tpes, in)(newSkolemTyVar(_, _))
            sigmaT <- inferSigma(t1)
            z <- zonkTypedExpr(sigmaT)
            unSkol = unskolemize(skols)(z)
            // unSkol is not a Rho type, we need instantiate it
            coerce <- instSigma(unSkol.getType, expect, region(term))
          } yield coerce(unSkol)
        case Lambda(name, None, result, tag) =>
          expect match {
            case Expected.Check((expTy, rr)) =>
              for {
                vb <- unifyFn(expTy, rr, region(term))
                // we know expTy is in weak-prenex form, and since Fn is covariant, bodyT must be
                // in weak prenex form
                (varT, bodyT) = vb
                bodyTRho <- assertRho(
                  bodyT,
                  s"expect a rho type in $vb from $expTy at $rr",
                  region(result)
                )
                typedBody <- extendEnv(name, varT) {
                  checkRho(result, bodyTRho)
                }
              } yield TypedExpr.AnnotatedLambda(name, varT, typedBody, tag)
            case infer @ Expected.Inf(_) =>
              for {
                varT <- newMetaType(Kind.Type) // the kind of a fn arg is a Type
                typedBody <- extendEnv(name, varT)(inferRho(result))
                bodyT = typedBody.getType
                _ <- infer.set((Type.Fun(varT, bodyT), region(term)))
              } yield TypedExpr.AnnotatedLambda(name, varT, typedBody, tag)
          }
        case Lambda(name, Some(tpe), result, tag) =>
          expect match {
            case Expected.Check((expTy, rr)) =>
              for {
                vb <- unifyFn(expTy, rr, region(term))
                // we know expTy is in weak-prenex form, and since Fn is covariant, bodyT must be
                // in weak prenex form
                (varT, bodyT) = vb
                bodyTRho <- assertRho(
                  bodyT,
                  s"expect a rho type in $vb from $expTy at $rr",
                  region(result)
                )
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
              } yield TypedExpr.AnnotatedLambda(
                name,
                varT /* or tpe? */,
                typedBody,
                tag
              )
            case infer @ Expected.Inf(_) =>
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
            newMetaType(Kind.Type) // the kind of a let value is a Type
              .flatMap { rhsTpe =>
                extendEnv(name, rhsTpe) {
                  for {
                    // the type variable needs to be unified with varT
                    // note, varT could be a sigma type, it is not a Tau or Rho
                    typedRhs <- inferSigmaMeta(
                      rhs,
                      Some((name, rhsTpe, region(rhs)))
                    )
                    varT = typedRhs.getType
                    // we need to overwrite the metavariable now with the full type
                    typedBody <- extendEnv(name, varT)(
                      typeCheckRho(body, expect)
                    )
                    // TODO: a more efficient algorithm would do this top down
                    // for each top level TypedExpr and build it bottom up.
                    // we could do this after all typechecking is done
                    frees = TypedExpr.freeVars(typedRhs :: Nil)
                    isRecursive = RecursionKind.recursive(frees.contains(name))
                  } yield TypedExpr.Let(
                    name,
                    typedRhs,
                    typedBody,
                    isRecursive,
                    tag
                  )
                }
              }
          } else {
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
            coerce <- instSigma(tpe, expect, region(tag))
          } yield coerce(typedTerm)
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
          inferSigma(term)
            .flatMap { tsigma =>
              val check = Expected.Check((tsigma.getType, region(term)))

              expect match {
                case Expected.Check((resT, _)) =>
                  for {
                    tbranches <- branches.traverse { case (p, r) =>
                      // note, resT is in weak-prenex form, so this call is permitted
                      checkBranch(p, check, r, resT)
                    }
                  } yield TypedExpr.Match(tsigma, tbranches, tag)
                case infer @ Expected.Inf(_) =>
                  for {
                    tbranches <- branches.traverse { case (p, r) =>
                      inferBranch(p, check, r)
                    }
                    (rho, regRho, resBranches) <- narrowBranches(tbranches)
                    _ <- infer.set((rho, regRho))
                  } yield TypedExpr.Match(tsigma, resBranches, tag)
              }
            }
      }
    }

    def narrowBranches[A: HasRegion](
        branches: NonEmptyList[(Pattern, TypedExpr.Rho[A])]
    ): Infer[(Type.Rho, Region, NonEmptyList[(Pattern, TypedExpr.Rho[A])])] = {

      def minBy[M[_]: Monad, B](head: B, tail: List[B])(
          lteq: (B, B) => M[Boolean]
      ): M[B] =
        tail match {
          case Nil => Monad[M].pure(head)
          case h :: tail =>
            lteq(head, h)
              .flatMap { keep =>
                val next = if (keep) head else h
                minBy(next, tail)(lteq)
              }
        }

      def ltEq[K](
          left: (TypedExpr[A], K),
          right: (TypedExpr[A], K)
      ): Infer[Boolean] = {
        val leftTE = left._1
        val rightTE = right._1
        val lt = leftTE.getType
        val lr = region(leftTE)
        val rt = rightTE.getType
        val rr = region(rightTE)
        // right <= left if left subsumes right
        subsCheck(lt, rt, lr, rr).peek
          .flatMap {
            case Right(_) => pure(true)
            case Left(_)  =>
              // maybe the other way around
              subsCheck(rt, lt, rr, lr).peek
                .flatMap {
                  case Right(_) =>
                    // okay, we see right > left
                    pure(false)
                  case Left(err) =>
                    // this is a real unification error
                    fail(err)
                }
          }
      }

      val withIdx = branches.zipWithIndex.map { case ((p, te), idx) =>
        (te, (p, idx))
      }

      for {
        (minRes, (minPat, minIdx)) <- minBy(withIdx.head, withIdx.tail)(
          ltEq(_, _)
        )
        resTpe = minRes.getType
        // inferBranch returns TypedExpr.Rho, so this should be a rho type
        resTRho <- assertRho(
          resTpe,
          s"infer on match $minRes",
          region(branches.toList(minIdx)._2)
        )
        resRegion = region(minRes)
        resBranches <- withIdx.traverse { case (te, (p, idx)) =>
          if (idx != minIdx) {
            // unfortunately we have to check each branch again to get the correct coerce
            subsCheck(resTRho, te.getType, resRegion, region(te))
              .map { coerce =>
                (p, coerce(te))
              }
          } else pure((p, te))
        }
      } yield (resTRho, resRegion, resBranches)
    }

    /*
     * we require resT in weak prenex form because we call checkRho with it
     */
    def checkBranch[A: HasRegion](
        p: Pattern,
        sigma: Expected.Check[(Type, Region)],
        res: Expr[A],
        resT: Type.Rho
    ): Infer[(Pattern, TypedExpr.Rho[A])] =
      for {
        patBind <- typeCheckPattern(p, sigma, region(res))
        (pattern, bindings) = patBind
        tres <- extendEnvList(bindings)(checkRho(res, resT))
      } yield (pattern, tres)

    def inferBranch[A: HasRegion](
        p: Pattern,
        sigma: Expected.Check[(Type, Region)],
        res: Expr[A]
    ): Infer[(Pattern, TypedExpr.Rho[A])] =
      for {
        patBind <- typeCheckPattern(p, sigma, region(res))
        (pattern, bindings) = patBind
        // inferRho returns a TypedExpr.Rho (which is only an alias)
        res <- extendEnvList(bindings)(inferRho(res))
      } yield (pattern, res)

    /** patterns can be a sigma type, not neccesarily a rho/tau return a list of
      * bound names and their (sigma) types
      *
      * TODO: Pattern needs to have a region for each part
      */
    def typeCheckPattern(
        pat: Pattern,
        sigma: Expected.Check[(Type, Region)],
        reg: Region
    ): Infer[(Pattern, List[(Bindable, Type)])] =
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
                val res =
                  (GenPattern.Annotation(GenPattern.Named(n, pat), t), t)
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
          val names = items.collect { case GenPattern.StrPart.NamedStr(n) =>
            (n, tpe)
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
              e: ListPart[Pattern]
          ): Infer[(ListPart[Pattern], List[(Bindable, Type)])] =
            e match {
              case l @ ListPart.WildList =>
                // this is *a pattern that has list type, and binds that type to the name
                Infer.pure((l, Nil))
              case l @ ListPart.NamedList(splice) =>
                // this is *a pattern that has list type, and binds that type to the name
                Infer.pure((l, (splice, lst) :: Nil))
              case ListPart.Item(p) =>
                // This is a non-splice
                checkPat(p, inner, reg).map { case (p, l) =>
                  (ListPart.Item(p), l)
                }
            }
          val tpeOfList: Infer[Type] =
            sigma.value match {
              case (Type.TyApply(Type.ListType, item), _) =>
                pure(item)
              case (
                    Type.ForAll(
                      b @ NonEmptyList(_, Nil),
                      Type.TyApply(Type.ListType, item)
                    ),
                    _
                  ) =>
                // list is covariant so we can push down
                pure(Type.forAll(b, item))
              case (_, reg) =>
                for {
                  tpeA <- newMetaType(Kind.Type) // lists +* -> *
                  listA = Type.TyApply(Type.ListType, tpeA)
                  _ <- checkPatSigma(listA, sigma, reg)
                } yield tpeA
            }

          for {
            tpeA <- tpeOfList
            listA = Type.TyApply(Type.ListType, tpeA)
            inners <- items.traverse(checkItem(tpeA, listA, _))
            innerPat = inners.map(_._1)
            innerBinds = inners.flatMap(_._2)
          } yield (
            GenPattern.Annotation(GenPattern.ListPat(innerPat), listA),
            innerBinds
          )

        case GenPattern.Annotation(p, tpe) =>
          // like in the case of an annotation, we check the type, then
          // instantiate a sigma type
          // checkSigma(term, tpe) *> instSigma(tpe, expect)
          for {
            patBind <- checkPat(p, tpe, reg)
            (p1, binds) = patBind
            _ <- checkPatSigma(tpe, sigma, reg)
          } yield (p1, binds)
        case GenPattern.PositionalStruct(nm, args) =>
          for {
            params <- instDataCon(nm, sigma.value._1, reg, sigma.value._2)
            // we need to do a pattern linting phase and probably error
            // if the pattern arity does not match the arity of the constructor
            // but we don't want to error type-checking since we want to show
            // the maximimum number of errors to the user
            envs <- args.zip(params).traverse { case (p, t) =>
              checkPat(p, t, reg)
            }
            pats = envs.map(_._1)
            bindings = envs.map(_._2)
          } yield (GenPattern.PositionalStruct(nm, pats), bindings.flatten)
        case u @ GenPattern.Union(h, t) =>
          (
            typeCheckPattern(h, sigma, reg),
            t.traverse(typeCheckPattern(_, sigma, reg))
          ).mapN { case ((h, binds), neList) =>
            val pat = GenPattern.Union(h, neList.map(_._1))
            val allBinds = NonEmptyList(binds, (neList.map(_._2).toList))
            identicalBinds(u, allBinds, reg).as((pat, binds))
          }.flatten
      }

    // Unions have to have identical bindings in all branches
    def identicalBinds(
        u: Pattern,
        binds: NonEmptyList[List[(Bindable, Type)]],
        reg: Region
    ): Infer[Unit] =
      binds.map(_.map(_._1)) match {
        case nel @ NonEmptyList(h, t) =>
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
          } else fail(Error.UnionPatternBindMismatch(u, nel, reg))
      }

    // TODO: we should be able to derive a region for any pattern
    def checkPat(
        pat: Pattern,
        sigma: Type,
        reg: Region
    ): Infer[(Pattern, List[(Bindable, Type)])] =
      typeCheckPattern(pat, Expected.Check((sigma, reg)), reg)

    def checkPatSigma(
        tpe: Type,
        exp: Expected.Check[(Type, Region)],
        sRegion: Region
    ): Infer[Unit] =
      exp match {
        case Expected.Check((texp, tr)) =>
          subsCheck(
            texp,
            tpe,
            tr,
            sRegion
          ).void // this unit does not seem right
      }

    /** To do this, Infer will need to know the names of the type constructors
      * in scope.
      *
      * Instantiation fills in all
      */
    def instDataCon(
        consName: (PackageName, Constructor),
        sigma: Type,
        reg: Region,
        sigmaRegion: Region
    ): Infer[List[Type]] =
      GetDataCons(consName, reg).flatMap { case (args, consParams, tpeName) =>
        val thisTpe = Type.TyConst(tpeName)
        def loop(
            revArgs: List[(Type.Var.Bound, Kind.Arg)],
            leftKind: Kind,
            sigma: Type
        ): Infer[Map[Type.Var, Type]] =
          (revArgs, sigma) match {
            case (Nil, tpe) =>
              for {
                lk <- kindOf(tpe, sigmaRegion)
                _ <- unifyKind(leftKind, thisTpe, lk, tpe, reg, sigmaRegion)
                _ <- unifyType(thisTpe, tpe, reg, sigmaRegion)
              } yield Map.empty
            case ((v0, k) :: vs, Type.TyApply(left, right)) =>
              for {
                rk <- kindOf(right, sigmaRegion)
                _ <- unifyKind(
                  k.kind,
                  Type.TyVar(v0),
                  rk,
                  right,
                  reg,
                  sigmaRegion
                )
                rest <- loop(vs, Kind.Cons(k, leftKind), left)
              } yield rest.updated(v0, right)
            case (_, fa @ Type.ForAll(_, _)) =>
              // we have to instantiate a rho type
              instantiate(fa, sigmaRegion).flatMap(loop(revArgs, leftKind, _))
            case ((v0, k) :: rest, _) =>
              // (k -> leftKind)(k)
              for {
                left <- newMetaType(Kind.Cons(k, leftKind))
                right <- newMetaType(k.kind)
                _ <- unifyType(
                  Type.TyApply(left, right),
                  sigma,
                  reg,
                  sigmaRegion
                )
                sigmaKind <- kindOf(sigma, sigmaRegion)
                _ <- unifyKind(
                  leftKind,
                  Type.TyVar(v0),
                  sigmaKind,
                  sigma,
                  reg,
                  sigmaRegion
                )
                nextKind = Kind.Cons(k, leftKind)
                rest <- loop(rest, nextKind, left)
              } yield rest.updated(v0, right)
          }
        // for a covariant type, forall a. t[a] == t[forall a. a]
        // so we push the forall down to avoid allocating a metaVar which can only
        // hold a monotype
        def pushDownCovariant(
            revArgs: List[(Type.Var.Bound, Kind.Arg)],
            revForAlls: List[(Type.Var.Bound, Kind)],
            sigma: Type
        ): Type = {
          (revArgs, sigma) match {
            case (_, Type.ForAll(params, over)) =>
              pushDownCovariant(
                revArgs,
                params.toList reverse_::: revForAlls,
                over
              )
            case (
                  (_, Kind.Arg(Variance.Covariant, _)) :: rest,
                  Type.TyApply(left, right)
                ) =>
              // TODO Phantom variance has some special rules too. I guess we
              // can push into phantom as well (though that's rare)
              val leftFree = Type.freeBoundTyVars(left :: Nil).toSet
              val rightFree = Type.freeBoundTyVars(right :: Nil).toSet

              val (nextRFA, nextRight) =
                revForAlls.filter { case (leftA, _) =>
                  rightFree(leftA) && !leftFree(leftA)
                } match {
                  case Nil    => (revForAlls, right)
                  case pushed =>
                    // it is safe to push it down
                    val pushedSet = pushed.iterator.map(_._1).toSet
                    val revFA1 = revForAlls.toList.filterNot { case (b, _) =>
                      pushedSet(b)
                    }
                    val pushedRight = Type.forAll(pushed.reverse, right)
                    (revFA1, pushedRight)
                }
              pushDownCovariant(rest, nextRFA, left) match {
                case Type.ForAll(bs, l) =>
                  Type.forAll(bs, Type.TyApply(l, nextRight))
                case rho: Type.Rho =>
                  Type.TyApply(rho, nextRight)
              }
            case (_ :: rest, Type.TyApply(left, right)) =>
              val rightFree = Type.freeBoundTyVars(right :: Nil).toSet
              val (keptRight, lefts) =
                revForAlls.partition { case (leftA, _) => rightFree(leftA) }

              Type.forAll(
                keptRight.reverse,
                pushDownCovariant(rest, lefts, left)
              ) match {
                case Type.ForAll(bs, l) =>
                  Type.forAll(bs, Type.TyApply(l, right))
                case rho: Type.Rho =>
                  Type.TyApply(rho, right)
              }
            case _ =>
              Type.forAll(revForAlls.reverse, sigma)
          }
        }
        val revArgs = args.reverse
        val pushedTpe = pushDownCovariant(revArgs, Nil, sigma)
        loop(revArgs, Kind.Type, pushedTpe)
          .map { env =>
            consParams.map(Type.substituteVar(_, env))
          }
      }

    def inferSigma[A: HasRegion](e: Expr[A]): Infer[TypedExpr[A]] =
      inferSigmaMeta(e, None)

    def inferSigmaMeta[A: HasRegion](
        e: Expr[A],
        meta: Option[(Identifier, Type.TyMeta, Region)]
    ): Infer[TypedExpr[A]] = {
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
        expTyRho <- assertRho(
          expTy,
          s"must be rho since $rho is a TypedExpr.Rho",
          region(e)
        )
        envTys <- unifySelf(expTyRho)
        q <- TypedExpr.quantify(
          envTys,
          rho,
          zonk(_),
          { (m, n) =>
            // quantify guarantees that the kind of n matches m
            writeMeta(m, Type.TyVar(n))
          }
        )
      } yield q
    }

    def checkSigma[A: HasRegion](t: Expr[A], tpe: Type): Infer[TypedExpr[A]] =
      for {
        skolRho <- skolemize(tpe, region(t))
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
              _ <- require(
                badTvs.isEmpty,
                Error.NotPolymorphicEnough(
                  tpe,
                  t,
                  NonEmptyList.fromListUnsafe(badTvs),
                  region(t)
                )
              )
              // we need to zonk before we unskolemize because some of the metas could be skolems
              zte <- zonkTypedExpr(te)
            } yield unskolemize(neskols)(zte)
        }
      } yield te1 // should be fine since the everything after te is just checking

    /** invariant: rho needs to be in weak-prenex form
      */
    def checkRho[A: HasRegion](
        t: Expr[A],
        rho: Type.Rho
    ): Infer[TypedExpr.Rho[A]] =
      typeCheckRho(t, Expected.Check((rho, region(t))))

    /** recall a rho type never has a top level Forall
      */
    def inferRho[A: HasRegion](t: Expr[A]): Infer[TypedExpr.Rho[A]] =
      for {
        ref <- initRef[(Type.Rho, Region)](
          Error.InferIncomplete("inferRho", t, region(t))
        )
        expr <- typeCheckRho(t, Expected.Inf(ref))
        _ <- lift(
          ref.reset
        ) // we don't need this ref, and it does not escape, so reset
      } yield expr
  }

  private def recursiveTypeCheck[A: HasRegion](
      name: Bindable,
      expr: Expr[A]
  ): Infer[TypedExpr[A]] =
    // values are of kind Type
    newMetaType(Kind.Type).flatMap { tpe =>
      extendEnv(name, tpe)(typeCheckMeta(expr, Some((name, tpe, region(expr)))))
    }

  def typeCheck[A: HasRegion](t: Expr[A]): Infer[TypedExpr[A]] =
    typeCheckMeta(t, None)

  private def unskolemize(
      skols: NonEmptyList[Type.Var.Skolem]
  ): TypedExpr.Coerce =
    new FunctionK[TypedExpr, TypedExpr] {
      def apply[A](te: TypedExpr[A]) = {
        // now replace the skols with generics
        val used = Type.tyVarBinders(te.getType :: Nil)
        val aligned = Type.alignBinders(skols, used)
        val te2 =
          substTyExpr(skols, aligned.map { case (_, b) => Type.TyVar(b) }, te)
        // TODO: we have to not forget the skolem kinds
        TypedExpr.forAll(aligned.map { case (s, b) => (b, s.kind) }, te2)
      }
    }

  private def typeCheckMeta[A: HasRegion](
      t: Expr[A],
      optMeta: Option[(Identifier, Type.TyMeta, Region)]
  ): Infer[TypedExpr[A]] = {
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
     *
     * TODO Kind we need to know the kinds of these skolems
     */
    val optSkols = t match {
      case Expr.Generic(vs, e) =>
        Some(for {
          skolsE1 <- Expr.skolemizeVars(vs, e)(newSkolemTyVar(_, _))
          (skols, e1) = skolsE1
          /*
           * This is a bit weird, but for top-level defs, the type parameters
           * only need to be a superset of free variables. You don't need
           * to declare them all. On inner variables of a def we assume
           * it is free in the top level def unless you declare it generic.
           * Maybe worth revisiting and require ALL free variables declared
           * or none of them...
           */
          optMore = Expr.skolemizeFreeVars(e1)(newSkolemTyVar(_, _))
          res <- optMore.fold(pure(skolsE1)) { restSkols =>
            restSkols.map { case (sM, eM) => (skols ::: sM, eM) }
          }
        } yield res)
      case notGeneric =>
        Expr.skolemizeFreeVars(notGeneric)(newSkolemTyVar(_, _))
    }

    optSkols match {
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

  def extendEnvList[A](bindings: List[(Bindable, Type)])(
      of: Infer[A]
  ): Infer[A] =
    Infer.Impl.ExtendEnvs(bindings.map { case (n, t) => ((None, n), t) }, of)

  private def extendEnvPack[A](pack: PackageName, name: Bindable, tpe: Type)(
      of: Infer[A]
  ): Infer[A] =
    Infer.Impl.ExtendEnvs(((Some(pack), name), tpe) :: Nil, of)

  /** Packages are generally just lists of lets, this allows you to infer the
    * scheme for each in the context of the list
    */
  def typeCheckLets[A: HasRegion](
      pack: PackageName,
      ls: List[(Bindable, RecursionKind, Expr[A])]
  ): Infer[List[(Bindable, RecursionKind, TypedExpr[A])]] =
    ls match {
      case Nil => Infer.pure(Nil)
      case (name, rec, expr) :: tail =>
        for {
          te <-
            if (rec.isRecursive) recursiveTypeCheck(name, expr)
            else typeCheck(expr)
          rest <- extendEnvPack(pack, name, te.getType)(
            typeCheckLets(pack, tail)
          )
        } yield (name, rec, te) :: rest
    }

  /** This is useful to testing purposes.
    *
    * Given types a and b, can we substitute a for for b
    */
  def substitutionCheck(a: Type, b: Type, ra: Region, rb: Region): Infer[Unit] =
    subsCheck(a, b, ra, rb).void
}
