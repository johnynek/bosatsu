package dev.bosatsu.rankn

import cats.{Functor, Monad}
import cats.arrow.FunctionK
import cats.data.{Chain, NonEmptyChain, NonEmptyList}

import cats.syntax.all._

import dev.bosatsu.{
  Expr,
  HasRegion,
  Identifier,
  Kind,
  ListUtil,
  PackageName,
  ParallelViaProduct,
  Pattern => GenPattern,
  Region,
  RecursionKind,
  TypedExpr,
  Variance
}

import scala.collection.immutable.SortedSet

import HasRegion.region

import Identifier.{Bindable, Constructor}
import scala.collection.immutable.SortedMap

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

  implicit val inferParallel: cats.Parallel[Infer] =
    new ParallelViaProduct[Infer] {
      def monad = inferMonad
      def parallelProduct[A, B](fa: Infer[A], fb: Infer[B]): Infer[(A, B)] =
        ParallelProduct(fa, fb)
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

    override def toString() = s"Env($uniq, $vars, $typeCons, $variances)"

    def addVars(vt: NonEmptyList[(Name, Type)]): Env =
      new Env(uniq, vars = (vars + vt.head) ++ vt.tail, typeCons, variances)

    private val kindCache: Type => Either[Region => Error, Kind] =
      Type.kindOf[Region => Error](
        b => { region =>
          Error.UnknownKindOfVar(Type.TyVar(b), region, s"unbound var: $b")
        },
        ap => { region =>
          Error.KindCannotTyApply(ap, region)
        },
        (ap, cons, rhs) => { region =>
          Error.KindInvalidApply(ap, cons, rhs, region)
        },
        { case Type.TyConst(const) =>
          val d = const.toDefined
          // some tests rely on syntax without importing
          // TODO remove this
          variances.get(d).orElse(Type.builtInKinds.get(d)) match {
            case Some(ks) => Right(ks)
            case None     => Left(region => Error.UnknownDefined(d, region))
          }
        }
      )

    def getKindOpt(t: Type): Option[Kind] =
      kindCache(t).toOption

    def getKind(t: Type, region: Region): Either[Error, Kind] =
      kindCache(t).leftMap { err =>
        err(region)
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
    sealed abstract class Single extends Error

    /** These are errors in the ability to type the code Generally these cannot
      * be caught by other phases
      */
    sealed abstract class TypeError extends Single

    case class NotUnifiable(
        left: Type,
        right: Type,
        leftRegion: Region,
        rightRegion: Region
    ) extends TypeError
    case class KindInvalidApply(
        typeApply: Type.TyApply,
        leftK: Kind.Cons,
        rightK: Kind,
        region: Region
    ) extends TypeError
    case class KindExpectedType(
        tpe: Type,
        kind: Kind.Cons,
        region: Region
    ) extends TypeError
    case class KindMismatch(
        target: Type,
        targetKind: Kind,
        source: Type,
        sourceKind: Kind,
        targetRegion: Region,
        sourceRegion: Region
    ) extends TypeError
    case class KindCannotTyApply(ap: Type.TyApply, region: Region)
        extends TypeError
    case class UnknownDefined(tpe: Type.Const.Defined, region: Region)
        extends TypeError
    case class NotPolymorphicEnough(
        tpe: Type,
        in: Expr[?],
        badTvs: NonEmptyList[Type],
        reg: Region
    ) extends TypeError
    case class SubsumptionCheckFailure(
        inferred: Type,
        declared: Type,
        infRegion: Region,
        decRegion: Region,
        badTvs: NonEmptyList[Type]
    ) extends TypeError
    // this sounds internal but can be due to an infinite type attempted to be defined
    case class UnexpectedMeta(
        m: Type.Meta,
        in: Type,
        left: Region,
        right: Region
    ) extends TypeError
    case class ArityMismatch(
        leftArity: Int,
        leftRegion: Region,
        rightArity: Int,
        rightRegion: Region
    ) extends TypeError
    case class ArityTooLarge(arity: Int, maxArity: Int, region: Region)
        extends TypeError

    /** These are errors that prevent typing due to unknown names, They could be
      * caught in a phase that collects all the naming errors
      */
    sealed abstract class NameError extends Single

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
    sealed abstract class InternalError extends Single {
      def message: String
      def region: Region
    }
    // This is a logic error which should never happen
    case class InferIncomplete(term: Expr[?], region: Region)
        extends InternalError {
      // $COVERAGE-OFF$ we don't test these messages, maybe they should be removed
      def message = s"inferRho not complete for $term"
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

    // here is when we have more than one error
    case class Combine(left: Error, right: Error) extends Error {
      private def flatten(
          errs: NonEmptyList[Error],
          inAcc: Set[Single],
          acc: Chain[Single]
      ): NonEmptyChain[Single] =
        errs match {
          case NonEmptyList(s: Single, tail) =>
            tail match {
              case Nil =>
                if (inAcc(s)) {
                  // we know s is in acc, so Chain must not be empty
                  NonEmptyChain.fromChainUnsafe(acc)
                } else {
                  NonEmptyChain.fromChainAppend(acc, s)
                }
              case h :: t =>
                if (inAcc(s)) {
                  flatten(NonEmptyList(h, t), inAcc, acc)
                } else {
                  flatten(NonEmptyList(h, t), inAcc + s, acc :+ s)
                }
            }
          case NonEmptyList(Combine(a, b), tail) =>
            flatten(NonEmptyList(a, b :: tail), inAcc, acc)
        }

      lazy val flatten: NonEmptyChain[Single] =
        flatten(NonEmptyList(left, right :: Nil), Set.empty, Chain.empty)
    }
  }

  /** This is where the internal implementation goes. It is here to make it easy
    * to make one block private and not do so on every little helper function
    */
  private object Impl {
    sealed abstract class Expected[A]
    object Expected {
      case class Inf[A](ref: Ref[Either[Error.InferIncomplete, A]])
          extends Expected[A] {
        def set(a: A): Infer[Unit] =
          Infer.lift(ref.set(Right(a)))
      }
      case class Check[A](value: A) extends Expected[A]
    }

    case class FlatMap[A, B](fa: Infer[A], fn: A => Infer[B]) extends Infer[B] {
      def run(env: Env) =
        fa.run(env).flatMap {
          case Right(a)       => fn(a).run(env)
          case left @ Left(_) => RefSpace.pure(left.rightCast)
        }
    }

    case class ParallelProduct[A, B](fa: Infer[A], fb: Infer[B])
        extends Infer[(A, B)] {
      def run(env: Env) =
        fa.run(env).flatMap {
          case Right(a) =>
            fb.run(env).map {
              case Right(b)       => Right((a, b))
              case left @ Left(_) => left.rightCast
            }
          case left @ Left(errA) =>
            fb.run(env).map {
              case Right(_)   => left.rightCast
              case Left(errB) => Left(Error.Combine(errA, errB))
            }
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
          case Right(a)       => RefSpace.pure(fn(a))
          case left @ Left(_) => RefSpace.pure(left.rightCast)
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
      def run(env: Env): RefSpace[Either[Error, Env]] =
        RefSpace.pure(Right(env))
    }

    def GetDataCons(fqn: (PackageName, Constructor), reg: Region): Infer[Cons] =
      GetEnv.mapEither { env =>
        env.typeCons.get(fqn) match {
          case Some(res) => Right(res)
          case None      =>
            Left(Error.UnknownConstructor(fqn, reg, env))
        }
      }

    case class ExtendEnvs[A](vt: NonEmptyList[(Name, Type)], in: Infer[A])
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
      GetEnv.map(env => tpe => env.getKind(tpe, emptyRegion).toOption)
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
        case Kind.Type                    =>
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
      *
      * see: https://www.csd.uwo.ca/~lkari/prenex.pdf It seems that if C[x] is
      * covariant, then C[forall x. D[x]] == forall x. C[D[x]]
      *
      * this is always true for existential quantification I think, but for
      * universal, we need that C is covariant which roughly means C[x] either
      * has x in a return position of a function, or not at all, which then
      * gives us that (forall x. (A(x) u B(x))) == (forall x A(x)) u (forall x
      * B(x)) where A(x) and B(x) represent the union branches of the type C
      *
      * Here, we only float quantification above completely covariant paths,
      * which includes function results returning functions, etc. I don't really
      * know why this works, but it is skolemizing in a smaller set of cases
      * compared to *any* covariant path (including contra * contra), yet still
      * keeping functions in weak-prenex form (which isn't type checked, but is
      * asserted in the inference and typechecking process).
      *
      * So, I guess one argument is we want to skolemize the least we can to
      * make inference work, especially with function return types, and this is
      * sufficient to do it. It also passes all the current tests.
      *
      * The paper we base the type inference on doesn't have a type system as
      * complex as bosatsu, so we have to generalize it.
      */
    private def skolemize(
        t: Type,
        region: Region
    ): Infer[(List[Type.Var.Skolem], List[Type.TyMeta], Type.Rho)] = {

      // Invariant: if t is Rho, then result._3 is Rho
      def loop(
          t: Type,
          allCo: Boolean
      ): Infer[(List[Type.Var.Skolem], List[Type.TyMeta], Type)] =
        t match {
          case q: Type.Quantified =>
            if (allCo) {
              val univ = q.forallList
              val exists = q.existList
              val ty = q.in
              // Rule PRPOLY
              for {
                sks1 <- univ.traverse { case (b, k) =>
                  newSkolemTyVar(b, k, existential = false)
                }
                ms <- exists.traverse { case (_, k) => newExistential(k) }
                sksT = sks1.map(Type.TyVar(_))
                ty1 = Type.substituteRhoVar(
                  ty,
                  (exists.map(_._1).iterator.zip(ms) ++
                    univ.map(_._1).iterator.zip(sksT.iterator)).toMap
                )
                (sks2, ms2, ty) <- loop(ty1, allCo)
              } yield (sks1 ::: sks2, ms ::: ms2, ty)
            } else pure((Nil, Nil, t))

          case ta @ Type.TyApply(left, right) =>
            // Rule PRFUN
            // we know the kind of left is k -> x, and right has kind k
            // since left: Rho, we know loop(left, path)._3 is Rho
            (varianceOfCons(ta, region), loop(left, allCo))
              .flatMapN { case (consVar, (sksl, el, ltpe0)) =>
                // due to loop invariant
                val ltpe: Type.Rho = ltpe0.asInstanceOf[Type.Rho]
                val allCoRight = allCo && (consVar == Variance.co)
                loop(right, allCoRight)
                  .map { case (sksr, er, rtpe) =>
                    (sksl ::: sksr, el ::: er, Type.TyApply(ltpe, rtpe))
                  }
              }
          case other: Type.Rho =>
            // Rule PRMONO
            pure((Nil, Nil, other))
        }

      loop(t, true).map {
        case (skols, metas, rho: Type.Rho) =>
          (skols, metas, rho)
        // $COVERAGE-OFF$ this should be unreachable
        // because we only return ForAll on paths nested inside noncovariant path in TyApply
        case (sks, metas, notRho) =>
          sys.error(s"type = $t, sks = $sks, metas = $metas notRho = $notRho")
        // $COVERAGE-ON$ this should be unreachable
      }
    }

    def getFreeTyVars(ts: List[Type]): Infer[Set[Type.Var]] =
      ts.traverse(zonkType).map(Type.freeTyVars(_).toSet)

    def getExistentialMetas(ts: List[Type]): Infer[Set[Type.Meta]] = {
      val pureEmpty = pure(SortedSet.empty[Type.Meta])

      def existentialsOf(tm: Type.Meta): Infer[Set[Type.Meta]] = {
        val parents = readMeta(tm).flatMap {
          case Some(Type.TyMeta(m2)) => existentialsOf(m2)
          case _                     => pureEmpty
        }

        if (tm.existential) parents.map(_ + tm)
        else parents
      }

      for {
        zonked <- ts.traverse(zonkType)
        metas = Type.metaTvs(zonked)
        metaSet <- metas.toList.traverse(existentialsOf)
      } yield metaSet.foldLeft(Set.empty[Type.Meta])(_ | _)
    }

    val zonk: Type.Meta => Infer[Option[Type.Rho]] =
      Type.zonk[Infer](SortedSet.empty, readMeta, writeMeta)

    /** This fills in any meta vars that have been quantified and replaces them
      * with what they point to
      */
    def zonkType(t: Type): Infer[Type] =
      Type.zonkMeta(t)(zonk)

    def zonkTypedExpr[A](e: TypedExpr[A]): Infer[TypedExpr[A]] =
      TypedExpr.zonkMeta(e)(zonk)

    val zonkTypeExprK: FunctionK[TypedExpr.Rho, [X] =>> Infer[TypedExpr[X]]] =
      new FunctionK[TypedExpr.Rho, [X] =>> Infer[TypedExpr[X]]] {
        def apply[A](fa: TypedExpr[A]): Infer[TypedExpr[A]] = zonkTypedExpr(fa)
      }

    def initRef[E: HasRegion, A](
        t: Expr[E]
    ): Infer[Ref[Either[Error.InferIncomplete, A]]] =
      lift(
        RefSpace.newRef[Either[Error.InferIncomplete, A]](
          Left(Error.InferIncomplete(t, region(t)))
        )
      )

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
    def instantiate(t: Type): Infer[(List[Type.Var.Skolem], Type.Rho)] =
      t match {
        case q: Type.Quantified =>
          // TODO: it may be possible to improve type checking
          // by pushing foralls into covariant constructors
          // but it's not trivial
          val univs = q.forallList
          val rho = q.in
          val univRho =
            NonEmptyList.fromList(univs) match {
              case Some(vars) =>
                vars
                  .traverse { case (_, k) => newMetaType(k) }
                  .map { vars1T =>
                    substTyRho(vars.map(_._1), vars1T)(rho)
                  }
              case None => pure(rho)
            }

          univRho.flatMap { rho =>
            val exists = q.existList
            for {
              skols <- exists.traverse { case (b, k) =>
                newSkolemTyVar(b, k, existential = true)
              }
              env = exists.iterator
                .map(_._1)
                .zip(skols.iterator.map(Type.TyVar(_)))
                .toMap[Type.Var, Type.TyVar]
              rho1 = Type.substituteRhoVar(rho, env)
            } yield (skols, rho1)
          }
        case rho: Type.Rho => pure((Nil, rho))
      }

    // Replace only the outer existentials with skolems, keep foralls intact.
    def skolemizeExistsOnly(t: Type): Infer[(List[Type.Var.Skolem], Type)] =
      t match {
        case q: Type.Quantified =>
          val exists = q.existList
          val foralls = q.forallList
          exists.traverse { case (b, k) =>
            newSkolemTyVar(b, k, existential = true)
          }.map { skols =>
            val env = exists.iterator
              .map(_._1)
              .zip(skols.iterator.map(Type.TyVar(_)))
              .toMap[Type.Var, Type.TyVar]
            val in1 = Type.substituteRhoVar(q.in, env)
            val t1 =
              Type.quantify(forallList = foralls, existList = Nil, in1)
            (skols, t1)
          }
        case rho: Type.Rho => pure((Nil, rho))
      }

    /*
     * Invariant: r2 needs to be in weak prenex form
     */
    def subsCheckFn(
        a1s: NonEmptyList[Type],
        r1: Type,
        a2s: NonEmptyList[Type],
        r2: Type.Rho,
        left: Region,
        right: Region
    ): Infer[TypedExpr.Coerce] =
      // note due to contravariance in input, we reverse the order there
      for {
        // we know that they have the same length because we have already called unifyFnRho
        coarg <- a2s.zip(a1s).parTraverse { case (a2, a1) =>
          subsCheck(a2, a1, right, left)
        }
        // r2 is already in weak-prenex form
        cores <- subsCheckRho(r1, r2, left, right)
        ks <- checkedKinds
      } yield TypedExpr.coerceFn(a1s, r2, coarg, cores, ks)

    /*
     * If t <:< rho then coerce to rho
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
        case (fa: Type.Quantified, rho) =>
          subsInstantiate(fa, rho, left, right) match {
            case Some(inf) => inf
            case None      =>
              // Rule SPEC
              for {
                (exSkols, faRho) <- instantiate(fa)
                unskol = unskolemizeExists(exSkols)
                coerce <- subsCheckRho2(faRho, rho, left, right)
              } yield coerce.andThen(unskol)
          }
        // for existential lower bounds, we skolemize the existentials
        // then verify they don't escape after inference and unskolemize
        // them (if they are free in the resulting type)
        case (rhot: Type.Rho, rho) =>
          subsCheckRho2(rhot, rho, left, right)
      }

    private val idCoerce = pure(FunctionK.id[TypedExpr])
    // if t <:< rho, then coerce to rho
    def subsCheckRho2(
        t: Type.Rho,
        rho: Type.Rho,
        left: Region,
        right: Region
    ): Infer[TypedExpr.Coerce] =
      if (t == rho) idCoerce
      else
        (t, rho) match {
          case (rho1, Type.Fun(a2, r2)) =>
            // Rule FUN
            for {
              (a1, r1) <- unifyFnRho(a2.length, rho1, left, right)
              // since rho is in weak prenex form, and Fun is covariant on r2, we know
              // r2 is in weak-prenex form and a rho type
              rhor2 <- assertRho(
                r2,
                s"subsCheckRho2($t, $rho, $left, $right), line 619",
                right
              )
              coerce <- subsCheckFn(a1, r1, a2, rhor2, left, right)
            } yield coerce
          case (Type.Fun(a1, r1), rho2) =>
            // Rule FUN
            for {
              (a2, r2) <- unifyFnRho(a1.length, rho2, right, left)
              // since rho is in weak prenex form, and Fun is covariant on r2, we know
              // r2 is in weak-prenex form
              rhor2 <- assertRho(
                r2,
                s"subsCheckRho2($t, $rho, $left, $right), line 628",
                right
              )
              coerce <- subsCheckFn(a1, r1, a2, rhor2, left, right)
            } yield coerce
          case (rho1, ta @ Type.TyApply(l2, r2)) =>
            for {
              (kl, kr) <- validateKinds(ta, right)
              (l1, r1) <- unifyTyApp(rho1, kl, kr, left, right)
              // Check from right to left
              _ <- subsCheckRho2(l1, l2, left, right)
              _ <- varianceOfConsKind(ta, kl, right).flatMap {
                case Variance.Covariant =>
                  subsCheck(r1, r2, left, right)
                case Variance.Contravariant =>
                  subsCheck(r2, r1, right, left)
                case Variance.Phantom =>
                  // this doesn't matter
                  unit
                case Variance.Invariant =>
                  unifyType(r1, r2, left, right)
              }
              ks <- checkedKinds
            } yield TypedExpr.coerceRho(ta, ks)
          case (ta @ Type.TyApply(l1, r1), rho2) =>
            // here we know that rho2 != TyApply
            for {
              (kl, kr) <- validateKinds(ta, left)
              // here we set the kinds of l2: kl and r2: k2
              // so the kinds definitely match
              (l2, r2) <- unifyTyApp(rho2, kl, kr, right, left)
              // Check from right to left
              _ <- subsCheckRho2(l1, l2, left, right)
              // we know that l2 has kind kl
              _ <- varianceOfConsKind(Type.TyApply(l2, r2), kl, right).flatMap {
                case Variance.Covariant =>
                  subsCheck(r1, r2, left, right)
                case Variance.Contravariant =>
                  subsCheck(r2, r1, right, left)
                case Variance.Phantom =>
                  // this doesn't matter
                  unit
                case Variance.Invariant =>
                  unifyType(r1, r2, left, right)
              }
              ks <- checkedKinds
            } yield TypedExpr.coerceRho(rho2, ks)
          case (t1, t2) =>
            // rule: MONO
            for {
              _ <- unify(t1, t2, left, right)
              ck <- checkedKinds
            } yield TypedExpr.coerceRho(
              t1,
              ck
            ) // TODO this coerce seems right, since we have unified
        }

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
            (exSkols, rho) <- instantiate(sigma)
            _ <- infer.set((rho, r))
            ks <- checkedKinds
            coerce = TypedExpr.coerceRho(rho, ks)
          } yield coerce.andThen(unskolemizeExists(exSkols))
      }

    def unifyFnRho(
        arity: Int,
        fnType: Type.Rho,
        fnRegion: Region,
        evidenceRegion: Region
    ): Infer[(NonEmptyList[Type], Type)] =
      fnType match {
        case Type.Fun(arg, res) =>
          val fnArity = arg.length
          if (fnArity == arity) pure((arg, res))
          else
            fail(Error.ArityMismatch(fnArity, fnRegion, arity, evidenceRegion))
        case tau =>
          if (Type.FnType.ValidArity.unapply(arity)) {
            val sized = NonEmptyList.fromListUnsafe((1 to arity).toList)
            for {
              argT <- sized.traverse(_ => newMeta)
              resT <- newMeta
              _ <- unify(tau, Type.Fun(argT, resT), fnRegion, evidenceRegion)
            } yield (argT, resT)
          } else {
            fail(
              Error.ArityTooLarge(arity, Type.FnType.MaxSize, evidenceRegion)
            )
          }
      }

    def validateKinds(ta: Type.TyApply, region: Region): Infer[(Kind, Kind)] =
      kindOf(ta.on, region)
        .parProduct(kindOf(ta.arg, region))
        .flatMap { case tup @ (lKind, rKind) =>
          Kind.validApply[Error](
            lKind,
            rKind,
            Error.KindCannotTyApply(ta, region)
          ) { cons =>
            Error.KindInvalidApply(ta, cons, rKind, region)
          } match {
            case Right(_)  => pure(tup)
            case Left(err) => fail(err)
          }
        }

    // destructure apType in left[right]
    // invariant apType is being checked against some rho with validated kind: lKind[rKind]
    def unifyTyApp(
        apType: Type.Rho,
        lKind: Kind,
        rKind: Kind,
        apRegion: Region,
        evidenceRegion: Region
    ): Infer[(Type.Rho, Type)] =
      apType match {
        case ta @ Type.TyApply(left, right) =>
          // this branch only happens when checking ta <:< (rho: lKind[rKind]) or >:> (rho)
          // TODO: what validates that ta has compatible kinds with lKind and rKind
          // since we could be doing subtyping or supertyping here we would need
          // to pass in some directionality
          validateKinds(ta, apRegion).as((left, right))
        case notApply =>
          for {
            leftT <- newMetaType(lKind)
            rightT <- newMetaType(rKind)
            ap = Type.TyApply(leftT, rightT)
            _ <- unify(notApply, ap, apRegion, evidenceRegion)
          } yield (leftT, rightT)
      }

    // invariant the flexible type variable ty1 is not bound
    def unifyUnboundVar(
        ty1: Type.TyMeta,
        ty2: Type.Tau,
        left: Region,
        right: Region
    ): Infer[Unit] =
      ty2 match {
        case meta2 @ Type.TyMeta(m2) =>
          val m = ty1.toMeta
          // we have to check that the kind matches before writing to a meta
          if (m.kind == m2.kind) {
            val cmp = Ordering[Type.Meta].compare(m, m2)
            if (cmp == 0) unit
            else
              (readMeta(m2).flatMap {
                case Some(ty2) =>
                  // we know that m2 is set, but m is not because ty1 is unbound
                  if (m.existential == m2.existential) {
                    // we unify here because ty2 could possibly be ty1
                    unify(ty1, ty2, left, right)
                  } else if (m.existential) {
                    // m2.existential == false
                    // we need to point m2 at m
                    writeMeta(m, ty2) *> writeMeta(m2, ty1)
                  } else {
                    // m.existential == false && m2.existential == true
                    // we need to point m at m2
                    writeMeta(m, meta2)
                  }
                case None =>
                  // Both m and m2 are not set. We just point one at the other
                  // by convention point to the smaller item which
                  // definitely prevents cycles.
                  if (cmp > 0) writeMeta(m, meta2)
                  else {
                    // since we checked above we know that
                    // m.id != m2.id, so it is safe to write without
                    // creating a self-loop here
                    writeMeta(m2, ty1)
                  }
              })
          } else {
            fail(
              Error.KindMismatch(
                ty1,
                ty1.toMeta.kind,
                meta2,
                m2.kind,
                left,
                right
              )
            )
          }
        case nonMeta =>
          // we have a non-meta, but inside of it (TyApply) we may have
          // metas. Let's go ahead and zonk them now to minimize nesting
          // metas inside metas.
          zonkType(nonMeta)
            .flatMap { nm2 =>
              val m = ty1.toMeta
              val tvs2 = Type.metaTvs(nm2 :: Nil)
              if (tvs2(m)) fail(Error.UnexpectedMeta(m, nonMeta, left, right))
              else {
                kindOf(nonMeta, right)
                  .flatMap { nmk =>
                    if (Kind.leftSubsumesRight(m.kind, nmk)) {
                      // we have to check that the kind matches before writing to a meta
                      // This is not symmetric unification: we are solving an
                      // unbound meta, so its kind is an upper bound on what we
                      // may instantiate it with. We only allow the concrete
                      // kind to be <= the meta kind (variance can be widened
                      // upward/forgotten, but not made more specific).
                      writeMeta(m, nonMeta)
                    } else {
                      fail(
                        Error.KindMismatch(
                          ty1,
                          m.kind,
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
        tv: Type.TyMeta,
        t: Type.Tau,
        left: Region,
        right: Region
    ): Infer[Unit] =
      readMeta(tv.toMeta).flatMap {
        case None      => unifyUnboundVar(tv, t, left, right)
        case Some(ty1) => unify(ty1, t, left, right)
      }

    def show(t: Type): String =
      Type.fullyResolvedDocument.document(t).render(80)

    def unify(t1: Type.Tau, t2: Type.Tau, r1: Region, r2: Region): Infer[Unit] =
      (t1, t2) match {
        case (Type.TyMeta(m1), Type.TyMeta(m2)) if m1.id == m2.id => unit
        case (meta @ Type.TyMeta(_), tpe) => unifyVar(meta, tpe, r1, r2)
        case (tpe, meta @ Type.TyMeta(_)) => unifyVar(meta, tpe, r2, r1)
        case (t1 @ Type.TyApply(a1, b1), t2 @ Type.TyApply(a2, b2)) =>
          validateKinds(t1, r1) &>
            validateKinds(t2, r2) &>
            unify(a1, a2, r1, r2) &>
            unifyType(b1, b2, r1, r2)
        case (Type.TyConst(c1), Type.TyConst(c2)) if c1 == c2 => unit
        case (Type.TyVar(v1), Type.TyVar(v2)) if v1 === v2    => unit
        case (Type.TyVar(b @ Type.Var.Bound(_)), _)           =>
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
          subsCheck(t1, t2, r1, r2) &> subsCheck(t2, t1, r2, r1).void
      }

    private val emptyRef = lift(RefSpace.newRef[Option[Type.Tau]](None))

    /** Allocate a new Meta variable which will point to a Tau (no forall
      * anywhere) type
      */
    def newMetaType0(kind: Kind, existential: Boolean): Infer[Type.TyMeta] =
      for {
        id <- nextId
        ref <- emptyRef
        meta = Type.Meta(kind, id, existential, ref)
      } yield Type.TyMeta(meta)

    def newMetaType(kind: Kind): Infer[Type.TyMeta] =
      newMetaType0(kind, existential = false)

    // the common case of a Meta with Kind = Type
    val newMeta: Infer[Type.TyMeta] = newMetaType(Kind.Type)

    def newExistential(kind: Kind): Infer[Type.TyMeta] =
      newMetaType0(kind, existential = true)

    // TODO: it would be nice to support kind inference on skolem variables
    def newSkolemTyVar(
        tv: Type.Var.Bound,
        kind: Kind,
        existential: Boolean
    ): Infer[Type.Var.Skolem] =
      nextId.map(Type.Var.Skolem(tv.name, kind, existential, _))

    /** See if the meta variable has been set with a Tau type
      */
    def readMeta(m: Type.Meta): Infer[Option[Type.Tau]] =
      lift(m.ref.get)

    /** Set the meta variable to point to a Tau type
      */
    private def writeMeta(m: Type.Meta, v: Type.Tau): Infer[Unit] =
      lift(m.ref.set(Some(v)))

    private def clearMeta(m: Type.Meta): Infer[Unit] =
      lift(m.ref.set(None))

    implicit class AndThenMap[F[_], G[_], J[_]](
        private val fk: FunctionK[F, [X] =>> G[J[X]]]
    ) extends AnyVal {
      def andThenMap[H[_]](
          fn2: FunctionK[J, H]
      )(implicit G: Functor[G]): FunctionK[F, [X] =>> G[H[X]]] =
        new FunctionK[F, [X] =>> G[H[X]]] {
          def apply[A](fa: F[A]): G[H[A]] =
            fk(fa).map(fn2(_))
        }

      def andThenFlatMap[H[_]](
          fn2: FunctionK[J, [X] =>> G[H[X]]]
      )(implicit G: Monad[G]): FunctionK[F, [X] =>> G[H[X]]] =
        new FunctionK[F, [X] =>> G[H[X]]] {
          def apply[A](fa: F[A]): G[H[A]] =
            fk(fa).flatMap(fn2(_))
        }
    }

    def checkEscapeSkols[A](
        skols: List[Type.Var.Skolem],
        declared: Type,
        envTpes: Infer[List[Type]],
        a: A,
        onErr: NonEmptyList[Type] => Error
    )(
        fn: (A, NonEmptyList[Type.Var.Skolem]) => A
    ): Infer[A] =
      skols match {
        case Nil            => pure(a)
        case nel @ (h :: t) =>
          envTpes
            .flatMap(tail => getFreeTyVars(declared :: tail))
            .flatMap { escTvs =>
              // if the escaped set is empty, then filter(Set.empty) == Nil
              val badList = if (escTvs.isEmpty) Nil else nel.filter(escTvs)

              NonEmptyList.fromList(badList) match {
                case None =>
                  pure(fn(a, NonEmptyList(h, t)))
                case Some(badTvs) => fail(onErr(badTvs.map(Type.TyVar(_))))
              }
            }
      }

    def checkEscapeMetas[A](
        metas: List[Type.TyMeta],
        declared: Type,
        envTpes: Infer[List[Type]],
        a: A,
        onErr: NonEmptyList[Type] => Error
    )(
        fn: (A, NonEmptyList[Type.TyMeta]) => Infer[A]
    ): Infer[A] =
      metas match {
        case Nil            => pure(a)
        case nel @ (h :: t) =>
          envTpes
            .flatMap(tail => getExistentialMetas(declared :: tail))
            .flatMap { escTvs =>
              // if the escaped set is empty, then filter(Set.empty) == Nil
              val badList =
                if (escTvs.isEmpty) Nil
                else nel.filter(tm => escTvs(tm.toMeta))

              NonEmptyList.fromList(badList) match {
                case None =>
                  fn(a, NonEmptyList(h, t))
                case Some(badTvs) => fail(onErr(badTvs))
              }
            }
      }

    def subsUpper[F[_], G[_]: Functor](
        declared: Type,
        region: Region,
        envTpes: Infer[List[Type]]
    )(
        fn: (
            List[Type.TyMeta],
            Type.Rho
        ) => Infer[FunctionK[F, [X] =>> G[TypedExpr[X]]]]
    )(
        onErr: NonEmptyList[Type] => Error
    ): Infer[FunctionK[F, [X] =>> G[TypedExpr[X]]]] =
      for {
        (skols, metas, rho) <- skolemize(declared, region)
        // metas are existentials introduced from covariant positions in declared;
        // callers decide whether to quantify them, we only ensure they don't escape.
        coerce <- fn(metas, rho)
        // if there are no skolem variables, we can shortcut here, because empty.filter(fn) == empty
        resSkols <- checkEscapeSkols(skols, declared, envTpes, coerce, onErr) {
          (coerce, nel) => coerce.andThenMap(unskolemize(nel))
        }
        res <- checkEscapeMetas(metas, declared, envTpes, resSkols, onErr) {
          (coerce, _) =>
            // TODO maybe this function should go ahead and quantify
            pure(coerce)
        }
      } yield res

    def subsInstantiate(
        inferred: Type,
        declared: Type,
        left: Region,
        right: Region
    ): Option[Infer[TypedExpr.Coerce]] =
      (inferred match {
        case Type.ForAll(vars, inT) =>
          Type.instantiate(vars.iterator.toMap, inT, declared, Map.empty).map {
            case (_, subs) =>
              validateSubs(subs.toList, left, right)
                .as {
                  new FunctionK[TypedExpr, TypedExpr] {
                    def apply[A](te: TypedExpr[A]): TypedExpr[A] =
                      // we apply the annotation here and let Normalization
                      // instantiate. We could explicitly have
                      // instantiation TypedExpr where you pass the variables to set
                      TypedExpr.Annotation(te, declared)
                  }
                }
          }
        case _ =>
          None
      }).orElse(declared match {
        case Type.Exists(vars, inT) =>
          Type.instantiate(vars.iterator.toMap, inT, inferred, Map.empty).map {
            case (_, subs) =>
              validateSubs(subs.toList, left, right)
                .as {
                  new FunctionK[TypedExpr, TypedExpr] {
                    def apply[A](te: TypedExpr[A]): TypedExpr[A] =
                      // we apply the annotation here and let Normalization
                      // instantiate. We could explicitly have
                      // instantiation TypedExpr where you pass the variables to set
                      TypedExpr.Annotation(te, declared)
                  }
                }
          }
        case _ =>
          // TODO: we should be able to handle Dual quantification which could
          // solve more cases. The challenge is existentials and universals appear
          // on different sides, so cases where both need solutions can't be done
          // with the current method that only solves one direction now.
          None
      })
    // note, this is identical to subsCheckRho when declared is a Rho type
    def subsCheck(
        inferred: Type,
        declared: Type,
        left: Region,
        right: Region
    ): Infer[TypedExpr.Coerce] =
      subsInstantiate(inferred, declared, left, right) match {
        case Some(inf) => inf
        case None      =>
          // DEEP-SKOL rule
          subsUpper[TypedExpr, cats.Id](
            declared,
            right,
            pure(inferred :: Nil)
          ) { (_, rho) =>
            // subsCheck only needs a coercion; any meta writes happen during
            // subsCheckRho, and remaining existentials are handled by callers
            // that produce a TypedExpr (e.g. checkSigma/quantify).
            subsCheckRho(inferred, rho, left, right)
          } {
            Error.SubsumptionCheckFailure(inferred, declared, left, right, _)
          }
      }

    def inferForAll[A: HasRegion](
        tpes: NonEmptyList[(Type.Var.Bound, Kind)],
        expr: Expr[A]
    ): Infer[TypedExpr[A]] =
      for {
        (skols, t1) <- Expr.skolemizeVars(tpes, expr)(
          newSkolemTyVar(_, _, existential = false)
        )
        sigmaT <- inferSigma(t1)
        z <- zonkTypedExpr(sigmaT)
      } yield unskolemize(skols)(z)

    def unsolvedExistentials(ts: List[Type]): Infer[List[Type.Meta]] =
      Type
        .metaTvs(ts)
        .iterator
        .filter(_.existential)
        .toList
        .traverseFilter { m =>
          readMeta(m).map {
            case None    => Some(m)
            case Some(_) => None
          }
        }
    private val pureNone: Infer[None.type] = pure(None)

    def solvedExistentitals(
        lst: List[Type.Meta]
    ): Infer[SortedMap[Type.Meta, Type.Rho]] =
      lst
        .traverseFilter { m =>
          readMeta(m).flatMap {
            case Some(tau) =>
              // reset this meta
              clearMeta(m).as(Some((m, tau)))
            case None => pureNone
          }
        }
        .map(_.to(SortedMap))

    def unifyExistential(
        m: Type.Meta,
        values: List[(Type.Rho, Region)]
    ): Infer[Unit] = {
      def loop(values: List[(Type.Rho, Region)]): Infer[Unit] =
        values match {
          case (Type.TyMeta(m1), r1) :: tail =>
            readMeta(m1).flatMap {
              case Some(rho1) => loop((rho1, r1) :: tail)
              case None       => loop(tail)
            }
          case (h1, _) :: (t1 @ ((h2, _) :: _)) =>
            if (h1 == h2) loop(t1)
            else {
              // there are at least two distinct values, make a new meta skolem
              nextId.flatMap { id =>
                val skol = Type.Var.Skolem(
                  s"meta${m.id}",
                  m.kind,
                  existential = true,
                  id
                )
                val tpe = Type.TyVar(skol)
                writeMeta(m, tpe)
              }
            }
          case (h, _) :: Nil => writeMeta(m, h)
          case Nil           => unit
        }

      loop(values)
    }

    /** This idea here is that each branch may solve a different value of a
      * given existential type. If that happens, we can assign an existential
      * skolem variable to the type and move on.
      *
      * In this way, existentials are a kind of union type, and the skolem
      * represents a value later that will be exists x. ...
      */
    def unifyBranchExistentials(
        lst: List[Type.Meta],
        branches: NonEmptyList[(SortedMap[Type.Meta, Type.Rho], Region)]
    ): Infer[Unit] =
      lst.traverse_ { m =>
        val toUnify = branches.toList.mapFilter { case (s, region) =>
          s.get(m).map((_, region))
        }

        // despite the name, this can't fail
        unifyExistential(m, toUnify)
      }

    def maybeSimple[A: HasRegion](
        term: Expr[A]
    ): Option[Infer[TypedExpr[A]]] = {
      import Expr._
      term match {
        case Literal(lit, t) =>
          val tpe = Type.getTypeOf(lit)
          Some(pure(TypedExpr.Literal(lit, tpe, t)))
        case Local(name, tag) =>
          Some(
            lookupVarType((None, name), region(term))
              .map { vSigma =>
                TypedExpr.Local(name, vSigma, tag)
              }
          )
        case Global(pack, name, tag) =>
          Some(
            lookupVarType((Some(pack), name), region(term))
              .map { vSigma =>
                TypedExpr.Global(pack, name, vSigma, tag)
              }
          )
        case Annotation(term, tpe, _) =>
          Some(checkSigma(term, tpe))
        case _ =>
          None
      }
    }

    def validateSubs(
        list: List[(Type.Var.Bound, (Kind, Type))],
        left: Region,
        right: Region
    ): Infer[Unit] =
      list.parTraverse_ { case (boundVar, (kind, tpe)) =>
        kindOf(tpe, right).flatMap { k =>
          if (Kind.leftSubsumesRight(kind, k)) {
            unit
          } else {
            fail(
              Error.KindMismatch(
                Type.TyVar(boundVar),
                kind,
                tpe,
                k,
                left,
                right
              )
            )
          }
        }
      }

    def checkApply[A: HasRegion](
        fn: Expr[A],
        args: NonEmptyList[Expr[A]],
        tag: A,
        tpe: Type,
        tpeRegion: Region
    ): Infer[TypedExpr[A]] = {
      val infOpt = maybeSimple(fn).flatTraverse { inferFnExpr =>
        inferFnExpr.map { fnTe =>
          fnTe.getType match {
            case Type.Fun.SimpleUniversal(univ, inT, outT)
                if inT.length == args.length =>
              // see if we can instantiate the result type
              // if we can, we use that to fix the known parameters and continue
              Type
                .instantiate(univ.iterator.toMap, outT, tpe, Map.empty)
                .flatMap { case (frees, inst) =>
                  // if instantiate works, we know outT => tpe
                  if (inst.nonEmpty && frees.isEmpty) {
                    // we made some progress and there are no frees
                    // TODO: we could support frees it seems but
                    // it triggers failures in tests now
                    Some((fnTe, inT, frees, inst))
                  } else {
                    // We learned nothing
                    None
                  }
                }
            case _ =>
              None
          }
        }
      }

      infOpt.flatMap {
        case Some((fnTe, inT, frees, inst)) =>
          val regTe = region(tag)
          val validKinds: Infer[Unit] =
            validateSubs(inst.toList, region(fn), regTe)
          val instNoKind = inst.iterator
            .map { case (k, (_, t)) => (k, t) }
            .toMap[Type.Var, Type]

          val subIn = inT.map(Type.substituteVar(_, instNoKind))

          validKinds.parProductR {
            val remainingFree =
              NonEmptyList.fromList(
                frees.iterator.map { case (_, (k, b)) => (b, k) }.toList
              )

            remainingFree match {
              case None =>
                // we can fully instantiate
                args
                  .zip(subIn)
                  .parTraverse { case (e, t) =>
                    checkSigma(e, t)
                  }
                  .map { argsTE =>
                    TypedExpr.App(fnTe, argsTE, tpe, tag)
                  }

              // $COVERAGE-OFF$
              // case Some(remainingFree) =>
              case Some(_) =>
                // Currently we are only returning infOpt as Some when
                // there are no remaining free variables due to unit
                // tests not passing
                sys.error("unreachable")
              // $COVERAGE-ON$
              /*
                // some items are still free
                // TODO we could use the args to try to fix these
                val freeSub = frees.iterator
                  .collect { case (v, (_, t)) if v != t => (v, Type.TyVar(t)) }
                  .toMap[Type.Var, Type]

                val subIn2 = subIn.map(Type.substituteVar(_, freeSub))
                val tpe1 = Type.Quantified(
                  Type.Quantification.ForAll(remainingFree),
                  Type.Fun(subIn2, tpe)
                )
                val fn1 = Expr.Annotation(fn, tpe1, fn.tag)
                val inner = Expr.App(fn1, args, tag)
                checkSigma(inner, tpe)
               */
            }
          }
        case None =>
          tpe match {
            case rho: Type.Rho =>
              applyRhoExpect(fn, args, tag, Expected.Check((rho, tpeRegion)))
            case notRho =>
              val inner = Expr.App(fn, args, tag)
              checkSigma(inner, notRho)
          }
      }
    }

    def applyViaInst[A: HasRegion](
        fn: Expr[A],
        args: NonEmptyList[Expr[A]],
        tag: A
    ): Infer[Option[TypedExpr[A]]] =
      (maybeSimple(fn), args.traverse(maybeSimple(_))).mapN { (infFn, infArgs) =>
        infFn.flatMap { fnTe =>
          fnTe.getType match {
            case Type.Fun.SimpleUniversal(us, argsT, resT)
                if argsT.length == args.length =>
              infArgs.sequence
                .flatMap { argsTE =>
                  val argTypes = argsTE.map(_.getType)
                  // we can lift any quantification of the args
                  // outside of the function application
                  // We have to lift *before* substitution
                  val noshadows =
                    Type.freeBoundTyVars(resT :: argTypes.toList).toSet ++
                      us.iterator.map(_._1)
                  val (optQ, liftArgs) =
                    TypedExpr.liftQuantification(argsTE, noshadows)

                  val liftArgTypes = liftArgs.map(_.getType)
                  Type.instantiate(
                    us.toList.toMap,
                    Type.Tuple(argsT.toList),
                    Type.Tuple(liftArgTypes.toList),
                    optQ.fold(Map.empty[Type.Var.Bound, Kind])(
                      _.vars.toList.toMap
                    )
                  ) match {
                    case None =>
                      /*
                          println(s"can't instantiate: ${
                            Type.fullyResolvedDocument.document(fnTe.getType).render(80)
                          } to ${liftArgTypes.map(Type.fullyResolvedDocument.document(_).render(80))}")
                       */
                      pureNone
                    case Some((frees, inst)) =>
                      if (frees.nonEmpty) {
                        // TODO maybe we could handle this, but not yet
                        // seems like if the free vars are set to the same
                        // variable, then we can just lift it into the
                        // quantification
                        /*
                            println(s"remaining frees in ${
                              Type.fullyResolvedDocument.document(fnTe.getType).render(80)
                            } to ${liftArgTypes.map(Type.fullyResolvedDocument.document(_).render(80))}: $frees")
                         */
                        pureNone
                      } else {
                        val subMap =
                          inst.view.mapValues(_._2).toMap[Type.Var, Type]
                        val fnType0 = Type.Fun(liftArgTypes, resT)
                        val fnType1 = Type.substituteVar(fnType0, subMap)
                        val resType = Type.substituteVar(resT, subMap)

                        val resTe = TypedExpr.App(
                          TypedExpr.Annotation(fnTe, fnType1),
                          liftArgs,
                          resType,
                          tag
                        )

                        val maybeQuant = optQ match {
                          case Some(q) => TypedExpr.Generic(q, resTe)
                          case None    => resTe
                        }

                        pure(Some(maybeQuant))
                      }
                  }
                }
            case _ =>
              pureNone
          }
        }
      }.flatSequence

    def applyRhoExpect[A: HasRegion](
        fn: Expr[A],
        args: NonEmptyList[Expr[A]],
        tag: A,
        expect: Expected[(Type.Rho, Region)]
    ): Infer[TypedExpr.Rho[A]] =
      for {
        (typedFn, fnTRho) <- inferRho(fn)
        argsRegion = args.reduceMap(region[Expr[A]](_))
        (argT, resT) <- unifyFnRho(args.length, fnTRho, region(fn), argsRegion)
        typedArg <- args.zip(argT).parTraverse { case (arg, argT) =>
          checkSigma(arg, argT)
        }
        coerce <- instSigma(resT, expect, region(tag))
        res <- zonkTypedExpr(TypedExpr.App(typedFn, typedArg, resT, tag))
      } yield coerce(res)

    def checkAnnotated[A: HasRegion](
        inner: Expr[A],
        tpe: Type,
        tpeRegion: Region,
        expect: Expected[(Type.Rho, Region)]
    ): Infer[TypedExpr.Rho[A]] =
      (checkSigma(inner, tpe), instSigma(tpe, expect, tpeRegion))
        .parFlatMapN { (typedTerm, coerce) =>
          zonkTypedExpr(typedTerm).map(coerce(_))
        }

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
            res0 = TypedExpr.Local(name, vSigma, tag)
            res <- zonkTypedExpr(res0)
          } yield coerce(res)
        case Global(pack, name, tag) =>
          for {
            vSigma <- lookupVarType((Some(pack), name), region(term))
            coerce <- instSigma(vSigma, expect, region(term))
            res <- zonkTypedExpr(TypedExpr.Global(pack, name, vSigma, tag))
          } yield coerce(res)
        case Annotation(App(fn, args, tag), resT, annTag) =>
          applyViaInst(fn, args, tag)
            .flatMap {
              case Some(te) =>
                for {
                  co1 <- subsCheck(
                    te.getType,
                    resT,
                    region(tag),
                    region(annTag)
                  )
                  co2 <- instSigma(resT, expect, region(annTag))
                  z <- zonkTypedExpr(te)
                } yield co2(co1(z))
              case None =>
                (
                  checkApply(fn, args, tag, resT, region(annTag)),
                  instSigma(resT, expect, region(annTag))
                )
                  .parFlatMapN { (typedTerm, coerce) =>
                    zonkTypedExpr(typedTerm).map(coerce(_))
                  }
            }
        case App(fn, args, tag) =>
          applyViaInst(fn, args, tag)
            .flatMap {
              case Some(te) =>
                for {
                  co <- instSigma(te.getType, expect, HasRegion.region(tag))
                  z <- zonkTypedExpr(te)
                } yield co(z)
              case None =>
                expect match {
                  case Expected.Check((rho, reg)) =>
                    checkApply(fn, args, tag, rho, reg)
                  case inf @ Expected.Inf(_) =>
                    applyRhoExpect(fn, args, tag, inf)
                }
            }
        case Generic(tpes, in) =>
          for {
            unSkol <- inferForAll(tpes, in)
            // unSkol is not a Rho type, we need instantiate it
            coerce <- instSigma(unSkol.getType, expect, region(term))
          } yield coerce(unSkol)
        case Lambda(args, result, tag) =>
          expect match {
            case Expected.Check((expTy, rr)) =>
              for {
                // we know expTy is in weak-prenex form, and since Fn is covariant, bodyT must be
                // in weak prenex form
                (varsT, bodyT) <- unifyFnRho(
                  args.length,
                  expTy,
                  rr,
                  region(term)
                )
                bodyTRho <- assertRho(
                  bodyT,
                  s"expected ${show(expTy)} at $rr to be in weak-prenex form.",
                  region(result)
                )
                // the length of args and varsT must be the same because of unifyFnRho
                zipped = args.zip(varsT)
                namesVarsT = zipped.map { case ((n, _), t) => (n, t) }
                typedBody <- extendEnvNonEmptyList(namesVarsT) {
                  // TODO we are ignoring the result of subsCheck here
                  // should we be coercing a var?
                  //
                  // this comes from page 54 of the paper, but I can't seem to find examples
                  // where this will fail if we reverse (as we had for a long time), which
                  // indicates the testing coverage is incomplete
                  zipped.parTraverse_ {
                    case ((_, Some(tpe)), varT) =>
                      // since a -> b <:< c -> d means, b <:< d and c <:< a
                      // we check that the varT <:< tpe
                      subsCheck(varT, tpe, region(term), rr)
                    case ((_, None), _) => unit
                  } &>
                    checkRho(result, bodyTRho)
                }
              } yield TypedExpr.AnnotatedLambda(namesVarsT, typedBody, tag)
            case infer @ Expected.Inf(_) =>
              for {
                nameVarsT <- args.parTraverse {
                  case (n, Some(tpe)) =>
                    // TODO do we need to narrow or instantiate tpe?
                    pure((n, tpe))
                  case (n, None) =>
                    // all functions args of kind type
                    newMeta.map((n, _))
                }
                (typedBody, bodyT) <- extendEnvNonEmptyList(nameVarsT)(
                  inferRho(result)
                )
                _ <- infer.set(
                  (Type.Fun(nameVarsT.map(_._2), bodyT), region(term))
                )
              } yield TypedExpr.AnnotatedLambda(nameVarsT, typedBody, tag)
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
            val rhsBody = rhs match {
              case Expr.Annotated(tpe) =>
                extendEnv(name, tpe) {
                  checkSigma(rhs, tpe).parProduct(typeCheckRho(body, expect))
                }
              case notAnnotated =>
                newMeta // the kind of a let value is a Type
                  .flatMap { rhsTpe =>
                    extendEnv(name, rhsTpe) {
                      for {
                        // the type variable needs to be unified with varT
                        // note, varT could be a sigma type, it is not a Tau or Rho
                        typedRhs <- inferSigmaMeta(
                          notAnnotated,
                          Some((name, rhsTpe, region(notAnnotated)))
                        )
                        varT = typedRhs.getType
                        // we need to overwrite the metavariable now with the full type
                        typedBody <- extendEnv(name, varT)(
                          typeCheckRho(body, expect)
                        )
                      } yield (typedRhs, typedBody)
                    }
                  }
            }

            rhsBody.map { case (rhs, body) =>
              // TODO: a more efficient algorithm would do this top down
              // for each top level TypedExpr and build it bottom up.
              // we could do this after all typechecking is done
              val frees = TypedExpr.freeVars(rhs :: Nil)
              val isRecursive = RecursionKind.recursive(frees.contains(name))
              TypedExpr.Let(name, rhs, body, isRecursive, tag)
            }
          } else {
            // In this branch, we typecheck the rhs *without* name in the environment
            // so any recursion in this case won't typecheck, and shadowing rules are
            // in place
            val rhsBody = rhs match {
              case Expr.Annotated(tpe) =>
                // check in parallel so we collect more errors
                checkSigma(rhs, tpe)
                  .parProduct(
                    extendEnv(name, tpe)(typeCheckRho(body, expect))
                  )
              case _ =>
                // we don't know the type of rhs, so we have to infer then check the body
                for {
                  typedRhs <- inferSigma(rhs)
                  typedBody <- extendEnv(name, typedRhs.getType)(
                    typeCheckRho(body, expect)
                  )
                } yield (typedRhs, typedBody)
            }

            rhsBody.map { case (rhs, body) =>
              // Note: in this branch, we know isRecursive.isRecursive == false
              TypedExpr.Let(
                name,
                rhs,
                body,
                recursive = RecursionKind.NonRecursive,
                tag
              )
            }
          }
        case Annotation(term, tpe, tag) =>
          val inner = term match {
            case Match(arg, branches, mtag) =>
              // We push the Annotation down to help with
              // existential type checking where each branch
              // has a different type
              Match(
                arg,
                branches.map { case (p, r) =>
                  // we have to put the tag to be r.tag
                  // because that's where the regions come from
                  (p, Annotation(r, tpe, r.tag))
                },
                mtag
              )
            case notMatch => notMatch
          }

          @inline def default = checkAnnotated(inner, tpe, region(tag), expect)

          tpe match {
            case rho: Type.Rho =>
              // expect can directly hold this type without instantiation
              maybeSimple(inner) match {
                case Some(simp) =>
                  // we can check direct instantiation
                  simp.flatMap { te =>
                    te.getType match {
                      case Type.ForAll(fas, in) =>
                        Type.instantiate(
                          fas.iterator.toMap,
                          in,
                          rho,
                          Map.empty
                        ) match {
                          case Some((frees, subs)) if frees.isEmpty =>
                            // we know that substituting in gives rho
                            // check kinds
                            // substitute
                            // see if substitute rho with subs <:< expected
                            // else set inferred value
                            val validKinds: Infer[Unit] =
                              validateSubs(
                                subs.toList,
                                region(term),
                                region(tag)
                              )

                            validKinds.parProductR(expect match {
                              case Expected.Check((r1, reg1)) =>
                                for {
                                  co <- subsCheckRho2(
                                    rho,
                                    r1,
                                    region(term),
                                    reg1
                                  )
                                  z <- zonkTypedExpr(
                                    TypedExpr.Annotation(te, rho)
                                  )
                                } yield co(z)
                              case inf @ Expected.Inf(_) =>
                                for {
                                  _ <- inf.set((rho, region(term)))
                                  ks <- checkedKinds
                                } yield TypedExpr.coerceRho(rho, ks)(te)
                            })
                          case _ =>
                            default
                        }
                      case _ =>
                        default
                    }
                  }

                case None => default
              }
            case _ =>
              // expect can't hold a sigma type
              default
          }

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
          // We infer the scrutinee once, then skolemize only outer existentials
          // so all branches share the same hidden type while keeping foralls intact.
          inferSigma(term)
            .flatMap { tsigma =>
              skolemizeExistsOnly(tsigma.getType).flatMap { case (exSkols, t1) =>
                val check = Expected.Check((t1, region(term)))
                val unskol = unskolemizeExists(exSkols)

                expect match {
                  case Expected.Check((resT, _)) =>
                    for {
                      rest <- envTypes
                      unknownExs <- unsolvedExistentials(resT :: rest)
                      tbranches <-
                        if (unknownExs.isEmpty) {
                          // in the common case there are no existentials save effort
                          branches.parTraverse { case (p, r) =>
                            // note, resT is in weak-prenex form, so this call is permitted
                            checkBranch(p, check, r, resT)
                          }
                        } else {
                          for {
                            tbranches <- branches.parTraverse { case (p, r) =>
                              // note, resT is in weak-prenex form, so this call is permitted
                              checkBranch(p, check, r, resT)
                                .product(
                                  solvedExistentitals(unknownExs).map(
                                    (_, region(r))
                                  )
                                )
                            }
                            _ <- unifyBranchExistentials(
                              unknownExs,
                              tbranches.map(_._2)
                            )
                          } yield tbranches.map(_._1)
                        }
                    } yield unskol(TypedExpr.Match(tsigma, tbranches, tag))
                  case infer @ Expected.Inf(_) =>
                    for {
                      tbranches <- branches.parTraverse { case (p, r) =>
                        inferBranch(p, check, r)
                      }
                      (rho, regRho, resBranches) <- widenBranches(tbranches)
                      _ <- infer.set((rho, regRho))
                    } yield unskol(TypedExpr.Match(tsigma, resBranches, tag))
                }
              }
            }
      }
    }

    def widenBranches[A: HasRegion](
        branches: NonEmptyList[(Pattern, (TypedExpr.Rho[A], Type.Rho))]
    ): Infer[(Type.Rho, Region, NonEmptyList[(Pattern, TypedExpr.Rho[A])])] = {

      def maxBy[M[_]: Monad, B](head: B, tail: List[B])(
          gteq: (B, B) => M[Boolean]
      ): M[B] =
        tail match {
          case Nil       => Monad[M].pure(head)
          case h :: tail =>
            gteq(head, h)
              .flatMap { keep =>
                val next = if (keep) head else h
                maxBy(next, tail)(gteq)
              }
        }

      def gtEq[K](
          left: (TypedExpr[A], K),
          right: (TypedExpr[A], K)
      ): Infer[Boolean] = {
        val leftTE = left._1
        val rightTE = right._1
        val lt = leftTE.getType
        val lr = region(leftTE)
        val rt = rightTE.getType
        val rr = region(rightTE)
        // left >= right if right subsumes left
        subsCheck(rt, lt, rr, lr).peek
          .flatMap {
            case Right(_) => pure(true)
            case Left(_)  =>
              // maybe the other way around
              subsCheck(lt, rt, lr, rr).peek
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

      val withIdx = branches.zipWithIndex.map { case ((p, (te, tpe)), idx) =>
        (te, (p, tpe, idx))
      }

      for {
        (maxRes, (_, resTRho, maxIdx)) <- maxBy(
          withIdx.head,
          withIdx.tail
        )((a, b) => gtEq(a, b))
        resRegion = region(maxRes)
        resBranches <- withIdx.parTraverse { case (te, (p, tpe, idx)) =>
          if (idx != maxIdx) {
            // unfortunately we have to check each branch again to get the correct coerce
            subsCheckRho2(tpe, resTRho, region(te), resRegion)
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
        (pattern, bindings) <- typeCheckPattern(p, sigma, region(res))
        tres <- extendEnvList(bindings)(checkRho(res, resT))
      } yield (pattern, tres)

    def inferBranch[A: HasRegion](
        p: Pattern,
        sigma: Expected.Check[(Type, Region)],
        res: Expr[A]
    ): Infer[(Pattern, (TypedExpr.Rho[A], Type.Rho))] =
      for {
        patBind <- typeCheckPattern(p, sigma, region(res))
        (pattern, bindings) = patBind
        // inferRho returns a TypedExpr.Rho (which is only an alias)
        res <- extendEnvList(bindings)(inferRho(res))
      } yield (pattern, res)

    /** patterns can be a sigma type, not neccesarily a rho/tau return a list of
      * bound names and their (sigma) types
      *
    * TODO: Pattern needs to have a region for each part (https://github.com/johnynek/bosatsu/issues/132)
      */
    def typeCheckPattern(
        pat: Pattern,
        sigma: Expected.Check[(Type, Region)],
        reg: Region
    ): Infer[(Pattern, List[(Bindable, Type)])] = {
      pat match {
        case GenPattern.WildCard     => Infer.pure((pat, Nil))
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
          val names = items.collect {
            case GenPattern.StrPart.NamedStr(n)  => (n, tpe)
            case GenPattern.StrPart.NamedChar(n) => (n, Type.CharType)
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
                  tpeA <- newMeta // lists +* -> *
                  listA = Type.TyApply(Type.ListType, tpeA)
                  _ <- unifyType(listA, sigma.value._1, reg, sigma.value._2)
                } yield tpeA
            }

          for {
            tpeA <- tpeOfList
            listA = Type.TyApply(Type.ListType, tpeA)
            inners <- items.parTraverse(checkItem(tpeA, listA, _))
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
            // we need to be able to widen sigma into tpe
            _ <- subsCheck(sigma.value._1, tpe, sigma.value._2, reg)
          } yield (p1, binds)
        case GenPattern.PositionalStruct(nm, args) =>
          for {
            params <- instDataCon(nm, sigma.value._1, reg, sigma.value._2)
            // we need to do a pattern linting phase and probably error
            // if the pattern arity does not match the arity of the constructor
            // but we don't want to error type-checking since we want to show
            // the maximimum number of errors to the user
            envs <- args.zip(params).parTraverse { case (p, t) =>
              checkPat(p, t, reg)
            }
            pats = envs.map(_._1)
            bindings = envs.map(_._2)
          } yield (GenPattern.PositionalStruct(nm, pats), bindings.flatten)
        case u @ GenPattern.Union(h, t) =>
          (
            typeCheckPattern(h, sigma, reg),
            t.parTraverse(typeCheckPattern(_, sigma, reg))
          ).parMapN { case ((h, binds), neList) =>
            val pat = GenPattern.Union(h, neList.map(_._1))
            val allBinds = NonEmptyList(binds, (neList.map(_._2).toList))
            identicalBinds(u, allBinds, reg).as((pat, binds))
          }.flatten
      }
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
            bs.toList.parTraverse_ { v =>
              val bmh = bm.head
              val bmt = bm.tail
              val tpe = bmh(v)
              bmt.parTraverse_ { m2 =>
                val tpe2 = m2(v)
                unifyType(tpe, tpe2, reg, reg)
              }
            }
          } else fail(Error.UnionPatternBindMismatch(u, nel, reg))
      }

    // TODO: we should be able to derive a region for any pattern (https://github.com/johnynek/bosatsu/issues/132)
    def checkPat(
        pat: Pattern,
        sigma: Type,
        reg: Region
    ): Infer[(Pattern, List[(Bindable, Type)])] =
      typeCheckPattern(pat, Expected.Check((sigma, reg)), reg)

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

        // It seems like maybe we should be checking someting about the kinds
        // to see if this constructor is well kinded, but remember, this is
        // for a pattern match, where we have already type checked the scrutinee
        // and the type constructor is well-kinded by the checks done at kind
        // inference time.
        def loop(
            revArgs: List[(Type.Var.Bound, Kind.Arg)],
            leftKind: Kind,
            sigma: Type
        ): Infer[Map[Type.Var, Type]] =
          (revArgs, sigma) match {
            case (Nil, tpe) =>
              for {
                _ <- unifyType(thisTpe, tpe, reg, sigmaRegion)
              } yield Map.empty
            case ((v0, k) :: vs, Type.TyApply(left, right)) =>
              for {
                rest <- loop(vs, Kind.Cons(k, leftKind), left)
              } yield rest.updated(v0, right)
            case (_, fa: Type.Quantified) =>
              // we have to instantiate a rho type
              instantiate(fa)
                .flatMap { case (_, faRho) =>
                  // TODO: it seems like we shouldn't ignore the existential skolems
                  loop(revArgs, leftKind, faRho)
                }
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
        ): Type =
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
                  // TODO: I think we can push down existentials too
                  Type.forAll(bs, Type.apply1(l, nextRight))
                case rho /*: Type.Rho */ =>
                  Type.apply1(rho, nextRight)
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
                  // TODO: we could possibly have an existential here?
                  Type.forAll(bs, Type.apply1(l, right))
                case rho /*: Type.Rho */ =>
                  Type.apply1(rho, right)
              }
            case _ =>
              Type.forAll(revForAlls.reverse, sigma)
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

    // invariant: if meta.isDefined then e is not Expr.Annotated
    def inferSigmaMeta[A: HasRegion](
        e: Expr[A],
        meta: Option[(Identifier, Type.TyMeta, Region)]
    ): Infer[TypedExpr[A]] = {
      def unifySelf(tpe: Type.Rho): Infer[Map[Name, Type]] =
        meta match {
          case None             => getEnv
          case Some((nm, m, r)) =>
            (unify(tpe, m, region(e), r) *> getEnv).map { envTys =>
              // we have to remove the recursive binding from the environment
              envTys - ((None, nm))
            }
        }

      /** if meta is Some, it is because it recursive, but those are almost
        * always functions, so we can at least fix the arity of the function.
        */
      val init: Infer[Unit] =
        meta match {
          case Some((_, tpe, rtpe)) =>
            def maybeUnified(e: Expr[A]): Infer[Unit] =
              e match {
                case Expr.Lambda(args, res, _) =>
                  unifyFnRho(
                    args.length,
                    tpe,
                    rtpe,
                    region(e) - region(res)
                  ).void
                case _ =>
                  // we just have to wait to infer
                  unit
              }

            maybeUnified(e)
          case None => unit
        }

      for {
        _ <- init
        (rho, expTyRho) <- inferRho(e)
        q <- quantify(unifySelf(expTyRho), rho)
      } yield q
    }

    def quantify[A](
        env: Infer[Map[Name, Type]],
        rho: TypedExpr.Rho[A]
    ): Infer[TypedExpr[A]] =
      for {
        e <- env
        zrho <- zonkTypedExpr(rho)
        q <- TypedExpr.quantify(e, zrho, readMeta, writeMeta)
      } yield q

    // allocate this once and reuse
    private val envTypes = getEnv.map(_.values.toList)

    def quantifyMetas(
        metas: List[Type.TyMeta]
    ): FunctionK[TypedExpr, [X] =>> Infer[TypedExpr[X]]] =
      NonEmptyList.fromList(metas) match {
        case None =>
          new FunctionK[TypedExpr, [X] =>> Infer[TypedExpr[X]]] {
            def apply[A](fa: TypedExpr[A]): Infer[TypedExpr[A]] = pure(fa)
          }
        case Some(nel) =>
          new FunctionK[TypedExpr, [X] =>> Infer[TypedExpr[X]]] {
            def apply[A](fa: TypedExpr[A]): Infer[TypedExpr[A]] = {
              // all these metas can be set to Var
              val used: Set[Type.Var.Bound] = fa.allBound
              val aligned = Type.alignBinders(nel, used)
              val bound = aligned.toList.traverseFilter { case (m, n) =>
                val meta = m.toMeta
                if (meta.existential)
                  writeMeta(m.toMeta, Type.TyVar(n)).as(Some((n, meta.kind)))
                else pure(None)
              }
              // we only need to zonk after doing a write:
              // it isnot clear that zonkMeta correctly here because the existentials
              // here have been realized to Type.Var now, and and meta pointing at them should
              // become visible (no longer hidden)
              val zFn = Type.zonk(
                metas.iterator
                  .map(_.toMeta)
                  .filter(_.existential)
                  .to(SortedSet),
                readMeta,
                writeMeta
              )
              (bound, TypedExpr.zonkMeta(fa)(zFn))
                .mapN { (typeArgs, r) =>
                  TypedExpr.quantVars(forallList = Nil, existList = typeArgs, r)
                }
            }
          }
      }

    def checkSigma[A: HasRegion](t: Expr[A], tpe: Type): Infer[TypedExpr[A]] = {
      val regionT = region(t)
      for {
        check <- subsUpper[[X] =>> (Expr[X], HasRegion[X]), Infer](
          tpe,
          regionT,
          envTypes
        ) { (metas, rho) =>
          val cRho = checkRhoK(rho)
          if (tpe == rho) {
            // we don't need to zonk here
            pure(cRho)
          } else {
            // we need to zonk before we unskolemize because some of the metas could be skolems
            pure(
              cRho
                .andThenFlatMap[TypedExpr](zonkTypeExprK)
                .andThenFlatMap[TypedExpr](quantifyMetas(metas))
            )
          }
        } { badTvs =>
          Error.NotPolymorphicEnough(tpe, t, badTvs, regionT)
        }
        te <- check((t, implicitly[HasRegion[A]]))
      } yield te
    }

    /** invariant: rho needs to be in weak-prenex form
      */
    def checkRho[A: HasRegion](
        t: Expr[A],
        rho: Type.Rho
    ): Infer[TypedExpr.Rho[A]] =
      typeCheckRho(t, Expected.Check((rho, region(t))))

    // same as checkRho but as a FunctionK
    def checkRhoK(
        rho: Type.Rho
    ): FunctionK[[X] =>> (Expr[X], HasRegion[X]), [X] =>> Infer[
      TypedExpr.Rho[X]
    ]] =
      new FunctionK[[X] =>> (Expr[X], HasRegion[X]), [X] =>> Infer[
        TypedExpr.Rho[X]
      ]] {
        def apply[A](fa: (Expr[A], HasRegion[A])): Infer[TypedExpr[A]] =
          checkRho(fa._1, rho)(using fa._2)
      }

    /** recall a rho type never has a top level Forall
      */
    def inferRho[A: HasRegion](
        t: Expr[A]
    ): Infer[(TypedExpr.Rho[A], Type.Rho)] =
      for {
        ref <- initRef[A, (Type.Rho, Region)](t)
        expr <- typeCheckRho(t, Expected.Inf(ref))
        // we don't need this ref, and it does not escape, so reset
        eitherTpe <- lift(ref.get <* ref.reset)
        tpe <- eitherTpe match {
          case Right(rho) => pure(rho._1)
          case Left(err)  => fail(err)
        }
      } yield (expr, tpe)
  }

  private def recursiveTypeCheck[A: HasRegion](
      name: Bindable,
      expr: Expr[A]
  ): Infer[TypedExpr[A]] =
    // values are of kind Type
    expr match {
      case Expr.Annotated(tpe) =>
        extendEnv(name, tpe)(checkSigma(expr, tpe))
      case notAnnotated =>
        newMeta.flatMap { tpe =>
          extendEnv(name, tpe)(
            typeCheckMeta(notAnnotated, Some((name, tpe, region(notAnnotated))))
          )
        }
    }

  def typeCheck[A: HasRegion](t: Expr[A]): Infer[TypedExpr[A]] =
    typeCheckMeta(t, None)

  private def unskolemize(
      skols: NonEmptyList[Type.Var.Skolem]
  ): TypedExpr.Coerce =
    new FunctionK[TypedExpr, TypedExpr] {
      def apply[A](te: TypedExpr[A]) = {
        // now replace the skols with generics
        val used = te.allBound
        val aligned = Type.alignBinders(skols, used)
        val te2 =
          substTyExpr(skols, aligned.map { case (_, b) => Type.TyVar(b) }, te)
        TypedExpr.forAll(aligned.map { case (s, b) => (b, s.kind) }, te2)
      }
    }

  private def unskolemizeExists(
      skols: List[Type.Var.Skolem]
  ): TypedExpr.Coerce =
    NonEmptyList.fromList(skols) match {
      case None        => FunctionK.id[TypedExpr]
      case Some(skols) =>
        new FunctionK[TypedExpr, TypedExpr] {
          def apply[A](te: TypedExpr[A]) = {
            val freeSkols =
              te.freeTyVars.iterator.collect {
                case s: Type.Var.Skolem => s
              }.toSet
            val toQuant = skols.filter(freeSkols)
            NonEmptyList.fromList(toQuant) match {
              case None => te
              case Some(skols) =>
                // now replace the skols with generics
                val used = te.allBound
                val aligned = Type.alignBinders(skols, used)
                val te2 = substTyExpr(
                  skols,
                  aligned.map { case (_, b) => Type.TyVar(b) },
                  te
                )
                TypedExpr.quantVars(
                  forallList = Nil,
                  existList = aligned.toList.map { case (s, b) => (b, s.kind) },
                  te2
                )
            }
          }
        }
    }

  // Invariant: if optMeta.isDefined then t is not Expr.Annotated
  private def typeCheckMeta[A: HasRegion](
      t: Expr[A],
      optMeta: Option[(Identifier, Type.TyMeta, Region)]
  ): Infer[TypedExpr[A]] = {
    def run(t: Expr[A]) = inferSigmaMeta(t, optMeta).flatMap(zonkTypedExpr)

    val optSkols = t match {
      case Expr.Generic(vs, e) =>
        Some(
          Expr.skolemizeVars(vs, e)(newSkolemTyVar(_, _, existential = false))
        )
      case _ => None
    }

    optSkols match {
      case None          => run(t)
      case Some(replace) =>
        for {
          mt <- replace
          (skols, t1) = mt
          te <- run(t1)
        } yield unskolemize(skols)(te)
    }
  }

  def extendEnv[A](varName: Bindable, tpe: Type)(of: Infer[A]): Infer[A] =
    extendEnvNonEmptyList(NonEmptyList.one((varName, tpe)))(of)

  def extendEnvList[A](
      bindings: List[(Bindable, Type)]
  )(of: Infer[A]): Infer[A] =
    NonEmptyList.fromList(bindings) match {
      case Some(nel) => extendEnvNonEmptyList(nel)(of)
      case None      => of
    }

  def extendEnvNonEmptyList[A](bindings: NonEmptyList[(Bindable, Type)])(
      of: Infer[A]
  ): Infer[A] =
    Infer.Impl.ExtendEnvs(bindings.map { case (n, t) => ((None, n), t) }, of)

  private def extendEnvListPack[A](
      pack: PackageName,
      nameTpe: NonEmptyList[(Bindable, Type)]
  )(of: Infer[A]): Infer[A] =
    Infer.Impl.ExtendEnvs(
      nameTpe.map { case (name, tpe) => ((Some(pack), name), tpe) },
      of
    )

  /** Packages are generally just lists of lets, this allows you to infer the
    * scheme for each in the context of the list
    */
  def typeCheckLets[A: HasRegion](
      pack: PackageName,
      ls: List[(Bindable, RecursionKind, Expr[A])],
      externals: Map[Bindable, (Type, Region)]
  ): Infer[List[(Bindable, RecursionKind, TypedExpr[A])]] = {
    // Group together lets that don't include each other to get more type errors
    // if we can
    type G = NonEmptyChain[(Bindable, RecursionKind, Expr[A])]

    def run(
        groups: List[G]
    ): Infer[List[(Bindable, RecursionKind, TypedExpr[A])]] =
      groups match {
        case Nil           => Infer.pure(Nil)
        case group :: tail =>
          for {
            groupChain <- group.parTraverse { case (name, rec, expr) =>
              (if (rec.isRecursive) recursiveTypeCheck(name, expr)
               else typeCheck(expr))
                .map(te => (name, rec, te))
            }
            glist = groupChain.toNonEmptyList
            tailRes <- extendEnvListPack(
              pack,
              glist.map { case (b, _, te) =>
                (b, te.getType)
              }
            ) {
              run(tail)
            }
          } yield glist.head :: glist.tail ::: tailRes
      }

    val groups: List[G] =
      ListUtil.greedyGroup(ls)(item => NonEmptyChain.one(item)) {
        case (bs, item @ (_, _, expr)) =>
          val dependsOnGroup =
            expr.globals.iterator.exists { case Expr.Global(p, n1, _) =>
              (p == pack) && bs.exists(_._1 == n1)
            }
          if (dependsOnGroup) None // we can't run in parallel
          else Some(bs :+ item)
      }

    val checkExternals =
      GetEnv.flatMap { env =>
        externals.toList
          .sortBy { case (_, (_, region)) => region }
          .parTraverse_ { case (_, (t, region)) =>
            env.getKind(t, region) match {
              case Right(Kind.Type)              => unit
              case Right(cons @ Kind.Cons(_, _)) =>
                fail(Error.KindExpectedType(t, cons, region))
              case Left(err) => fail(err)
            }
          }
      }

    run(groups).parProductL(checkExternals)
  }

  /** This is useful to testing purposes.
    *
    * Given types a and b, can we substitute a for for b
    */
  def substitutionCheck(a: Type, b: Type, ra: Region, rb: Region): Infer[Unit] =
    subsCheck(a, b, ra, rb).void
}
