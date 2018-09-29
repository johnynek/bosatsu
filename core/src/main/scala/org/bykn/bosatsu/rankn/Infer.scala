package org.bykn.bosatsu.rankn
import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._

import org.bykn.bosatsu.{Pattern => GenPattern}

sealed abstract class Infer[+A] {
  import Infer.Error

  def run(env: Infer.Env): RefSpace[Either[Error, A]]

  final def flatMap[B](fn: A => Infer[B]): Infer[B] =
    Infer.Impl.FlatMap(this, fn)

  final def runVar(v: Map[String, Type], tpes: Map[String, Infer.Cons]): RefSpace[Either[Error, A]] =
    Infer.Env.init(v, tpes).flatMap(run(_))

  final def runFully(v: Map[String, Type], tpes: Map[String, Infer.Cons]): Either[Error, A] =
    runVar(v, tpes).run.value
}

object Infer {

  type Pattern = GenPattern[String]

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


  type Cons = (List[Type.Var], List[Type], Type.Const.Defined)

  case class Env(uniq: Ref[Long],
    vars: Map[String, Type],
    typeCons: Map[String, Cons])

  object Env {
    def empty: RefSpace[Env] =
      init(Map.empty, Map.empty)

    def init(vars: Map[String, Type], tpes: Map[String, Cons]): RefSpace[Env] =
      RefSpace.newRef(0L).map(Env(_, vars, tpes))
  }

  def getEnv: Infer[Map[String, Type]] = GetEnv

  def lift[A](rs: RefSpace[A]): Infer[A] =
    Lift(rs.map(Right(_)))

  def fail(err: Error): Infer[Nothing] =
    Lift(RefSpace.pure(Left(err)))

  def pure[A](a: A): Infer[A] =
    Lift(RefSpace.pure(Right(a)))

  def defer[A](tc: => Infer[A]): Infer[A] =
    Defer(() => tc)

  def require(b: Boolean, err: => Error): Infer[Unit] =
    if (b) pure(()) else fail(err)

  // Fails if v is not in the env
  def lookupVarType(v: String): Infer[Type] =
    getEnv.flatMap { env =>
      env.get(v) match {
        case None => fail(Error.VarNotInScope(v, env))
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

    case class NotUnifiable(left: Type, right: Type) extends TypeError {
      def message = s"$left cannot be unified with $right"
    }

    case class NotPolymorphicEnough(tpe: Type, in: Term) extends TypeError {
      def message = s"type $tpe not polymorphic enough in $in"
    }

    case class SubsumptionCheckFailure(inferred: Type, declared: Type) extends TypeError {
      def message = s"subsumption check failed: $inferred $declared"
    }

    /**
     * These are errors that prevent typing due to unknown names,
     * They could be caught in a phase that collects all the naming errors
     */
    sealed abstract class NameError extends Error

    // This could be a user error if we don't check scoping before typing
    case class VarNotInScope(varName: String, vars: Map[String, Type]) extends NameError {
      def message = s"$varName not in scope: $vars"
    }

    // This could be a user error if we don't check scoping before typing
    case class UnexpectedBound(v: Type.Var.Bound, in: Type) extends NameError {
      def message = s"unexpected bound ${v.name} in unification with $in"
    }

    case class UnknownConstructor(name: String, env: Env) extends NameError {
      def message = s"unknown Constructor $name. Known: ${env.typeCons.keys.toList.sorted}"
    }

    /**
     * These can only happen if the compiler has bugs at some point
     */
    sealed abstract class InternalError extends Error
    case class UnexpectedMeta(m: Type.Meta, in: Type) extends InternalError {
      def message = s"meta $m occurs in $in and should not"
    }

    // This is a logic error which should never happen
    case class InferPatIncomplete(pattern: Pattern) extends InternalError {
      def message = s"inferPat not complete for $pattern"
    }
    case class InferIncomplete(method: String, term: Term) extends InternalError {
      def message = s"$method not complete for $term"
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

    case class Defer[A](tc: () => Infer[A]) extends Infer[A] {
      lazy val finalInfer = {
        def loop(tc: Infer[A]): Infer[A] =
          tc match {
            case Defer(next) => loop(next())
            case nonDefer => nonDefer
          }

        loop(tc())
      }

      def run(env: Env) = finalInfer.run(env)
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
        def step(a: A): RefSpace[Either[A, Either[Error, B]]] =
          fn(a).run(env).map {
            case Left(err) => Right(Left(err))
            case Right(Left(a)) => Left(a)
            case Right(Right(a)) => Right(Right(a))
          }

        Monad[RefSpace].tailRecM(init)(step _)
      }
    }

    case object GetEnv extends Infer[Map[String, Type]] {
      def run(env: Env) = RefSpace.pure(Right(env.vars))
    }

    case class GetDataCons(name: String) extends Infer[Cons] {
      def run(env: Env) =
        RefSpace.pure(
          env.typeCons.get(name) match {
            case None =>
              Left(Error.UnknownConstructor(name, env))
            case Some(res) =>
              Right(res)
          })
    }

    case class ExtendEnvs[A](vt: List[(String, Type)], in: Infer[A]) extends Infer[A] {
      def run(env: Env) = in.run(env.copy(vars = vt.foldLeft(env.vars)(_ + _)))
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

    def getMetaTyVars(tpes: List[Type]): Infer[Set[Type.Meta]] =
      tpes.traverse(zonkType).map(Type.metaTvs(_))

    def tyVarBinders(tpes: List[Type], acc: Set[Type.Var]): Set[Type.Var] =
      tpes match {
        case Nil => acc
        case Type.ForAll(tvs, body) :: rest =>
          tyVarBinders(rest, acc ++ tvs.toList)
        case Type.TyApply(arg, res) :: rest =>
          tyVarBinders(arg :: res :: rest, acc)
        case _ :: rest => tyVarBinders(rest, acc)
      }

    /**
     * Quantify over the specified type variables (all flexible)
     */
    def quantify(forAlls: List[Type.Meta], rho: Type.Rho): Infer[Type] =
      forAlls match {
        case Nil =>
          // this case is not really discussed in the paper
          zonkType(rho)
        case ne@(h :: tail) =>
          val used = tyVarBinders(List(rho), Set.empty)
          // on 2.11 without the iterator this seems to run forever
          def newBinders = Type.allBinders.iterator.filterNot(used)
          val newBindersNE =
            NonEmptyList.fromListUnsafe(newBinders.take(forAlls.size).toList)
          val bound = ne
            .iterator
            .zip(newBinders)
            .toStream
            .traverse_ { case (m, n) =>
              writeMeta(m, Type.TyVar(n))
            }
          (bound *> zonkType(rho)).map(Type.ForAll(newBindersNE, _))
      }

    def skolemize(t: Type): Infer[(List[Type.Var], Type.Rho)] =
      t match {
        case Type.ForAll(tvs, ty) =>
          // Rule PRPOLY
          for {
            sks1 <- tvs.traverse(newSkolemTyVar)
            sksT = sks1.map(Type.TyVar(_))
            sks2ty <- skolemize(substTy(tvs, sksT, ty))
            (sks2, ty2) = sks2ty
          } yield (sks1.toList ::: sks2, ty2)
        case Type.Fun(argTy, resTy) =>
          skolemize(resTy).map {
            case (sks, resTy) =>
              (sks, Type.Fun(argTy, resTy))
          }
          // Rule PRFUN
        case other =>
          // Rule PRMONO
          pure((Nil, other))
      }

    def freeTyVars(ts: List[Type]): Set[Type.Var] = {
      @annotation.tailrec
      def go(ts: List[Type], bound: Set[Type.Var], acc: Set[Type.Var]): Set[Type.Var] =
        ts match {
          case Nil => acc
          case Type.TyVar(tv) :: rest =>
            if (bound(tv)) go(rest, bound, acc)
            else go(rest, bound, acc + tv)
          case Type.TyApply(a, b) :: rest => go(a :: b :: rest, bound, acc)
          case Type.ForAll(tvs, ty) :: rest => go(ty :: rest, bound ++ tvs.toList, acc)
          case (Type.TyMeta(_) | Type.TyConst(_)) :: rest => go(rest, bound, acc)
        }

      ts.foldLeft(Set.empty[Type.Var]) { (acc, t) =>
        go(t :: Nil, Set.empty, acc)
      }
    }

    def getFreeTyVars(ts: List[Type]): Infer[Set[Type.Var]] =
      ts.traverse(zonkType).map(freeTyVars(_))

    def zonkType(t: Type): Infer[Type] =
      t match {
        case Type.ForAll(ns, ty) =>
          zonkType(ty).map(Type.ForAll(ns, _))
        case Type.TyApply(on, arg) =>
          (zonkType(on), zonkType(arg)).mapN(Type.TyApply(_, _))
        case c@Type.TyConst(_) => pure(c)
        case v@Type.TyVar(_) => pure(v)
        case t@Type.TyMeta(m) =>
          readMeta(m).flatMap {
            case None => pure(t)
            case Some(ty) =>
              zonkType(ty).flatMap { ty1 =>
                // short out multiple hops (I guess an optimization?)
                writeMeta(m, ty1) *> pure(ty1)
              }
          }
      }

    def initRef[A](err: Error): Infer[Ref[Either[Error, A]]] =
      lift(RefSpace.newRef[Either[Error, A]](Left(err)))

    def substTy(keys: NonEmptyList[Type.Var], vals: NonEmptyList[Type], t: Type): Type = {

      def subst(env: Map[Type.Var, Type], t: Type): Type =
        t match {
          case Type.TyApply(on, arg) => Type.TyApply(subst(env, on), subst(env, arg))
          case v@Type.TyVar(n) => env.getOrElse(n, v)
          case Type.ForAll(ns, rho) =>
            val boundSet: Set[Type.Var] = ns.toList.toSet
            val env1 = env.filterKeys { v => !boundSet(v) }
            Type.ForAll(ns, subst(env1, rho))
          case m@Type.TyMeta(_) => m
          case c@Type.TyConst(_) => c
        }

      val env = keys.toList.iterator.zip(vals.toList.iterator).toMap
      subst(env, t)
    }

    // Return a Rho type (not a Forall)
    def instantiate(t: Type): Infer[Type.Rho] =
      t match {
        case Type.ForAll(vars, ty) =>
          for {
            vars1 <- vars.traverse(_ => newMetaTyVar)
            vars1T = vars1.map(Type.TyMeta(_))
          } yield substTy(vars, vars1T, ty)
        case rho => pure(rho)
      }

    def subsCheckFn(a1: Type, r1: Type.Rho, a2: Type, r2: Type.Rho): Infer[Unit] =
      // note due to contravariance in input, we reverse the order there
      subsCheck(a2, a1) *> subsCheckRho(r1, r2)

    // invariant: second argument is in weak prenex form
    def subsCheckRho(t: Type, rho: Type.Rho): Infer[Unit] =
      (t, rho) match {
        case (fa@Type.ForAll(_, _), rho) =>
          // Rule SPEC
          instantiate(fa).flatMap(subsCheckRho(_, rho))
        case (rho1, Type.Fun(a2, r2)) =>
          // Rule FUN
          unifyFn(rho1).flatMap {
            case (a1, r1) =>
              subsCheckFn(a1, r1, a2, r2)
          }
        case (Type.Fun(a1, r1), rho2) =>
          // Rule FUN
          unifyFn(rho2).flatMap {
            case (a2, r2) =>
              subsCheckFn(a1, r1, a2, r2)
          }
        case (t1, t2) =>
          // rule: MONO
          unify(t1, t2)
      }

    def instSigma(sigma: Type, expect: Expected[Type.Rho]): Infer[Unit] =
      expect match {
        case Expected.Check(t) =>
          subsCheckRho(sigma, t)
        case infer@Expected.Inf(_) =>
          instantiate(sigma).flatMap(infer.set(_))
      }

    def unifyFn(fnType: Type): Infer[(Type, Type)] =
      fnType match {
        case Type.Fun(arg, res) => pure((arg, res))
        case tau =>
          for {
            argT <- newTyVarTy
            resT <- newTyVarTy
            _ <- unify(tau, Type.Fun(argT, resT))
          } yield (argT, resT)
      }

    def occursCheckErr(m: Type.Meta, t: Type): Infer[Unit] =
      fail(Error.UnexpectedMeta(m, t))

    // invariant the flexible type variable tv1 is not bound
    def unifyUnboundVar(m: Type.Meta, ty2: Type): Infer[Unit] =
      ty2 match {
        case Type.TyMeta(m2) =>
          readMeta(m2).flatMap {
            case Some(ty2) => unify(Type.TyMeta(m), ty2)
            case None => writeMeta(m, ty2)
          }
        case nonMeta =>
          getMetaTyVars(List(nonMeta))
            .flatMap { tvs2 =>
              if (tvs2(m)) occursCheckErr(m, nonMeta)
              else writeMeta(m, nonMeta)
            }
      }

    def unifyVar(tv: Type.Meta, t: Type): Infer[Unit] =
      readMeta(tv).flatMap {
        case None => unifyUnboundVar(tv, t)
        case Some(ty1) => unify(ty1, t)
      }

    def unify(t1: Type.Tau, t2: Type.Tau): Infer[Unit] =
      (t1, t2) match {
        case (Type.TyVar(b@Type.Var.Bound(_)), _) =>
          fail(Error.UnexpectedBound(b, t2))
        case (_, Type.TyVar(b@Type.Var.Bound(_))) =>
          fail(Error.UnexpectedBound(b, t1))
        case (Type.TyVar(v1), Type.TyVar(v2)) if v1 == v2 => pure(())
        case (Type.TyMeta(m1), Type.TyMeta(m2)) if m1.id == m2.id => pure(())
        case (Type.TyMeta(m), tpe) => unifyVar(m, tpe)
        case (tpe, Type.TyMeta(m)) => unifyVar(m, tpe)
        case (Type.TyApply(a1, b1), Type.TyApply(a2, b2)) =>
          unify(a1, a2) *> unify(b1, b2)
        case (Type.TyConst(c1), Type.TyConst(c2)) if c1 == c2 => pure(())
        case (left, right) => fail(Error.NotUnifiable(left, right))
      }

    def newTyVarTy: Infer[Type.Tau] =
      newMetaTyVar.map(Type.TyMeta(_))

    def newMetaTyVar: Infer[Type.Meta] =
      for {
        id <- nextId
        ref <- lift(RefSpace.newRef[Option[Type]](None))
      } yield Type.Meta(id, ref)

    def newSkolemTyVar(tv: Type.Var): Infer[Type.Var] =
      nextId.map(Type.Var.Skolem(tv.name, _))

    def readMeta(m: Type.Meta): Infer[Option[Type.Tau]] =
      lift(m.ref.get)

    def writeMeta(m: Type.Meta, v: Type.Tau): Infer[Unit] =
      lift(m.ref.set(Some(v)))

    // DEEP-SKOL rule
    def subsCheck(inferred: Type, declared: Type): Infer[Unit] =
      for {
        skolRho <- skolemize(declared)
        (skolTvs, rho2) = skolRho
        _ <- subsCheckRho(inferred, rho2)
        escTvs <- getFreeTyVars(List(inferred, declared))
        badTvs = skolTvs.filter(escTvs)
        _ <- require(badTvs.isEmpty, Error.SubsumptionCheckFailure(inferred, declared))
      } yield ()

    /**
     * Invariant: if the second argument is (Check rho) then rho is in weak prenex form
     */
    def typeCheckRho(term: Term, expect: Expected[Type.Rho]): Infer[Unit] = {
      import Term._

      term match {
        case Lit(Term.Literal.Integer(_)) =>
          instSigma(Type.intType, expect)
        case Lit(Term.Literal.Bool(_)) =>
          instSigma(Type.boolType, expect)
        case Var(name) =>
          for {
            vSigma <- lookupVarType(name)
            _ <- instSigma(vSigma, expect)
           } yield ()
        case App(fn, arg) =>
           for {
             fnT <- inferRho(fn)
             argRes <- unifyFn(fnT)
             (argT, resT) = argRes
             _ <- checkSigma(arg, argT)
             _ <- instSigma(resT, expect)
           } yield ()
        case Lam(name, result) =>
          expect match {
            case Expected.Check(expTy) =>
              unifyFn(expTy).flatMap {
                case (varT, bodyT) =>
                  extendEnv(name, varT) {
                    checkRho(result, bodyT)
                  }
                }
            case infer@Expected.Inf(_) =>
              for {
                varT <- newTyVarTy
                bodyT <- extendEnv(name, varT)(inferRho(result))
                _ <- infer.set(Type.Fun(varT, bodyT))
              } yield ()
          }
        case ALam(name, tpe, result) =>
          expect match {
            case Expected.Check(expTy) =>
              unifyFn(expTy).flatMap {
                case (varT, bodyT) =>
                  extendEnv(name, varT) {
                    subsCheck(tpe, varT) *> checkRho(result, bodyT)
                  }
                }
            case infer@Expected.Inf(_) =>
              for {
                bodyT <- extendEnv(name, tpe)(inferRho(result))
                _ <- infer.set(Type.Fun(tpe, bodyT))
              } yield ()
          }
        case Let(name, rhs, body) =>
          for {
            varT <- inferSigma(rhs)
            _ <- extendEnv(name, varT)(typeCheckRho(body, expect))
          } yield ()
        case Ann(term, tpe) =>
          checkSigma(term, tpe) *> instSigma(tpe, expect)
        case If(cond, ifTrue, ifFalse) =>
          val condTpe =
            typeCheckRho(cond,
              Expected.Check(Type.boolType))
          val rest = expect match {
            case check@Expected.Check(_) =>
              typeCheckRho(ifTrue, check) *>
                typeCheckRho(ifFalse, check)

            case infer@Expected.Inf(_) =>
              for {
                rT <- inferRho(ifTrue)
                rF <- inferRho(ifFalse)
                _ <- subsCheck(rT, rF)
                _ <- subsCheck(rF, rT)
                _ <- infer.set(rT) // see section 7.1
              } yield ()

          }
          condTpe *> rest
        case Match(term, branches) =>
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
            case Expected.Check(resT) =>
              for {
                tsigma <- inferSigma(term)
                _ <- branches.traverse_ { case (p, r) => checkBranch(p, Expected.Check(tsigma), r, resT) }
              } yield ()
            case infer@Expected.Inf(_) =>
              for {
                tsigma <- inferSigma(term)
                resTs <- branches.traverse { case (p, r) => inferBranch(p, Expected.Check(tsigma), r) }
                _ <- resTs.flatMap { t0 => resTs.map((t0, _)) }.traverse_ {
                  case (t0, t1) if t0 eq t1 => Infer.pure(())
                  case (t0, t1) => subsCheck(t0, t1)
                }
                _ <- infer.set(resTs.head)
              } yield ()
          }
      }
    }

    def checkBranch(p: Pattern, sigma: Expected[Type], res: Term, resT: Type): Infer[Unit] =
      for {
        bindings <- typeCheckPattern(p, sigma)
        _ <- extendEnvList(bindings)(checkRho(res, resT))
      } yield ()

    def inferBranch(p: Pattern, sigma: Expected[Type], res: Term): Infer[Type] =
      for {
        bindings <- typeCheckPattern(p, sigma)
        res <- extendEnvList(bindings)(inferRho(res))
      } yield res

    /**
     * patterns can be a sigma type, not neccesarily a rho/tau
     * return a list of bound names and their (sigma) types
     */
    def typeCheckPattern(pat: Pattern, sigma: Expected[Type]): Infer[List[(String, Type)]] =
      pat match {
        case GenPattern.WildCard => Infer.pure(Nil)
        case GenPattern.Var(n) =>
          sigma match {
            case Expected.Check(t) => Infer.pure(List((n, t)))
            case infer@Expected.Inf(_) =>
              for {
                t <- newTyVarTy
                _ <- infer.set(t)
              } yield List((n, t))
          }
        case GenPattern.PositionalStruct(nm, args) =>
          for {
            paramRes <- instDataCon(nm)
            (params, res) = paramRes
            // we need to do a pattern linting phase and probably error
            // if the pattern arity does not match the arity of the constructor
            // but we don't want to error type-checking since we want to show
            // the maximimum number of errors to the user
            envs <- args.zip(params).traverse { case (p, t) => checkPat(p, t) }
            _ <- instPatSigma(res, sigma)
          } yield envs.flatten
      }


    def checkPat(pat: Pattern, sigma: Type): Infer[List[(String, Type)]] =
      typeCheckPattern(pat, Expected.Check(sigma))

    def inferPat(pat: Pattern): Infer[Type] =
      for {
        ref <- initRef[Type](Error.InferPatIncomplete(pat))
        _ <- typeCheckPattern(pat, Expected.Inf(ref))
        sigma <- (Lift(ref.get): Infer[Type])
        _ <- lift(ref.reset) // we don't need this ref, and it does not escape, so reset
      } yield sigma

    def instPatSigma(sigma: Type, exp: Expected[Type]): Infer[Unit] =
      exp match {
        case infer@Expected.Inf(_) => infer.set(sigma)
        case Expected.Check(texp) => subsCheck(texp, sigma)
      }

    /**
     * To do this, Infer will need to know the names of the type
     * constructors in scope
     */
    def instDataCon(consName: String): Infer[(List[Type], Type.Tau)] =
      GetDataCons(consName).flatMap {
        case (Nil, consParams, tpeName) =>
          Infer.pure((consParams, Type.TyConst(tpeName)))
        case (v0 :: vs, consParams, tpeName) =>
          val vars = NonEmptyList(v0, vs)
          vars.traverse(_ => newMetaTyVar)
            .map { vars1 =>
              val vars1T = vars1.map(Type.TyMeta(_))
              val params1 = consParams.map(substTy(vars, vars1T, _))
              val res = vars1T.foldLeft(Type.TyConst(tpeName): Type)(Type.TyApply(_, _))
              (params1, res)
            }
      }
  }


  def typeCheck(t: Term): Infer[Type] =
    inferSigma(t).flatMap(zonkType _)

  def inferSigma(e: Term): Infer[Type] =
    for {
      expTy <- inferRho(e)
      envTys <- getEnv
      envTypeVars <- getMetaTyVars(envTys.values.toList)
      resTypeVars <- getMetaTyVars(List(expTy))
      forAllTvs = resTypeVars -- envTypeVars
      q <- quantify(forAllTvs.toList, expTy)
    } yield q

  def checkSigma(t: Term, tpe: Type): Infer[Unit] =
    for {
      skolRho <- skolemize(tpe)
      (skols, rho) = skolRho
      _ <- checkRho(t, rho)
      envTys <- getEnv
      escTvs <- getFreeTyVars(tpe :: envTys.values.toList)
      badTvs = skols.filter(escTvs)
      _ <- require(badTvs.isEmpty, Error.NotPolymorphicEnough(tpe, t))
    } yield ()

  def checkRho(t: Term, rho: Type.Rho): Infer[Unit] =
    typeCheckRho(t, Expected.Check(rho))

  def inferRho(t: Term): Infer[Type.Rho] =
    for {
      ref <- initRef[Type.Rho](Error.InferIncomplete("inferRho", t))
      _ <- typeCheckRho(t, Expected.Inf(ref))
      rho <- (Lift(ref.get): Infer[Type.Rho])
      _ <- lift(ref.reset) // we don't need this ref, and it does not escape, so reset
    } yield rho


  def extendEnv[A](varName: String, tpe: Type)(of: Infer[A]): Infer[A] =
    extendEnvList(List((varName, tpe)))(of)

  def extendEnvList[A](bindings: List[(String, Type)])(of: Infer[A]): Infer[A] =
    Infer.Impl.ExtendEnvs(bindings, of)

}
