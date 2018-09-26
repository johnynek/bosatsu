package org.bykn.bosatsu.rankn
import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._

sealed abstract class Tc[+A] {
  def run(env: Tc.Env): RefSpace[Either[String, A]]

  final def flatMap[B](fn: A => Tc[B]): Tc[B] =
    Tc.FlatMap(this, fn)

  final def runVar(v: Map[String, Type]): RefSpace[Either[String, A]] =
    Tc.Env.init(v).flatMap(run(_))

  final def runFully(v: Map[String, Type]): Either[String, A] =
    runVar(v).run.value
}

object Tc {
  implicit val tcMonad: Monad[Tc] =
    new Monad[Tc] {
      def pure[A](a: A) = Tc.pure(a)
      def flatMap[A, B](fa: Tc[A])(fn: A => Tc[B]): Tc[B] = fa.flatMap(fn)
      def tailRecM[A, B](a: A)(fn: A => Tc[Either[A, B]]): Tc[B] =
        TailRecM(a, fn)
    }


  case class Env(uniq: Ref[Long], vars: Map[String, Type])

  object Env {
    def empty: RefSpace[Env] =
      init(Map.empty)

    def init(vars: Map[String, Type]): RefSpace[Env] =
      RefSpace.newRef(0L).map(Env(_, vars))
  }

  sealed abstract class Expected[A]
  object Expected {
    case class Infer[A](ref: Ref[Either[String, A]]) extends Expected[A] {
      def set(a: A): Tc[Unit] =
        Tc.lift(ref.set(Right(a)))
    }
    case class Check[A](value: A) extends Expected[A]
  }

  private case class Defer[A](tc: () => Tc[A]) extends Tc[A] {
    lazy val finalTc = {
      def loop(tc: Tc[A]): Tc[A] =
        tc match {
          case Defer(next) => loop(next())
          case nonDefer => nonDefer
        }

      loop(tc())
    }

    def run(env: Env) = finalTc.run(env)
  }

  private case class FlatMap[A, B](fa: Tc[A], fn: A => Tc[B]) extends Tc[B] {
    def run(env: Env) =
      fa.run(env).flatMap {
        case Left(msg) => RefSpace.pure(Left(msg))
        case Right(a) => fn(a).run(env)
      }
  }
  private case class TailRecM[A, B](init: A, fn: A => Tc[Either[A, B]]) extends Tc[B] {
    def run(env: Env) = {
      def step(a: A): RefSpace[Either[A, Either[String, B]]] =
        fn(a).run(env).map {
          case Left(err) => Right(Left(err))
          case Right(Left(a)) => Left(a)
          case Right(Right(a)) => Right(Right(a))
        }

      Monad[RefSpace].tailRecM(init)(step _)
    }
  }

  private case object GetEnv extends Tc[Map[String, Type]] {
    def run(env: Env) = RefSpace.pure(Right(env.vars))
  }

  private case class ExtendEnv[A](varName: String, tpe: Type, in: Tc[A]) extends Tc[A] {
    def run(env: Env) = in.run(env.copy(vars = env.vars.updated(varName, tpe)))
  }

  private case class Lift[A](res: RefSpace[Either[String, A]]) extends Tc[A] {
    def run(env: Env) = res
  }

  private case object NextId extends Tc[Long] {
    def run(env: Env) =
      for {
        thisId <- env.uniq.get
        _ <- env.uniq.set(thisId + 1L)
      } yield Right(thisId)
  }

  def getEnv: Tc[Map[String, Type]] = GetEnv

  def lift[A](rs: RefSpace[A]): Tc[A] =
    Lift(rs.map(Right(_)))

  def fail(msg: String): Tc[Nothing] =
    Lift(RefSpace.pure(Left(msg)))

  def pure[A](a: A): Tc[A] =
    Lift(RefSpace.pure(Right(a)))

  def defer[A](tc: => Tc[A]): Tc[A] =
    Defer(() => tc)

  def require(b: Boolean, msg: String): Tc[Unit] =
    if (b) pure(()) else fail(msg)

  // Fails if v is not in the env
  def lookupVarType(v: String): Tc[Type] =
    getEnv.flatMap { env =>
      env.get(v) match {
        case None => fail(s"$v not in scope: $env")
        case Some(t) => pure(t)
      }
    }

  def nextId: Tc[Long] = NextId

  def typeCheck(t: Term): Tc[Type] =
    inferSigma(t).flatMap(zonkType _)

  def getMetaTyVars(tpes: List[Type]): Tc[Set[Type.Meta]] =
    tpes.traverse(zonkType).map(Type.metaTvs(_))

  private def tyVarBinders(tpes: List[Type], acc: Set[Type.Var]): Set[Type.Var] =
    tpes match {
      case Nil => acc
      case Type.ForAll(tvs, body) :: rest =>
        tyVarBinders(rest, acc ++ tvs.toList)
      case Type.Fun(arg, res) :: rest =>
        tyVarBinders(arg :: res :: rest, acc)
      case _ :: rest => tyVarBinders(rest, acc)
    }

  /**
   * Quantify over the specified type variables (all flexible)
   */
  def quantify(forAlls: List[Type.Meta], rho: Type.Rho): Tc[Type] =
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

  def inferSigma(e: Term): Tc[Type] =
    for {
      expTy <- inferRho(e)
      envTys <- getEnv
      envTypeVars <- getMetaTyVars(envTys.values.toList)
      resTypeVars <- getMetaTyVars(List(expTy))
      forAllTvs = resTypeVars -- envTypeVars
      q <- quantify(forAllTvs.toList, expTy)
    } yield q

  def skolemize(t: Type): Tc[(List[Type.Var], Type.Rho)] =
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

  private def freeTyVars(ts: List[Type]): Set[Type.Var] = {
    @annotation.tailrec
    def go(ts: List[Type], bound: Set[Type.Var], acc: Set[Type.Var]): Set[Type.Var] =
      ts match {
        case Nil => acc
        case Type.TyVar(tv) :: rest =>
          if (bound(tv)) go(rest, bound, acc)
          else go(rest, bound, acc + tv)
        case Type.Fun(a, b) :: rest => go(a :: b :: rest, bound, acc)
        case Type.ForAll(tvs, ty) :: rest => go(ty :: rest, bound ++ tvs.toList, acc)
        case (Type.TyMeta(_) | Type.TyConst(_)) :: rest => go(rest, bound, acc)
      }

    ts.foldLeft(Set.empty[Type.Var]) { (acc, t) =>
      go(t :: Nil, Set.empty, acc)
    }
  }

  private def getFreeTyVars(ts: List[Type]): Tc[Set[Type.Var]] =
    ts.traverse(zonkType).map(freeTyVars(_))

  def checkSigma(t: Term, tpe: Type): Tc[Unit] =
    for {
      skolRho <- skolemize(tpe)
      (skols, rho) = skolRho
      _ <- checkRho(t, rho)
      envTys <- getEnv
      escTvs <- getFreeTyVars(tpe :: envTys.values.toList)
      badTvs = skols.filter(escTvs)
      _ <- require(badTvs.isEmpty, s"type $tpe not polymorphic enough in $t")
    } yield ()

  def zonkType(t: Type): Tc[Type] =
    t match {
      case Type.ForAll(ns, ty) =>
        zonkType(ty).map(Type.ForAll(ns, _))
      case Type.Fun(arg, res) =>
        (zonkType(arg), zonkType(res)).mapN(Type.Fun(_, _))
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

  def checkRho(t: Term, rho: Type.Rho): Tc[Unit] =
    typeCheckRho(t, Expected.Check(rho))

  def inferRho(t: Term): Tc[Type.Rho] =
    for {
      ref <- lift(RefSpace.newRef[Either[String, Type.Rho]](Left(s"inferRho not complete for $t")))
      _ <- typeCheckRho(t, Expected.Infer(ref))
      rho <- (Lift(ref.get): Tc[Type.Rho])
      _ <- lift(ref.reset) // we don't need this ref, and it does not escape, so reset
    } yield rho

  private def substTy(keys: NonEmptyList[Type.Var], vals: NonEmptyList[Type], t: Type): Type = {

    def subst(env: Map[Type.Var, Type], t: Type): Type =
      t match {
        case Type.Fun(arg, res) => Type.Fun(subst(env, arg), subst(env, res))
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
  def instantiate(t: Type): Tc[Type.Rho] =
    t match {
      case Type.ForAll(vars, ty) =>
        for {
          vars1 <- vars.traverse(_ => newMetaTyVar)
          vars1T = vars1.map(Type.TyMeta(_))
        } yield substTy(vars, vars1T, ty)
      case rho => pure(rho)
    }

  private def subsCheckFn(a1: Type, r1: Type.Rho, a2: Type, r2: Type.Rho): Tc[Unit] =
    // note due to contravariance in input, we reverse the order there
    subsCheck(a2, a1) *> subsCheckRho(r1, r2)

  // invariant: second argument is in weak prenex form
  private def subsCheckRho(t: Type, rho: Type.Rho): Tc[Unit] =
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

  def instSigma(sigma: Type, expect: Expected[Type.Rho]): Tc[Unit] =
    expect match {
      case Expected.Check(t) =>
        subsCheckRho(sigma, t)
      case infer@Expected.Infer(_) =>
        instantiate(sigma).flatMap(infer.set(_))
    }

  private def unifyFn(fnType: Type): Tc[(Type, Type)] =
    fnType match {
      case Type.Fun(arg, res) => pure((arg, res))
      case tau =>
        for {
          argT <- newTyVarTy
          resT <- newTyVarTy
          _ <- unify(tau, Type.Fun(argT, resT))
        } yield (argT, resT)
    }

  private def occursCheckErr(m: Type.Meta, t: Type): Tc[Unit] =
    fail(s"meta $m occurs in $t and should not")

  // invariant the flexible type variable tv1 is not bound
  private def unifyUnboundVar(m: Type.Meta, ty2: Type): Tc[Unit] =
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

  private def unifyVar(tv: Type.Meta, t: Type): Tc[Unit] =
    readMeta(tv).flatMap {
      case None => unifyUnboundVar(tv, t)
      case Some(ty1) => unify(ty1, t)
    }

  private def unify(t1: Type.Tau, t2: Type.Tau): Tc[Unit] =
    (t1, t2) match {
      case (Type.TyVar(Type.Var.Bound(v)), _) =>
        fail(s"unexpected bound $v in unification with $t2")
      case (_, Type.TyVar(Type.Var.Bound(v))) =>
        fail(s"unexpected bound $v in unification with $t1")
      case (Type.TyVar(v1), Type.TyVar(v2)) if v1 == v2 => pure(())
      case (Type.TyMeta(m1), Type.TyMeta(m2)) if m1.id == m2.id => pure(())
      case (Type.TyMeta(m), tpe) => unifyVar(m, tpe)
      case (tpe, Type.TyMeta(m)) => unifyVar(m, tpe)
      case (Type.Fun(a1, r1), Type.Fun(a2, r2)) =>
        unify(a1, a2) *> unify(r1, r2)
      case (Type.TyConst(c1), Type.TyConst(c2)) if c1 == c2 => pure(())
      case (left, right) => fail(s"$left cannot be unified with $right")
    }

  def extendEnv[A](varName: String, tpe: Type)(of: Tc[A]): Tc[A] =
    Tc.ExtendEnv(varName, tpe, of)

  private def newTyVarTy: Tc[Type.Tau] =
    newMetaTyVar.map(Type.TyMeta(_))

  private def newMetaTyVar: Tc[Type.Meta] =
    for {
      id <- nextId
      ref <- lift(RefSpace.newRef[Option[Type]](None))
    } yield Type.Meta(id, ref)

  private def newSkolemTyVar(tv: Type.Var): Tc[Type.Var] =
    nextId.map(Type.Var.Skolem(tv.name, _))

  private def readMeta(m: Type.Meta): Tc[Option[Type.Tau]] =
    lift(m.ref.get)

  private def writeMeta(m: Type.Meta, v: Type.Tau): Tc[Unit] =
    lift(m.ref.set(Some(v)))

  // DEEP-SKOL rule
  private def subsCheck(inferred: Type, declared: Type): Tc[Unit] =
    for {
      skolRho <- skolemize(declared)
      (skolTvs, rho2) = skolRho
      _ <- subsCheckRho(inferred, rho2)
      escTvs <- getFreeTyVars(List(inferred, declared))
      badTvs = skolTvs.filter(escTvs)
      _ <- require(badTvs.isEmpty, s"subsumption check failed: $inferred $declared")
    } yield ()

  /**
   * Invariant: if the second argument is (Check rho) then rho is in weak prenex form
   */
  private def typeCheckRho(term: Term, expect: Expected[Type.Rho]): Tc[Unit] = {
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
          case infer@Expected.Infer(_) =>
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
          case infer@Expected.Infer(_) =>
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

          case infer@Expected.Infer(_) =>
            for {
              rT <- inferRho(ifTrue)
              rF <- inferRho(ifFalse)
              _ <- subsCheck(rT, rF)
              _ <- subsCheck(rF, rT)
              _ <- infer.set(rT) // see section 7.1
            } yield ()

        }
        condTpe *> rest
    }
  }


}
