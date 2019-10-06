package org.bykn.bosatsu

import cats.{Applicative, Eval, Traverse, Monad, Monoid}
import cats.arrow.FunctionK
import cats.data.{NonEmptyList, Writer}
import cats.implicits._
import org.bykn.bosatsu.rankn.Type
import org.typelevel.paiges.{Doc, Document }
import scala.collection.immutable.SortedSet
import scala.util.hashing.MurmurHash3

import Identifier.{Bindable, Constructor}

sealed abstract class TypedExpr[+T] { self: Product =>
  import TypedExpr._

  // It is really important to cache the hashcode and these large dags if
  // we use them as hash keys
  final override val hashCode: Int =
    MurmurHash3.productHash(this)

  def tag: T
  /**
   * For any well typed expression, i.e.
   * one that has already gone through type
   * inference, we should be able to get a type
   * for each expression
   *
   */
  lazy val getType: Type =
    this match {
      case Generic(params, expr, _) =>
        val tpe = expr.getType
        // if tpe has no Var.Bound in common,
        // then we don't need the forall
        val frees = Type.freeBoundTyVars(tpe :: Nil).toSet
        Type.forAll(params.toList.filter(frees), tpe)
      case Annotation(_, tpe, _) =>
        tpe
      case a@AnnotatedLambda(arg, tpe, res, _) =>
        Type.Fun(tpe, res.getType)
      case Var(_, _, tpe, _) => tpe
      case App(_, _, tpe, _) => tpe
      case Let(_, _, in, _, _) =>
        in.getType
      case Literal(_, tpe, _) =>
        tpe
      case Match(_, branches, _) =>
        // all branches have the same type:
        branches.head._2.getType
    }

  // TODO: we need to make sure this parsable and maybe have a mode that has the compiler
  // emit these
  def repr: String = {
    val tfn = TypeRef.fromTypes(None, this.allTypes.toList)

    // We need a consistent naming for meta variables,
    // so build this table once
    def rept(t: Type): String =
      tfn(t).toDoc.renderWideStream.mkString

    def loop(te: TypedExpr[T]): String = {
      te match {
        case Generic(params, expr, _) =>
          val pstr = params.toList.map(_.name).mkString(",")
          s"(generic [$pstr] ${loop(expr)})"
        case Annotation(expr, tpe, _) =>
          s"(ann ${rept(tpe)} ${loop(expr)})"
        case a@AnnotatedLambda(arg, tpe, res, _) =>
          s"(lambda ${arg.asString} ${rept(tpe)} ${loop(res)})"
        case Var(p, v, tpe, _) =>
          val pstr = p match {
            case None => ""
            case Some(p) => p.asString + "::"
          }
          s"(var $pstr${v.asString} ${rept(tpe)})"
        case App(fn, arg, tpe, _) =>
          s"(ap ${loop(fn)} ${loop(arg)} ${rept(tpe)})"
        case Let(n, b, in, rec, _) =>
          val nm = if (rec.isRecursive) "letrec" else "let"
          s"($nm $n ${loop(b)} ${loop(in)})"
        case Literal(v, tpe, _) =>
          s"(lit ${v.repr} ${rept(tpe)})"
        case Match(arg, branches, _) =>
          implicit val docType: Document[Type] =
            Document.instance { tpe => Doc.text(rept(tpe)) }
          val cpat = Pattern.compiledDocument[Type]
          def pat(p: Pattern[(PackageName, Constructor), Type]): String =
            cpat.document(p).renderWideStream.mkString

          val bstr = branches.toList.map { case (p, t) => s"[${pat(p)}, ${loop(t)}]" }.mkString("[", ", ", "]")
          s"(match ${loop(arg)} $bstr)"
      }
    }

    loop(this)
  }
}

object TypedExpr {
  type Rho[A] = TypedExpr[A] // an expression with a Rho type (no top level forall)
  /**
   * This says that the resulting term is generic on a given param
   *
   * The paper says to add TyLam and TyApp nodes, but it never mentions what to do with them
   */
  // TODO: this shouldn't have a tag, the tag should be the same as in
  case class Generic[T](typeVars: NonEmptyList[Type.Var.Bound], in: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Annotation[T](term: TypedExpr[T], coerce: Type, tag: T) extends TypedExpr[T]
  case class AnnotatedLambda[T](arg: Bindable, tpe: Type, expr: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Var[T](pack: Option[PackageName], name: Identifier, tpe: Type, tag: T) extends TypedExpr[T]
  case class App[T](fn: TypedExpr[T], arg: TypedExpr[T], result: Type, tag: T) extends TypedExpr[T]
  case class Let[T](arg: Bindable, expr: TypedExpr[T], in: TypedExpr[T], recursive: RecursionKind, tag: T) extends TypedExpr[T]
  // TODO, this shouldn't have a type, we know the type from Lit currently
  case class Literal[T](lit: Lit, tpe: Type, tag: T) extends TypedExpr[T]
  case class Match[T](arg: TypedExpr[T], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])], tag: T) extends TypedExpr[T]

  private implicit val setM: Monoid[SortedSet[Type]] =
    new Monoid[SortedSet[Type]] {
      def empty = SortedSet.empty
      def combine(a: SortedSet[Type], b: SortedSet[Type]) = a ++ b
    }

  implicit class InvariantTypedExpr[A](val self: TypedExpr[A]) extends AnyVal {
    def updatedTag(t: A): TypedExpr[A] =
      self match {
        case g@Generic(_, _, _) => g.copy(tag=t)
        case a@Annotation(_, _, _) => a.copy(tag=t)
        case al@AnnotatedLambda(_, _, _, _) => al.copy(tag=t)
        case v@Var(_, _, _, _) => v.copy(tag=t)
        case a@App(_, _, _, _) => a.copy(tag=t)
        case let@Let(_, _, _, _, _) => let.copy(tag=t)
        case lit@Literal(_, _, _) => lit.copy(tag=t)
        case m@Match(_, _, _) => m.copy(tag=t)
      }

    def allTypes: SortedSet[Type] =
      traverseType { t => Writer(SortedSet(t), t) }.run._1

    def traverseType[F[_]: Applicative](fn: Type => F[Type]): F[TypedExpr[A]] =
      self match {
        case Generic(params, expr, tag) =>
          // The parameters are are like strings, but this
          // is a bit unsafe... we only use it for zonk which
          // ignores Bounds
          expr.traverseType(fn).map(Generic(params, _, tag))
        case Annotation(of, tpe, tag) =>
          (of.traverseType(fn), fn(tpe)).mapN(Annotation(_, _, tag))
        case AnnotatedLambda(arg, tpe, res, tag) =>
          (fn(tpe), res.traverseType(fn)).mapN {
            AnnotatedLambda(arg, _, _, tag)
          }
        case Var(p, v, tpe, tag) =>
          fn(tpe).map(Var(p, v, _, tag))
        case App(f, arg, tpe, tag) =>
          (f.traverseType(fn), arg.traverseType(fn), fn(tpe)).mapN {
            App(_, _, _, tag)
          }
        case Let(v, exp, in, rec, tag) =>
          (exp.traverseType(fn), in.traverseType(fn)).mapN {
            Let(v, _, _, rec, tag)
          }
        case Literal(lit, tpe, tag) =>
          fn(tpe).map(Literal(lit, _, tag))
        case Match(expr, branches, tag) =>
          // all branches have the same type:
          val tbranch = branches.traverse {
            case (p, t) =>
              p.traverseType(fn).product(t.traverseType(fn))
          }
          (expr.traverseType(fn), tbranch).mapN(Match(_, _, tag))
      }
  }

  def zonkMeta[F[_]: Applicative, A](te: TypedExpr[A])(fn: Type.Meta => F[Option[Type.Rho]]): F[TypedExpr[A]] =
    te.traverseType(Type.zonkMeta(_)(fn))

  /**
   * quantify every meta variable that is not escaped into
   * the outer environment.
   *
   * TODO: This can probably be optimized. I think it is currently
   * quadradic in depth of the TypedExpr
   */
  def quantify[F[_]: Monad, A](
    env: Map[(Option[PackageName], Identifier), Type],
    rho: TypedExpr.Rho[A],
    zFn: Type.Meta => F[Option[Type.Rho]],
    writeFn: (Type.Meta, Type.Var) => F[Unit]): F[TypedExpr[A]] = {

    // we need to zonk before we get going because
    // some of the meta-variables may point to the same values
    def getMetaTyVars(tpes: List[Type]): F[SortedSet[Type.Meta]] =
      tpes.traverse(Type.zonkMeta(_)(zFn)).map(Type.metaTvs(_))

    def quantify0(forAlls: List[Type.Meta], rho: TypedExpr.Rho[A]): F[TypedExpr[A]] =
      NonEmptyList.fromList(forAlls) match {
        case None => Applicative[F].pure(rho)
        case Some(metas) =>
          val used: Set[Type.Var.Bound] = Type.tyVarBinders(rho.getType :: Nil)
          val aligned = Type.alignBinders(metas, used)
          val bound = aligned.traverse_ { case (m, n) => writeFn(m, n) }
          // we only need to zonk after doing a write:
          (bound *> zonkMeta(rho)(zFn)).map(forAll(aligned.map(_._2), _))
      }

    type Name = (Option[PackageName], Identifier)

    def quantifyMetas(env: Map[Name, Type], metas: SortedSet[Type.Meta], te: TypedExpr[A]): F[TypedExpr[A]] =
      if (metas.isEmpty) Applicative[F].pure(te)
      else {
        for {
          envTypeVars <- getMetaTyVars(env.values.toList)
          forAllTvs = metas -- envTypeVars
          q <- quantify0(forAllTvs.toList, te)
        } yield q
      }

    def quantifyFree(env: Map[Name, Type], te: TypedExpr[A]): F[TypedExpr[A]] =
      getMetaTyVars(te.getType :: Nil)
        .flatMap(quantifyMetas(env, _, te))

    /*
     * By only quantifying the outside
     * the inside may still have some metas that don't
     * make it all the way out.
     *
     * This algorithm isn't great. It is quadratic in depth
     * because we have to do work linear in depth at each
     * level.
     */
    def deepQuantify(env: Map[Name, Type], te: TypedExpr[A]): F[TypedExpr[A]] =
      quantifyFree(env, te).flatMap {
        case Generic(typeVars, in, tag) =>
          deepQuantify(env, in).map { in1 =>
            forAll(typeVars, in1).updatedTag(tag)
          }
        case Annotation(term, coerce, tag) =>
          deepQuantify(env, term).map { t1 =>
            Annotation(t1, coerce, tag)
          }
        case AnnotatedLambda(arg, tpe, expr, tag) =>
          deepQuantify(env.updated(((None, arg)), tpe), expr)
            .map { e1 =>
              lambda(arg, tpe, e1, tag)
            }
        case Let(arg, expr, in, rec, tag) =>
          // this introduces something into the env
          val inEnv = env.updated((None, arg), expr.getType)
          val exprEnv = if (rec.isRecursive) inEnv else env
          (deepQuantify(exprEnv, expr), deepQuantify(inEnv, in))
            .mapN { (e1, i1) =>
              Let(arg, e1, i1, rec, tag)
            }
        case App(fn, arg, tpe, tag) =>
          (deepQuantify(env, fn), deepQuantify(env, arg))
            .mapN { (f1, a1) =>
              App(f1, a1, tpe, tag)
            }
        case Match(arg, branches, tag) =>
          /*
           * We consider the free metas of
           * arg and inside the branches
           * together. for instance,
           * matching (x: forall a. Option[a])
           *
           * match x:
           *   None: 0
           *   Some(y): 1
           *
           * would give:
           * (generic [a]
           *   (match (var x Option[a])
           *     [[None, (lit 0 Int)],
           *     [[Some(x: a), (lit 1 Int)]]))
           *
           * which has a type forall a. Int which is the same
           * as Int
           */
          type Branch = (Pattern[(PackageName, Constructor), Type], TypedExpr[A])

          def allTypes[X](p: Pattern[X, Type]): SortedSet[Type] =
            p.traverseType { t => Writer(SortedSet(t), t) }.run._1

          val allMatchMetas: F[SortedSet[Type.Meta]] =
            getMetaTyVars(arg.getType :: branches.foldMap { case (p, _) => allTypes(p) }.toList)

          def handleBranch(br: Branch): F[Branch] = {
            val (p, expr) = br
            val branchEnv = Pattern.envOf(p, env) { ident => (None, ident) }
            deepQuantify(branchEnv, expr).map((p, _))
          }

          val noArg = for {
            br1 <- branches.traverse(handleBranch(_))
            ms <- allMatchMetas
            quant <- quantifyMetas(env, ms, Match(arg, br1, tag))
          } yield quant

          def finish(te: TypedExpr[A]): F[TypedExpr[A]] =
            te match {
              case Match(arg, branches, tag) =>
                // we still need to recurse on arg
                deepQuantify(env, arg).map(Match(_, branches, tag))
              case Generic(ps, expr, tag) =>
                finish(expr).map(forAll(ps, _).updatedTag(tag))
              case unreach =>
                // $COVERAGE-OFF$
                sys.error(s"Match quantification yielded neither Generic nor Match")
                // $COVERAGE-ON$
            }

          noArg.flatMap(finish)

        case nonest@(Var(_, _, _, _) | Literal(_, _, _)) =>
          Applicative[F].pure(nonest)
      }

    deepQuantify(env, rho)
  }

  implicit val traverseTypedExpr: Traverse[TypedExpr] = new Traverse[TypedExpr] {
    def traverse[F[_]: Applicative, T, S](typedExprT: TypedExpr[T])(fn: T => F[S]): F[TypedExpr[S]] =
      typedExprT match {
        case Generic(params, expr, tag) =>
          (expr.traverse(fn), fn(tag)).mapN(Generic(params, _, _))
        case Annotation(of, tpe, tag) =>
          (of.traverse(fn), fn(tag)).mapN(Annotation(_, tpe, _))
        case AnnotatedLambda(arg, tpe, res, tag) =>
          (res.traverse(fn), fn(tag)).mapN {
            AnnotatedLambda(arg, tpe, _, _)
          }
        case Var(p, v, tpe, tag) =>
          fn(tag).map(Var(p, v, tpe, _))
        case App(f, arg, tpe, tag) =>
          (f.traverse(fn), arg.traverse(fn), fn(tag)).mapN {
            App(_, _, tpe, _)
          }
        case Let(v, exp, in, rec, tag) =>
          (exp.traverse(fn), in.traverse(fn), fn(tag)).mapN {
            Let(v, _, _, rec, _)
          }
        case Literal(lit, tpe, tag) =>
          fn(tag).map(Literal(lit, tpe, _))
        case Match(expr, branches, tag) =>
          // all branches have the same type:
          val tbranch = branches.traverse {
            case (p, t) =>
              t.traverse(fn).map((p, _))
          }
          (expr.traverse(fn), tbranch, fn(tag)).mapN(Match(_, _, _))
      }

    def foldLeft[A, B](typedExprA: TypedExpr[A], b: B)(f: (B, A) => B): B = typedExprA match {
      case Generic(_, e, tag) =>
        val b1 = foldLeft(e, b)(f)
        f(b1, tag)
      case Annotation(e, _, tag) =>
        val b1 = foldLeft(e, b)(f)
        f(b1, tag)
      case AnnotatedLambda(_, _, e, tag) =>
        val b1 = foldLeft(e, b)(f)
        f(b1, tag)
      case Var(_, _, _, tag) => f(b, tag)
      case App(fn, a, _, tag) =>
        val b1 = foldLeft(fn, b)(f)
        val b2 = foldLeft(a, b1)(f)
        f(b2, tag)
      case Let(_, exp, in, _, tag) =>
        val b1 = foldLeft(exp, b)(f)
        val b2 = foldLeft(in, b1)(f)
        f(b2, tag)
      case Literal(_, _, tag) =>
        f(b, tag)
      case Match(arg, branches, tag) =>
        val b1 = foldLeft(arg, b)(f)
        val b2 = branches.foldLeft(b1) { case (bn, (p,t)) => foldLeft(t, bn)(f) }
        f(b2, tag)
    }

    def foldRight[A, B](typedExprA: TypedExpr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = typedExprA match {
      case Generic(_, e, tag) =>
        val b1 = foldRight(e, lb)(f)
        f(tag, b1)
      case Annotation(e, _, tag) =>
        val lb1 = foldRight(e, lb)(f)
        f(tag, lb1)
      case AnnotatedLambda(_, _, e, tag) =>
        val lb1 = foldRight(e, lb)(f)
        f(tag, lb1)
      case Var(_, _ , _, tag) => f(tag, lb)
      case App(fn, a, _, tag) =>
        val b1 = f(tag, lb)
        val b2 = foldRight(a, b1)(f)
        foldRight(fn, b2)(f)
      case Let(_, exp, in, _, tag) =>
        val b1 = f(tag, lb)
        val b2 = foldRight(in, b1)(f)
        foldRight(exp, b2)(f)
      case Literal(_, _, tag) =>
        f(tag, lb)
      case Match(arg, branches, tag) =>
        val b1 = f(tag, lb)
        val b2 = branches.foldRight(b1) { case ((p,t), bn) => foldRight(t, bn)(f) }
        foldRight(arg, b2)(f)
    }

    override def map[A, B](te: TypedExpr[A])(fn: A => B): TypedExpr[B] = te match {
      case Generic(tv, in, tag) => Generic(tv, map(in)(fn), fn(tag))
      case Annotation(term, tpe, tag) => Annotation(map(term)(fn), tpe, fn(tag))
      case AnnotatedLambda(b, tpe, expr, tag) => AnnotatedLambda(b, tpe, map(expr)(fn), fn(tag))
      case v@Var(_, _, _, _) => v.copy(tag = fn(v.tag))
      case App(fnT, arg, tpe, tag) => App(map(fnT)(fn), map(arg)(fn), tpe, fn(tag))
      case Let(b, e, in, r, t) => Let(b, map(e)(fn), map(in)(fn), r, fn(t))
      case lit@Literal(_, _, _) => lit.copy(tag = fn(lit.tag))
      case Match(arg, branches, tag) =>
        Match(map(arg)(fn), branches.map { case (p, t) => (p, map(t)(fn)) }, fn(tag))
    }
  }

  type Coerce = FunctionK[TypedExpr, TypedExpr]

  def coerceRho(tpe: Type.Rho): Coerce =
    tpe match {
      case Type.Fun(a: Type.Rho, b: Type.Rho) =>
        coerceFn(a, b, coerceRho(a), coerceRho(b))
      case _ =>
        new FunctionK[TypedExpr, TypedExpr] { self =>
          def apply[A](expr: TypedExpr[A]) =
            expr match {
              case Annotation(t, _, _) => self(t)
              case Generic(_, expr, _) =>
                // a Generic type is not a rho type,
                // so we discard the outer forAll and continue on
                self(expr)
              case Var(p, name, _, t) => Var(p, name, tpe, t)
              case AnnotatedLambda(arg, argT, res, tag) =>
                // only some coercions would make sense here
                // how to handle?
                // one way out could be to return a type to Annotation
                // and just wrap it in this case, could it be that simple?
                Annotation(expr, tpe, expr.tag)
              case App(fn, arg, _, tag) =>
                fn.getType match {
                  case Type.Fun(ta: Type.Rho, tr) =>
                    // we know that we should coerce arg with ta, and narrow tr to tpe
                    // this makes an infinite loop:
                    //val cfn = coerceRho(Type.Fun(ta, tpe))
                    //val fn1 = cfn(fn)
                    val carg = coerceRho(ta)
                    App(fn, carg(arg), tpe, tag)
                  case _ =>
                    // TODO, what should we do here?
                    // It is currently certainly wrong
                    // we have learned that the type is tpe
                    // but that implies something for fn and arg
                    // but we are ignoring that, which
                    // leaves them with potentially skolems or metavars
                    App(fn, arg, tpe, tag)
                }
              case Let(arg, argE, in, rec, tag) =>
                Let(arg, argE, self(in), rec, tag)
              case Literal(l, _, tag) => Literal(l, tpe, tag)
              case Match(arg, branches, tag) =>
                // TODO: this is wrong. We are leaving metas in the types
                // embedded in patterns
                Match(arg, branches.map { case (p, expr) => (p, self(expr)) }, tag)
            }
        }
    }

  /**
   * Return the list of the free vars
   */
  def freeVars[A](ts: List[TypedExpr[A]]): List[Identifier] = {

    // usually we can recurse in a loop, but sometimes not
    def cheat(te: TypedExpr[A], bound: Set[Identifier], acc: List[Identifier]): List[Identifier] =
      go(te :: Nil, bound, acc)

    @annotation.tailrec
    def go(ts: List[TypedExpr[A]], bound: Set[Identifier], acc: List[Identifier]): List[Identifier] =
      ts match {
        case Nil => acc
        case Generic(_, expr, _) :: tail =>
          go(expr :: tail, bound, acc)
        case Annotation(t, _, _) :: tail =>
          go(t :: tail, bound, acc)
        case Var(opt, ident, _, _) :: tail if bound(ident) || opt.isDefined => go(tail, bound, acc)
        case Var(None, name, _, _) :: tail => go(tail, bound, name :: acc)
        case AnnotatedLambda(arg, _, res, _) :: tail =>
          val acc1 = cheat(res, bound + arg, acc)
          go(tail, bound, acc1)
        case App(fn, arg, _, _) :: tail =>
          go(fn :: arg :: tail, bound, acc)
        case Let(arg, argE, in, rec, _) :: tail =>
          val barg = bound + arg
          val acc1 = cheat(in, barg, acc)
          if (rec.isRecursive) {
            // if rec is recursive, arg is in scope
            // also in argE
            val acc2 = cheat(argE, barg, acc1)
            go(tail, bound, acc2)
          }
          else {
            go(argE :: tail, bound, acc1)
          }
        case Literal(_, _, _) :: tail =>
          go(tail, bound, acc)
        case Match(arg, branches, _) :: tail =>
          // Maintain the order we encounter things:
          val acc1 = cheat(arg, bound, acc)
          val acc2 = branches.foldLeft(acc1) { case (acc1, (p, b)) =>
            // these are not free variables in this branch
            val newBinds = p.names
            cheat(b, bound ++ newBinds, acc1)
          }
          go(tail, bound, acc2)
      }

    go(ts, Set.empty, Nil)
      .reverse
      .distinct
  }

  /**
   * Try to substitute ex for ident in the expression: in
   *
   * This can fail if the free varriables in ex are shadowed
   * above ident in in.
   */
  def substitute[A](ident: Identifier, ex: TypedExpr[A], in: TypedExpr[A]): Option[TypedExpr[A]] = {
    val exfrees = freeVars(ex :: Nil).toSet

    val shadows: Identifier => Boolean = {
      if (exfrees.isEmpty) { i: Identifier => i === ident }
      else { i: Identifier => (i === ident || exfrees(i)) }
    }

    def loop(in: TypedExpr[A]): Option[TypedExpr[A]] =
      in match {
        case Var(None, i, _, _) if i === ident => Some(ex)
        case Var(_, _, _, _) | Literal(_, _, _) => Some(in)
        case Generic(a, expr, tag) =>
          loop(expr).map(Generic(a, _, tag))
        case Annotation(t, tpe, tag) =>
          loop(t).map(Annotation(_, tpe, tag))
        case AnnotatedLambda(arg, tp, res, tag) =>
          if (shadows(arg)) None
          else loop(res).map(AnnotatedLambda(arg, tp, _, tag))
        case App(fn, arg, tpe, tag) =>
          (loop(fn), loop(arg)).mapN(App(_, _, tpe, tag))
        case let@Let(arg, argE, in, rec, tag) =>
          if (shadows(arg)) None
          else {
            (loop(argE), loop(in)).mapN(Let(arg, _, _, rec, tag))
          }
        case Match(arg, branches, tag) =>
          // Maintain the order we encounter things:
          val arg1 = loop(arg)
          val b1 = branches.traverse { case branch@(p, b) =>
            // these are not free variables in this branch
            val s = p.names.exists(shadows)
            if (s) None
            else loop(b).map((p, _))
          }
          (arg1, b1).mapN(Match(_, _, tag))
      }

    loop(in)
  }

  private def replaceVarType[A](te: TypedExpr[A], name: Identifier, tpe: Type): TypedExpr[A] = {
    def recur(t: TypedExpr[A]) = replaceVarType(t, name, tpe)

    te match {
      case Generic(tv, in, tag) => Generic(tv, recur(in), tag)
      case Annotation(term, tpe, tag) => Annotation(recur(term), tpe, tag)
      case AnnotatedLambda(b, tpe, expr, tag) =>
        // this is a kind of let:
        if (b == name) {
          // we are shadowing, so we are done:
          te
        }
        else {
          // no shadow
          AnnotatedLambda(b, tpe, recur(expr), tag)
        }
      case Var(None, nm, _, tag) if nm == name => Var(None, name, tpe, tag)
      case v@Var(_, _, _, _) => v
      case App(fnT, arg, tpe, tag) =>
        App(recur(fnT), recur(arg), tpe, tag)
      case Let(b, e, in, r, t) =>
        if (b == name) {
          if (r.isRecursive) {
            // in this case, b is in scope for e
            // so it shadows a the previous definition
            te // shadow
          } else {
            // then b is not in scope for e
            // but b does shadow inside `in`
            Let(b, recur(e), in, r, t)
          }
        }
        else Let(b, recur(e), recur(in), r, t)
      case lit@Literal(_, _, _) => lit
      case Match(arg, branches, tag) =>
        Match(recur(arg), branches.map { case (p, t) => (p, recur(t)) }, tag)
    }
  }

  /**
   * TODO this seems pretty expensive to blindly apply: we are deoptimizing
   * the nodes pretty heavily
   */
  def coerceFn(arg: Type, result: Type.Rho, coarg: Coerce, cores: Coerce): Coerce =
    new FunctionK[TypedExpr, TypedExpr] { self =>
      def apply[A](expr: TypedExpr[A]) = {
        expr match {
          case Annotation(t, _, _) => self(t)
          case AnnotatedLambda(name, _, res, tag) =>
            // note, Var(None, name, originalType, tag)
            // is hanging out in res, or it is unused
            AnnotatedLambda(name, arg, cores(replaceVarType(res, name, arg)), tag)
          case Generic(_, in, _) => self(in)
          case Var(p, n, _, tag) =>
            Var(p, n, Type.Fun(arg, result), tag)
          case _ =>
            /*
             * We have to be careful not to collide with the free vars in expr
             */
            val free = SortedSet(freeVars(expr :: Nil): _*)
            val name = Type.allBinders.iterator.map { v => Identifier.Name(v.name) }.filterNot(free).next
            // \name -> (expr((name: arg)): result)
            // TODO: why do we need coarg when we already know the type (arg)?
            val result1 = cores(App(expr, coarg(Var(None, name, arg, expr.tag)), result, expr.tag))
            AnnotatedLambda(name, arg, result1, expr.tag)
        }
      }
    }

  def forAll[A](params: NonEmptyList[Type.Var.Bound], expr: TypedExpr[A]): TypedExpr[A] =
    expr match {
      case Generic(ps, ex0, tag) =>
        // if params and ps have duplicates, that
        // implies that those params weren't free, because
        // they have already been quantified by ps, so
        // they can be removed
        val newParams = params.toList.filterNot(ps.toList.toSet)
        val ps1 = NonEmptyList.fromList(newParams) match {
          case None => ps
          case Some(nep) => nep ::: ps
        }
        Generic(ps1, ex0, tag)
      case expr =>
        Generic(params, expr, expr.tag)
    }

  def lambda[A](arg: Bindable, tpe: Type, expr: TypedExpr[A], tag: A): TypedExpr[A] =
    expr match {
      case Generic(ps, ex0, tag0) =>
        val frees = Type.freeBoundTyVars(tpe :: Nil).toSet
        val quants = ps.toList.toSet
        val collisions = frees.intersect(quants)
        NonEmptyList.fromList(collisions.toList) match {
          case None => Generic(ps, AnnotatedLambda(arg, tpe, ex0, tag0), tag)
          case Some(cols) =>
            // we should sort cols if we use it for anything below
            // lift Generic out TODO: what if arg tpe is in ps? we need to substitute for a new name
            // we can rebind all the collisions to anything that isn't a free type var in expr
            // we should find a test that fails currently, add that test, then fix the issue
            sys.error(s"TODO: support lambda($arg, $tpe, ${expr.repr}, tag)")
        }
      case notGen =>
        AnnotatedLambda(arg, tpe, notGen, tag)
    }

  implicit def typedExprHasRegion[T: HasRegion]: HasRegion[TypedExpr[T]] =
    HasRegion.instance[TypedExpr[T]] { e => HasRegion.region(e.tag) }

  /**
   * if the te is not in normal form, transform it into normal form
   */
  def normalize[A](te: TypedExpr[A]): Option[TypedExpr[A]] =
    te match {
      case Generic(vars, in, tag) =>
        // normalize the inside, then get all the freeBoundTyVars and
        // and if we can reallocate typevars to be the a, b, ... do so,
        // if they are the same, return none
        //
        // Also, Generic(Generic(...)) => Generic
        // Generic(vs, te, _) but vs are not bound in te, we can remove Generic
        normalize(in).map(Generic(vars, _, tag))
      case Annotation(term, tpe, tag) =>
        // if we annotate twice, we can ignore the inner annotation
        // we should have type annotation where we normalize type parameters
        val e1 = normalize(term).getOrElse(te)
        e1 match {
          case _ if e1.getType == tpe =>
            // the type is already right
            Some(e1)
          case Annotation(t1, _, tag) =>
            Some(Annotation(t1, tpe, tag))
          case notAnn =>
            if (e1 eq te) None
            else Some(Annotation(notAnn, tpe, tag))
        }

      case AnnotatedLambda(arg, tpe, expr, tag) =>
        // we can normalize the arg to the smallest non-free var
        // \x -> f(x) == f (eta conversion)
        // \x -> generic(g) = generic(\x -> g) if the type of x doesn't have free types with vars
        val e1 = normalize(expr).getOrElse(te)
        e1 match {
          case App(fn, Var(None, ident, _, _), _, _) if ident == arg =>
            val tetpe = te.getType
            Some {
              if (fn.getType == tetpe) fn
              else Annotation(fn, tetpe, te.tag)
            }
          case notApp =>
            if (e1 eq te) None
            else Some(AnnotatedLambda(arg, tpe, notApp, tag))
        }

      case Var(_, _, _, _) | Literal(_, _, _) =>
        // these are fundamental
        None
      case a@App(fn, arg, tpe, tag) =>
        val f1 = normalize(fn).getOrElse(fn)
        f1 match {
          case AnnotatedLambda(b, ltpe, expr, ltag) =>
            // (\y -> z)(x) = let y = x in z
            val a2 = if (ltpe != arg.getType) Annotation(arg, ltpe, ltag) else arg
            val expr2 = if (tpe != expr.getType) Annotation(expr, tpe, expr.tag) else expr
            val l = Let(b, a2, expr, RecursionKind.NonRecursive, tag)
            normalize(l)
          case _ =>
            val a1 = normalize(arg).getOrElse(arg)
            if ((f1 eq fn) && (a1 eq arg)) None
            else Some(App(f1, a1, tpe, tag))
        }
      case Let(arg, ex, in, rec, tag) =>
        val in1 = normalize(in).getOrElse(in)
        val cnt = freeVars(in1 :: Nil).count(_ === (arg: Identifier))
        if (cnt > 0) {
          // the arg is needed
          val ex1 = normalize(ex).getOrElse(ex)
          def isSimple = ex match {
            case Literal(_, _, _) | Var(_, _, _, _) => true
            case _ => false
          }
          val shouldInline = (!rec.isRecursive) && {
            (cnt == 1) || isSimple
          }
          if (shouldInline) {
            // we should inline ex
            substitute(arg, ex1, in1)
          }
          else if ((in1 eq in) && (ex1 eq ex)) None
          else Some(Let(arg, ex1, in1, rec, tag))
        }
        else {
          // let x = y in z if x isn't free in z = z
          Some(in1)
        }

      case Match(arg, branches, tag) =>
        // match x:
        //   y: fn
        // let y = x in fn
        def ncount(e: TypedExpr[A]): (Int, TypedExpr[A]) =
          normalize(e) match {
            case None => (0, e)
            case Some(e) => (1, e)
          }
        // TODO normalize the patterns too
        // we can remove any bindings that aren't used in branches
        val (changed, branches1) = branches.traverse { case (p, t) =>
          val (c, t1) = ncount(t)
          (c, (p, t1))
        }
        val a1 = normalize(arg).getOrElse(arg)
        if ((a1 eq arg) && (changed == 0)) None
        else Some(Match(a1, branches1, tag))
    }
}
