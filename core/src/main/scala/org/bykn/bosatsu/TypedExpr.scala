package org.bykn.bosatsu

import cats.{Applicative, Eval, Monad, Monoid, Traverse}
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
      case Generic(params, expr) =>
        Type.forAll(params.toList, expr.getType)
      case Annotation(_, tpe, _) =>
        tpe
      case a@AnnotatedLambda(arg, tpe, res, _) =>
        Type.Fun(tpe, res.getType)
      case Local(_, tpe, _) => tpe
      case Global(_, _, tpe, _) => tpe
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
    def rept(t: Type): Doc = tfn(t).toDoc

    def loop(te: TypedExpr[T]): Doc = {
      te match {
        case Generic(params, expr) =>
          val pstr = Doc.intercalate(Doc.comma + Doc.lineOrSpace, params.toList.map { p => Doc.text(p.name) })
          (Doc.text("(generic") + Doc.lineOrSpace + Doc.char('[') + pstr + Doc.char(']') + Doc.lineOrSpace + loop(expr) + Doc.char(')')).nested(4)
        case Annotation(expr, tpe, _) =>
          (Doc.text("(ann") + Doc.lineOrSpace + rept(tpe) + Doc.lineOrSpace + loop(expr) + Doc.char(')')).nested(4)
        case a@AnnotatedLambda(arg, tpe, res, _) =>
          (Doc.text("(lambda") + Doc.lineOrSpace + Doc.text(arg.sourceCodeRepr) + Doc.lineOrSpace + rept(tpe) + Doc.lineOrSpace + loop(res) + Doc.char(')')).nested(4)
        case Local(v, tpe, _) =>
          (Doc.text("(var") + Doc.lineOrSpace + Doc.text(v.sourceCodeRepr) + Doc.lineOrSpace + rept(tpe) + Doc.char(')')).nested(4)
        case Global(p, v, tpe, _) =>
          val pstr = Doc.text(p.asString + "::" + v.sourceCodeRepr)
          (Doc.text("(var") + Doc.lineOrSpace + pstr + Doc.lineOrSpace + rept(tpe) + Doc.char(')')).nested(4)
        case App(fn, arg, tpe, _) =>
          (Doc.text("(ap") + Doc.lineOrSpace + loop(fn) + Doc.lineOrSpace + loop(arg) + Doc.lineOrSpace + rept(tpe) + Doc.char(')')).nested(4)
        case Let(n, b, in, rec, _) =>
          val nm = if (rec.isRecursive) Doc.text("(letrec") else Doc.text("(let")
          (nm + Doc.lineOrSpace + Doc.text(n.sourceCodeRepr) + Doc.lineOrSpace + loop(b) + Doc.lineOrSpace + loop(in) + Doc.char(')')).nested(4)
        case Literal(v, tpe, _) =>
          (Doc.text("(lit") + Doc.lineOrSpace + Doc.text(v.repr) + Doc.lineOrSpace + rept(tpe) + Doc.char(')')).nested(4)
        case Match(arg, branches, _) =>
          implicit val docType: Document[Type] =
            Document.instance { tpe => rept(tpe) }
          val cpat = Pattern.compiledDocument[Type]
          def pat(p: Pattern[(PackageName, Constructor), Type]): Doc =
            cpat.document(p)

          val bstr = branches.toList.map { case (p, t) => (Doc.char('[') + pat(p) + Doc.comma + Doc.lineOrSpace + loop(t) + Doc.char(']')).nested(4) }
          (Doc.text("(match") + Doc.lineOrSpace + loop(arg) + Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, bstr).nested(4) + Doc.char(')')).nested(4)
      }
    }

    loop(this).renderTrim(100)
  }


  /**
   * All the free variables in this expression in order
   * encountered and with duplicates (to see how often
   * they appear)
   */
  lazy val freeVarsDup: List[Bindable] =
    this match {
      case Generic(_, expr) =>
        expr.freeVarsDup
      case Annotation(t, _, _) =>
        t.freeVarsDup
      case Local(ident, _, _) =>
        ident :: Nil
      case Global(_, _, _, _) =>
        Nil
      case AnnotatedLambda(arg, _, res, _) =>
        TypedExpr.filterNot(res.freeVarsDup)(_ === arg)
      case App(fn, arg, _, _) =>
        fn.freeVarsDup ::: arg.freeVarsDup
      case Let(arg, argE, in, rec, _) =>
        val argFree0 = argE.freeVarsDup
        val argFree =
          if (rec.isRecursive) {
            TypedExpr.filterNot(argFree0)(_ === arg)
          }
          else argFree0

        argFree ::: (TypedExpr.filterNot(in.freeVarsDup)(_ === arg))
      case Literal(_, _, _) =>
        Nil
      case Match(arg, branches, _) =>
        val argFree = arg.freeVarsDup

        val branchFrees = branches.toList.map { case (p, b) =>
          // these are not free variables in this branch
          val newBinds = p.names.toSet
          val bfree = b.freeVarsDup
          if (newBinds.isEmpty) bfree
          else TypedExpr.filterNot(bfree)(newBinds)
        }
        // we can only take one branch, so count the max on each branch:
        val branchFreeMax = branchFrees
          .zipWithIndex
          .flatMap { case (names, br) => names.map((_, br)) }
          .groupBy(identity) // group-by-name x branch
          .map { case ((name, branch), names) => (names.length, branch, name) }
          .groupBy(_._3) // group by just the name now
          .toList
          .flatMap { case (_, vs) =>
            val (cnt, branch, name) = vs.maxBy(_._1)
            List.fill(cnt)((branch, name))
          }
          .sorted
          .map(_._2)

        argFree ::: branchFreeMax
    }

  def notFree(b: Bindable): Boolean =
    !freeVarsDup.contains(b)
}

object TypedExpr {

  // filter b from a pretty short lst but try to conserve lst if possible
  private def filterNot[A](lst: List[A])(b: A => Boolean): List[A] =
    lst match {
      case Nil => lst
      case h :: tail =>
        val t1 = filterNot(tail)(b)
        if (b(h)) t1
        else if (t1 eq tail) lst
        else (h :: t1) // we only allocate here
    }

  type Rho[A] = TypedExpr[A] // an expression with a Rho type (no top level forall)

  sealed abstract class Name[A] extends TypedExpr[A] with Product
  /**
   * This says that the resulting term is generic on a given param
   *
   * The paper says to add TyLam and TyApp nodes, but it never mentions what to do with them
   */
  case class Generic[T](typeVars: NonEmptyList[Type.Var.Bound], in: TypedExpr[T]) extends TypedExpr[T] {
    def tag: T = in.tag
  }
  case class Annotation[T](term: TypedExpr[T], coerce: Type, tag: T) extends TypedExpr[T]
  case class AnnotatedLambda[T](arg: Bindable, tpe: Type, expr: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Local[T](name: Bindable, tpe: Type, tag: T) extends Name[T]
  case class Global[T](pack: PackageName, name: Identifier, tpe: Type, tag: T) extends Name[T]
  case class App[T](fn: TypedExpr[T], arg: TypedExpr[T], result: Type, tag: T) extends TypedExpr[T]
  case class Let[T](arg: Bindable, expr: TypedExpr[T], in: TypedExpr[T], recursive: RecursionKind, tag: T) extends TypedExpr[T]
  // TODO, this shouldn't have a type, we know the type from Lit currently
  case class Literal[T](lit: Lit, tpe: Type, tag: T) extends TypedExpr[T]
  case class Match[T](arg: TypedExpr[T], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])], tag: T) extends TypedExpr[T]

  /**
   * For a recursive binding, what kind of call do we have?
   */
  def selfCallKind[A](nm: Bindable, expr: TypedExpr[A]): SelfCallKind = {
    val arity = Type.Fun.arity(expr.getType)
    callKind(nm, arity, expr)
  }

  sealed abstract class SelfCallKind {
    import SelfCallKind._

    // if you have two branches in match what is the result
    def merge(that: => SelfCallKind): SelfCallKind =
      this match {
        case NonTailCall => NonTailCall
        case NoCall => that
        case TailCall =>
          that match {
            case NonTailCall => NonTailCall
            case TailCall | NoCall => TailCall
          }
      }

    def callNotTail: SelfCallKind =
      this match {
        case NoCall => NoCall
        case NonTailCall | TailCall => NonTailCall
      }

    def ifNoCallThen(sc: => SelfCallKind): SelfCallKind =
      this match {
        case NoCall => sc
        case other => other
      }
  }

  object SelfCallKind {
    case object NoCall extends SelfCallKind
    case object TailCall extends SelfCallKind
    case object NonTailCall extends SelfCallKind
  }

  private def callKind[A](n: Bindable, arity: Int, te: TypedExpr[A]): SelfCallKind =
    te match {
      case Generic(_, in) => callKind(n, arity, in)
      case Annotation(te, _, _) => callKind(n, arity, te)
      case AnnotatedLambda(b, _, body, _) =>
        // let fn = \x -> fn(x) in fn(1)
        // is a tail-call
        if (n == b) {
          //shadow
          SelfCallKind.NoCall
        }
        else {
          callKind(n, arity, body)
        }
      case Global(_, _, _, _) => SelfCallKind.NoCall
      case Local(vn, _, _)  =>
        if (vn != n) SelfCallKind.NoCall
        else if (arity == 0) SelfCallKind.TailCall
        else SelfCallKind.NonTailCall
      case App(fn, arg, _, _) =>
        callKind(n, arity, arg)
          .callNotTail
          .ifNoCallThen(callKind(n, arity - 1, fn))
      case Let(arg, ex, in, rec, _) =>
        if (arg == n) {
          // shadow
          if (rec.isRecursive) {
            // shadow still in scope in ex
            SelfCallKind.NoCall
          }
          else {
            // ex isn't in tail position, so if there is a call, we aren't tail
            callKind(n, arity, ex).callNotTail
          }
        }
        else {
          callKind(n, arity, ex)
            .callNotTail
            .merge(callKind(n, arity, in))
        }
      case Literal(_, _, _) => SelfCallKind.NoCall
      case Match(arg, branches, _) =>
        callKind(n, arity, arg)
          .callNotTail
          .ifNoCallThen {
            // then we check all the branches
            branches.foldLeft(SelfCallKind.NoCall: SelfCallKind) { case (acc, (_, b)) =>
              acc.merge(callKind(n, arity, b))
            }
          }
    }

  /**
   * If we expect expr to be a lambda of the given arity, return
   * the parameter names and types and the rest of the body
   */
  def toArgsBody[A](arity: Int, expr: TypedExpr[A]): Option[(List[(Bindable, Type)], TypedExpr[A])] =
    expr match {
      case _ if arity == 0 =>
        Some((Nil, expr))
      case Generic(_, e) => toArgsBody(arity, e)
      case Annotation(e, _, _) => toArgsBody(arity, e)
      case AnnotatedLambda(name, tpe, expr, _) =>
        toArgsBody(arity - 1, expr).map { case (args0, body) =>
          ((name, tpe) :: args0, body)
        }
      case Let(arg, e, in, r, t) =>
        toArgsBody(arity, in).flatMap { case (args, body) =>
          // if args0 don't shadow arg, we can push
          // it down
          if (args.exists(_._1 == arg)) {
            // this we shadow, so we
            // can't lift, we could alpha-rename to
            // deal with this case
            None
          }
          else {
            // push it down:
            Some((args, Let(arg, e, body, r, t)))
          }
        }
      case Match(arg, branches, tag) =>
        val argSetO = branches.traverse { case (p, b) =>
          toArgsBody(arity, b).flatMap { case (n, b1) =>
            val nset: Bindable => Boolean = n.iterator.map(_._1).toSet
            if (p.names.exists(nset)) {
              // this we shadow, so we
              // can't lift, we could alpha-rename to
              // deal with this case
              None
            }
            else {
              Some((n, (p, b1)))
            }
          }
        }

        argSetO.flatMap { argSet =>
          if (argSet.map(_._1).toList.toSet.size == 1) {
            Some((argSet.head._1, Match(arg, argSet.map(_._2), tag)))
          }
          else {
            None
          }
        }
      case _ => None
    }

  private implicit val setM: Monoid[SortedSet[Type]] =
    new Monoid[SortedSet[Type]] {
      def empty = SortedSet.empty
      def combine(a: SortedSet[Type], b: SortedSet[Type]) = a ++ b
    }

  implicit class InvariantTypedExpr[A](val self: TypedExpr[A]) extends AnyVal {
    def updatedTag(t: A): TypedExpr[A] =
      self match {
        case Generic(ts, te) => Generic(ts, te.updatedTag(t))
        case a@Annotation(_, _, _) => a.copy(tag=t)
        case al@AnnotatedLambda(_, _, _, _) => al.copy(tag=t)
        case v@Local(_, _, _) => v.copy(tag=t)
        case g@Global(_, _, _, _) => g.copy(tag=t)
        case a@App(_, _, _, _) => a.copy(tag=t)
        case let@Let(_, _, _, _, _) => let.copy(tag=t)
        case lit@Literal(_, _, _) => lit.copy(tag=t)
        case m@Match(_, _, _) => m.copy(tag=t)
      }

    def allTypes: SortedSet[Type] =
      traverseType { t => Writer(SortedSet(t), t) }.run._1

    /**
     * Traverse all the *non-shadowed* types inside the TypedExpr
     */
    def traverseType[F[_]: Applicative](fn: Type => F[Type]): F[TypedExpr[A]] =
      self match {
        case gen@Generic(params, expr) =>
          // params shadow below, so they are not free values
          // and can easily create bugs if passed into fn
          val shadowed: Set[Type.Var.Bound] = params.toList.toSet
          val shadowFn: Type => F[Type] = {
            case tvar@Type.TyVar(v: Type.Var.Bound) if shadowed(v) => Applicative[F].pure(tvar)
            case notShadowed => fn(notShadowed)
          }

          val paramsF = params.traverse_ { v => fn(Type.TyVar(v)) }
          (paramsF *> fn(gen.getType) *> expr.traverseType(shadowFn))
            .map(Generic(params, _))
        case Annotation(of, tpe, tag) =>
          (of.traverseType(fn), fn(tpe)).mapN(Annotation(_, _, tag))
        case lam@AnnotatedLambda(arg, tpe, res, tag) =>
          fn(lam.getType) *> (fn(tpe), res.traverseType(fn)).mapN {
            AnnotatedLambda(arg, _, _, tag)
          }
        case Local(v, tpe, tag) =>
          fn(tpe).map(Local(v, _, tag))
        case Global(p, v, tpe, tag) =>
          fn(tpe).map(Global(p, v, _, tag))
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

    /**
     * This applies fn on all the contained types, replaces the elements, then calls on the
     * resulting. This is "bottom up"
     */
    def traverseUp[F[_]: Monad](fn: TypedExpr[A] => F[TypedExpr[A]]): F[TypedExpr[A]] = {
      // be careful not to mistake loop with fn
      def loop(te: TypedExpr[A]): F[TypedExpr[A]] = te.traverseUp(fn)

      self match {
        case Generic(params, expr) =>
          loop(expr).flatMap { fx =>
            fn(Generic(params, fx))
          }
        case Annotation(of, tpe, tag) =>
          loop(of).flatMap { o2 =>
            fn(Annotation(o2, tpe, tag))
          }
        case AnnotatedLambda(arg, tpe, res, tag) =>
          loop(res).flatMap { res1 =>
            fn(AnnotatedLambda(arg, tpe, res1, tag))
          }
        case v@(Global(_, _, _, _) | Local(_, _, _) | Literal(_, _, _)) =>
            fn(v)
        case App(f, arg, tpe, tag) =>
          (loop(f), loop(arg))
            .mapN(App(_, _, tpe, tag))
            .flatMap(fn)
        case Let(v, exp, in, rec, tag) =>
          (loop(exp), loop(in))
            .mapN(Let(v, _, _, rec, tag))
            .flatMap(fn)
        case Match(expr, branches, tag) =>
          val tbranch = branches.traverse {
            case (p, t) => loop(t).map((p, _))
          }
          (loop(expr), tbranch)
            .mapN(Match(_, _, tag))
            .flatMap(fn)
      }
    }

    /**
     * Here are all the global names inside this expression
     */
    def globals: Set[Global[A]] =
      traverseUp[Writer[Set[Global[A]], *]] {
        case g @ Global(_, _, _, _) => Writer.tell(Set(g)).as(g)
        case notG => Monad[Writer[Set[Global[A]], *]].pure(notG)
      }
      .written
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
        case Generic(typeVars, in) =>
          deepQuantify(env, in).map { in1 =>
            forAll(typeVars, in1)
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
              case Generic(ps, expr) =>
                finish(expr).map(forAll(ps, _))
              case unreach =>
                // $COVERAGE-OFF$
                sys.error(s"Match quantification yielded neither Generic nor Match")
                // $COVERAGE-ON$
            }

          noArg.flatMap(finish)

        case nonest@(Global(_, _, _, _) | Local(_, _, _) | Literal(_, _, _)) =>
          Applicative[F].pure(nonest)
      }

    deepQuantify(env, rho)
  }

  implicit val traverseTypedExpr: Traverse[TypedExpr] = new Traverse[TypedExpr] {
    def traverse[F[_]: Applicative, T, S](typedExprT: TypedExpr[T])(fn: T => F[S]): F[TypedExpr[S]] =
      typedExprT match {
        case Generic(params, expr) =>
          expr.traverse(fn).map(Generic(params, _))
        case Annotation(of, tpe, tag) =>
          (of.traverse(fn), fn(tag)).mapN(Annotation(_, tpe, _))
        case AnnotatedLambda(arg, tpe, res, tag) =>
          (res.traverse(fn), fn(tag)).mapN {
            AnnotatedLambda(arg, tpe, _, _)
          }
        case Local(v, tpe, tag) =>
          fn(tag).map(Local(v, tpe, _))
        case Global(p, v, tpe, tag) =>
          fn(tag).map(Global(p, v, tpe, _))
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
      case Generic(_, e) =>
        foldLeft(e, b)(f)
      case Annotation(e, _, tag) =>
        val b1 = foldLeft(e, b)(f)
        f(b1, tag)
      case AnnotatedLambda(_, _, e, tag) =>
        val b1 = foldLeft(e, b)(f)
        f(b1, tag)
      case n: Name[A] => f(b, n.tag)
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
      case Generic(_, e) =>
        foldRight(e, lb)(f)
      case Annotation(e, _, tag) =>
        val lb1 = foldRight(e, lb)(f)
        f(tag, lb1)
      case AnnotatedLambda(_, _, e, tag) =>
        val lb1 = foldRight(e, lb)(f)
        f(tag, lb1)
      case n: Name[A] => f(n.tag, lb)
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
      case Generic(tv, in) => Generic(tv, map(in)(fn))
      case Annotation(term, tpe, tag) => Annotation(map(term)(fn), tpe, fn(tag))
      case AnnotatedLambda(b, tpe, expr, tag) => AnnotatedLambda(b, tpe, map(expr)(fn), fn(tag))
      case l@Local(_, _, _) => l.copy(tag = fn(l.tag))
      case g@Global(_, _, _, _) => g.copy(tag = fn(g.tag))
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
              case Generic(_, expr) =>
                // a Generic type is not a rho type,
                // so we discard the outer forAll and continue on
                self(expr)
              case Local(name, _, t) => Local(name, tpe, t)
              case Global(p, name, _, t) => Global(p, name, tpe, t)
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
  def freeVars[A](ts: List[TypedExpr[A]]): List[Bindable] =
    freeVarsDup(ts).distinct

  def freeVarsSet[A](ts: List[TypedExpr[A]]): SortedSet[Bindable] =
    SortedSet(freeVarsDup(ts): _*)

  private def freeVarsDup[A](ts: List[TypedExpr[A]]): List[Bindable] =
    ts.flatMap(_.freeVarsDup)

  /**
   * Try to substitute ex for ident in the expression: in
   *
   * This can fail if the free variables in ex are shadowed
   * above ident in in.
   *
   * this code is very similar to Declaration.substitute
   * if bugs are found in one, consult the other
   */
  def substitute[A](ident: Bindable, ex: TypedExpr[A], in: TypedExpr[A]): Option[TypedExpr[A]] = {
    // if we hit a shadow, we don't need to substitute down
    // that branch
    @inline def shadows(i: Bindable): Boolean = i === ident

    // free variables in ex are being rebound,
    // this causes us to return None
    lazy val masks: Bindable => Boolean =
      freeVarsSet(ex :: Nil)

    def loop(in: TypedExpr[A]): Option[TypedExpr[A]] =
      in match {
        case Local(i, _, _) if i === ident => Some(ex)
        case Global(_, _, _, _) | Local(_, _, _) | Literal(_, _, _) => Some(in)
        case Generic(a, expr) =>
          loop(expr).map(Generic(a, _))
        case Annotation(t, tpe, tag) =>
          loop(t).map(Annotation(_, tpe, tag))
        case AnnotatedLambda(arg, tp, res, tag) =>
          if (masks(arg)) None
          else if (shadows(arg)) Some(in)
          else loop(res).map(AnnotatedLambda(arg, tp, _, tag))
        case App(fn, arg, tpe, tag) =>
          (loop(fn), loop(arg)).mapN(App(_, _, tpe, tag))
        case let@Let(arg, argE, in, rec, tag) =>
          if (masks(arg)) None
          else if (shadows(arg)) {
            // recursive shadow blocks both argE and in
            if (rec.isRecursive) Some(let)
            else loop(argE).map(Let(arg, _, in, rec, tag))
          }
          else {
            (loop(argE), loop(in)).mapN(Let(arg, _, _, rec, tag))
          }
        case Match(arg, branches, tag) =>
          // Maintain the order we encounter things:
          val arg1 = loop(arg)
          val b1 = branches.traverse { case in@(p, b) =>
            // these are not free variables in this branch
            val ns = p.names
            if (ns.exists(masks)) None
            else if (ns.exists(shadows)) Some(in)
            else loop(b).map((p, _))
          }
          (arg1, b1).mapN(Match(_, _, tag))
      }

    loop(in)
  }

  def substituteTypeVar[A](typedExpr: TypedExpr[A], env: Map[Type.Var, Type]): TypedExpr[A] =
    typedExpr match {
      case Generic(params, expr) =>
        // we need to remove the params which are shadowed below
        val paramSet: Set[Type.Var] = params.toList.toSet
        val env1 = env.filterKeys { k => !paramSet(k) }
        Generic(params, substituteTypeVar(expr, env1))
      case Annotation(of, tpe, tag) =>
        Annotation(
          substituteTypeVar(of, env),
          Type.substituteVar(tpe, env),
          tag)
      case AnnotatedLambda(arg, tpe, res, tag) =>
        AnnotatedLambda(
          arg,
          Type.substituteVar(tpe, env),
          substituteTypeVar(res, env),
          tag)
      case Local(v, tpe, tag) =>
        Local(v, Type.substituteVar(tpe, env), tag)
      case Global(p, v, tpe, tag) =>
        Global(p, v, Type.substituteVar(tpe, env), tag)
      case App(f, arg, tpe, tag) =>
        App(
          substituteTypeVar(f, env),
          substituteTypeVar(arg, env),
          Type.substituteVar(tpe, env),
          tag)
      case Let(v, exp, in, rec, tag) =>
        Let(
          v,
          substituteTypeVar(exp, env),
          substituteTypeVar(in, env),
          rec,
          tag)
      case Literal(lit, tpe, tag) =>
        Literal(lit, Type.substituteVar(tpe, env), tag)
      case Match(expr, branches, tag) =>
        val branches1 = branches.map {
          case (p, t) =>
            val p1 = p.mapType(Type.substituteVar(_, env))
            val t1 = substituteTypeVar(t, env)
            (p1, t1)
        }
        val expr1 = substituteTypeVar(expr, env)
        Match(expr1, branches1, tag)
    }

  private def replaceVarType[A](te: TypedExpr[A], name: Bindable, tpe: Type): TypedExpr[A] = {
    def recur(t: TypedExpr[A]) = replaceVarType(t, name, tpe)

    te match {
      case Generic(tv, in) => Generic(tv, recur(in))
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
      case Local(nm, _, tag) if nm == name => Local(name, tpe, tag)
      case n: Name[A] => n
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
          case Generic(_, in) => self(in)
          case Local(n, _, tag) =>
            Local(n, Type.Fun(arg, result), tag)
          case Global(p, n, _, tag) =>
            Global(p, n, Type.Fun(arg, result), tag)
          case _ =>
            /*
             * We have to be careful not to collide with the free vars in expr
             */
            val free = freeVarsSet(expr :: Nil)
            val name = Type.allBinders.iterator.map { v => Identifier.Name(v.name) }.filterNot(free).next
            // \name -> (expr((name: arg)): result)
            // TODO: why do we need coarg when we already know the type (arg)?
            val result1 = cores(App(expr, coarg(Local(name, arg, expr.tag)), result, expr.tag))
            AnnotatedLambda(name, arg, result1, expr.tag)
        }
      }
    }

  def forAll[A](params: NonEmptyList[Type.Var.Bound], expr: TypedExpr[A]): TypedExpr[A] =
    expr match {
      case Generic(ps, ex0) =>
        // if params and ps have duplicates, that
        // implies that those params weren't free, because
        // they have already been quantified by ps, so
        // they can be removed
        val newParams = params.toList.filterNot(ps.toList.toSet)
        val ps1 = NonEmptyList.fromList(newParams) match {
          case None => ps
          case Some(nep) => nep ::: ps
        }
        Generic(ps1, ex0)
      case expr =>
        Generic(params, expr)
    }

  def lambda[A](arg: Bindable, tpe: Type, expr: TypedExpr[A], tag: A): TypedExpr[A] =
    expr match {
      case Generic(ps, ex0) =>
        // due to covariance in the return type, we can always lift
        // generics on the return value out of the lambda
        val frees = Type.freeBoundTyVars(tpe :: Nil).toSet
        val quants = ps.toList.toSet
        val collisions = frees.intersect(quants)
        NonEmptyList.fromList(collisions.toList) match {
          case None => Generic(ps, AnnotatedLambda(arg, tpe, ex0, tag))
          case Some(cols) =>
            // don't replace with any existing type variable or any of the free variables
            val replacements = Type.allBinders.iterator.filterNot(quants | frees)
            val repMap: Map[Type.Var.Bound, Type.Var.Bound] =
              collisions
                .iterator
                .zip(replacements)
                .toMap

            val ps1 = ps.map { v => repMap.getOrElse(v, v) }
            val typeMap: Map[Type.Var, Type] =
              repMap.iterator.map { case (k, v) => (k, Type.TyVar(v)) }.toMap
            val ex1 = substituteTypeVar(ex0, typeMap)
            Generic(ps1,
              AnnotatedLambda(arg, tpe, ex1, tag))
        }
      case notGen =>
        AnnotatedLambda(arg, tpe, notGen, tag)
    }

  implicit def typedExprHasRegion[T: HasRegion]: HasRegion[TypedExpr[T]] =
    HasRegion.instance[TypedExpr[T]] { e => HasRegion.region(e.tag) }
}
