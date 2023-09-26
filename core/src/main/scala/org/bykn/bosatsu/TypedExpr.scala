package org.bykn.bosatsu

import cats.{Applicative, Eval, Monad, Traverse}
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
      case g@Generic(_, _) => g.forAllType
      case Annotation(_, tpe) =>
        tpe
      case AnnotatedLambda(args, res, _) =>
        Type.Fun(args.map(_._2), res.getType)
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

  lazy val size: Int = 
    this match {
      case Generic(_, g) => g.size
      case Annotation(a, _) => a.size
      case AnnotatedLambda(_, res, _) =>
        res.size
      case Local(_, _, _) | Literal(_, _, _) | Global(_, _, _, _) => 1
      case App(fn, args, _, _) => fn.size + args.foldMap(_.size)
      case Let(_, e, in, _, _) => e.size + in.size
      case Match(a, branches, _) =>
        a.size + branches.foldMap(_._2.size)
  }

  // TODO: we need to make sure this parsable and maybe have a mode that has the compiler
  // emit these
  def repr: String = {
    def rept(t: Type): Doc = Type.fullyResolvedDocument.document(t)

    def loop(te: TypedExpr[T]): Doc = {
      te match {
        case Generic(params, expr) =>
          val pstr = Doc.intercalate(
            Doc.comma + Doc.lineOrSpace,
            params.toList.map { case (p, k) => Doc.text(p.name) + Doc.text(": ") + k.toDoc }
          )
          (Doc.text("(generic") + Doc.lineOrSpace + Doc.char('[') + pstr + Doc.char(']') + Doc.lineOrSpace + loop(expr) + Doc.char(')')).nested(4)
        case Annotation(expr, tpe) =>
          (Doc.text("(ann") + Doc.lineOrSpace + rept(tpe) + Doc.lineOrSpace + loop(expr) + Doc.char(')')).nested(4)
        case AnnotatedLambda(args, res, _) =>
          (Doc.text("(lambda") + Doc.lineOrSpace + (
              Doc.char('[') + Doc.intercalate(Doc.lineOrSpace, args.toList.map { case (arg, tpe) =>
                Doc.text(arg.sourceCodeRepr) + Doc.lineOrSpace + rept(tpe)
              }) + Doc.char(']')
            ) + Doc.lineOrSpace + loop(res) + Doc.char(')')).nested(4)
        case Local(v, tpe, _) =>
          (Doc.text("(var") + Doc.lineOrSpace + Doc.text(v.sourceCodeRepr) + Doc.lineOrSpace + rept(tpe) + Doc.char(')')).nested(4)
        case Global(p, v, tpe, _) =>
          val pstr = Doc.text(p.asString + "::" + v.sourceCodeRepr)
          (Doc.text("(var") + Doc.lineOrSpace + pstr + Doc.lineOrSpace + rept(tpe) + Doc.char(')')).nested(4)
        case App(fn, args, tpe, _) =>
          val argsDoc = Doc.intercalate(Doc.lineOrSpace, args.toList.map(loop))
          (Doc.text("(ap") + Doc.lineOrSpace + loop(fn) + Doc.lineOrSpace + argsDoc + Doc.lineOrSpace + rept(tpe) + Doc.char(')')).nested(4)
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
    // nearly identical code to Expr.freeVarsDup, bugs should be fixed in both places
    this match {
      case Generic(_, expr) =>
        expr.freeVarsDup
      case Annotation(t, _) =>
        t.freeVarsDup
      case Local(ident, _, _) =>
        ident :: Nil
      case Global(_, _, _, _) =>
        Nil
      case AnnotatedLambda(args, res, _) =>
        val nameSet = args.toList.iterator.map(_._1).toSet
        ListUtil.filterNot(res.freeVarsDup)(nameSet)
      case App(fn, args, _, _) =>
        fn.freeVarsDup ::: args.reduceMap(_.freeVarsDup)
      case Let(arg, argE, in, rec, _) =>
        val argFree0 = argE.freeVarsDup
        val argFree =
          if (rec.isRecursive) {
            ListUtil.filterNot(argFree0)(_ === arg)
          }
          else argFree0

        argFree ::: (ListUtil.filterNot(in.freeVarsDup)(_ === arg))
      case Literal(_, _, _) =>
        Nil
      case Match(arg, branches, _) =>
        val argFree = arg.freeVarsDup

        val branchFrees = branches.toList.map { case (p, b) =>
          // these are not free variables in this branch
          val newBinds = p.names.toSet
          val bfree = b.freeVarsDup
          if (newBinds.isEmpty) bfree
          else ListUtil.filterNot(bfree)(newBinds)
        }
        // we can only take one branch, so count the max on each branch:
        val branchFreeMax = branchFrees
          .zipWithIndex
          .flatMap { case (names, br) => names.map((_, br)) }
          // these groupBys are okay because we sort at the end
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

  type Rho[A] = TypedExpr[A] // an expression with a Rho type (no top level forall)

  sealed abstract class Name[A] extends TypedExpr[A] with Product
  /**
   * This says that the resulting term is generic on a given param
   *
   * The paper says to add TyLam and TyApp nodes, but it never mentions what to do with them
   */
  case class Generic[T](typeVars: NonEmptyList[(Type.Var.Bound, Kind)], in: TypedExpr[T]) extends TypedExpr[T] {
    def tag: T = in.tag

    lazy val forAllType: Type.ForAll = Type.forAll(typeVars, in.getType)
  }
  // Annotation really means "widen", the term has a type that is a subtype of coerce, so we are widening
  // to the given type. This happens on Locals/Globals also in their tpe
  case class Annotation[T](term: TypedExpr[T], coerce: Type) extends TypedExpr[T] {
    def tag: T = term.tag
  }
  case class AnnotatedLambda[T](args: NonEmptyList[(Bindable, Type)], expr: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Local[T](name: Bindable, tpe: Type, tag: T) extends Name[T]
  case class Global[T](pack: PackageName, name: Identifier, tpe: Type, tag: T) extends Name[T]
  case class App[T](fn: TypedExpr[T], args: NonEmptyList[TypedExpr[T]], result: Type, tag: T) extends TypedExpr[T]
  case class Let[T](arg: Bindable, expr: TypedExpr[T], in: TypedExpr[T], recursive: RecursionKind, tag: T) extends TypedExpr[T]
  // TODO, this shouldn't have a type, we know the type from Lit currently
  case class Literal[T](lit: Lit, tpe: Type, tag: T) extends TypedExpr[T]
  case class Match[T](arg: TypedExpr[T], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])], tag: T) extends TypedExpr[T]

  def letAllNonRec[T](binds: NonEmptyList[(Bindable, TypedExpr[T])], in: TypedExpr[T], tag: T): Let[T] = {
    val in1 = binds.tail match {
      case Nil => in
      case h1 :: t1 => letAllNonRec(NonEmptyList(h1, t1), in, tag)
    }
    val (n, ne) = binds.head
    Let(n, ne, in1, RecursionKind.NonRecursive, tag)
  }

  /**
   * If we expect expr to be a lambda of the given arity, return
   * the parameter names and types and the rest of the body
   */
  def toArgsBody[A](arity: Int, expr: TypedExpr[A]): Option[(NonEmptyList[(Bindable, Type)], TypedExpr[A])] =
    expr match {
      case Generic(_, e) => toArgsBody(arity, e)
      case Annotation(e, _) => toArgsBody(arity, e)
      case AnnotatedLambda(args, expr, _) =>
        if (args.length == arity) {
          Some((args, expr))
        }
        else {
          None
        }
      case Let(arg, e, in, r, t) =>
        toArgsBody(arity, in).flatMap { case (args, body) =>
          // if args0 don't shadow arg, we can push
          // it down
          if (args.exists(_._1 === arg)) {
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

  implicit class InvariantTypedExpr[A](val self: TypedExpr[A]) extends AnyVal {
    def allTypes: SortedSet[Type] =
      traverseType { t => Writer[SortedSet[Type], Type](SortedSet(t), t) }.run._1

    /**
     * Traverse all the *non-shadowed* types inside the TypedExpr
     */
    def traverseType[F[_]: Applicative](fn: Type => F[Type]): F[TypedExpr[A]] =
      self match {
        case gen@Generic(params, expr) =>
          // params shadow below, so they are not free values
          // and can easily create bugs if passed into fn
          val shadowed: Set[Type.Var.Bound] = params.toList.iterator.map(_._1).toSet
          val shadowFn: Type => F[Type] = {
            case tvar@Type.TyVar(v: Type.Var.Bound) if shadowed(v) => Applicative[F].pure(tvar)
            case notShadowed => fn(notShadowed)
          }

          val paramsF = params.traverse_ { v => fn(Type.TyVar(v._1)) }
          (paramsF *> fn(gen.getType) *> expr.traverseType(shadowFn))
            .map(Generic(params, _))
        case Annotation(of, tpe) =>
          (of.traverseType(fn), fn(tpe)).mapN(Annotation(_, _))
        case lam@AnnotatedLambda(args, res, tag) =>
          val a1 = args.traverse { case (n, t) => fn(t).map(n -> _) }
          fn(lam.getType) *> (a1, res.traverseType(fn)).mapN {
            AnnotatedLambda( _, _, tag)
          }
        case Local(v, tpe, tag) =>
          fn(tpe).map(Local(v, _, tag))
        case Global(p, v, tpe, tag) =>
          fn(tpe).map(Global(p, v, _, tag))
        case App(f, args, tpe, tag) =>
          (f.traverseType(fn), args.traverse(_.traverseType(fn)), fn(tpe)).mapN {
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
        case Annotation(of, tpe) =>
          loop(of).flatMap { o2 =>
            fn(Annotation(o2, tpe))
          }
        case AnnotatedLambda(args, res, tag) =>
          loop(res).flatMap { res1 =>
            fn(AnnotatedLambda(args, res1, tag))
          }
        case v@(Global(_, _, _, _) | Local(_, _, _) | Literal(_, _, _)) =>
            fn(v)
        case App(f, args, tpe, tag) =>
          (loop(f), args.traverse(loop(_)))
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
    def globals: Set[(PackageName, Identifier)] =
      traverseUp[Writer[Set[(PackageName, Identifier)], *]] {
        case g @ Global(p, i, _, _) => Writer.tell(Set[(PackageName, Identifier)]((p, i))).as(g)
        case notG => Monad[Writer[Set[(PackageName, Identifier)], *]].pure(notG)
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
          val bound = aligned.traverse { case (m, n) => writeFn(m, n).as((n, m.kind)) }
          // we only need to zonk after doing a write:
          (bound, zonkMeta(rho)(zFn)).mapN { (typeArgs, r) => forAll(typeArgs, r) }
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
        case Annotation(term, coerce) =>
          deepQuantify(env, term).map { t1 =>
            Annotation(t1, coerce)
          }
        case AnnotatedLambda(args, expr, tag) =>
          val env1 = env ++ args.iterator.map { case (arg, tpe) => ((None, arg)) -> tpe }
          deepQuantify(env1, expr)
            .map { e1 =>
              lambda(args, e1, tag)
            }
        case Let(arg, expr, in, rec, tag) =>
          // this introduces something into the env
          val inEnv = env.updated((None, arg), expr.getType)
          val exprEnv = if (rec.isRecursive) inEnv else env
          (deepQuantify(exprEnv, expr), deepQuantify(inEnv, in))
            .mapN { (e1, i1) =>
              Let(arg, e1, i1, rec, tag)
            }
        case App(fn, args, tpe, tag) =>
          (deepQuantify(env, fn), args.traverse(deepQuantify(env, _)))
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
            p.traverseType { t => Writer[SortedSet[Type], Type](SortedSet(t), t) }.run._1

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
                sys.error(s"Match quantification yielded neither Generic nor Match: $unreach")
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
        case Annotation(of, tpe) =>
          of.traverse(fn).map(Annotation(_, tpe))
        case AnnotatedLambda(args, res, tag) =>
          (res.traverse(fn), fn(tag)).mapN {
            AnnotatedLambda(args, _, _)
          }
        case Local(v, tpe, tag) =>
          fn(tag).map(Local(v, tpe, _))
        case Global(p, v, tpe, tag) =>
          fn(tag).map(Global(p, v, tpe, _))
        case App(f, args, tpe, tag) =>
          (f.traverse(fn), args.traverse(_.traverse(fn)), fn(tag)).mapN {
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
      case Annotation(e, _) =>
        foldLeft(e, b)(f)
      case AnnotatedLambda(_, e, tag) =>
        val b1 = foldLeft(e, b)(f)
        f(b1, tag)
      case n: Name[A] => f(b, n.tag)
      case App(fn, args, _, tag) =>
        val b1 = foldLeft(fn, b)(f)
        val b2 = args.foldLeft(b1)((b1, a) => foldLeft(a, b1)(f))
        f(b2, tag)
      case Let(_, exp, in, _, tag) =>
        val b1 = foldLeft(exp, b)(f)
        val b2 = foldLeft(in, b1)(f)
        f(b2, tag)
      case Literal(_, _, tag) =>
        f(b, tag)
      case Match(arg, branches, tag) =>
        val b1 = foldLeft(arg, b)(f)
        val b2 = branches.foldLeft(b1) { case (bn, (_, t)) => foldLeft(t, bn)(f) }
        f(b2, tag)
    }

    def foldRight[A, B](typedExprA: TypedExpr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = typedExprA match {
      case Generic(_, e) =>
        foldRight(e, lb)(f)
      case Annotation(e, _) =>
        foldRight(e, lb)(f)
      case AnnotatedLambda(_, e, tag) =>
        val lb1 = f(tag, lb)
        foldRight(e, lb1)(f)
      case n: Name[A] => f(n.tag, lb)
      case App(fn, args, _, tag) =>
        val b1 = f(tag, lb)
        val b2 = args.toList.foldRight(b1)((a, b1) => foldRight(a, b1)(f))
        foldRight(fn, b2)(f)
      case Let(_, exp, in, _, tag) =>
        val b1 = f(tag, lb)
        val b2 = foldRight(in, b1)(f)
        foldRight(exp, b2)(f)
      case Literal(_, _, tag) =>
        f(tag, lb)
      case Match(arg, branches, tag) =>
        val b1 = f(tag, lb)
        val b2 = branches.foldRight(b1) { case ((_, t), bn) => foldRight(t, bn)(f) }
        foldRight(arg, b2)(f)
    }

    override def map[A, B](te: TypedExpr[A])(fn: A => B): TypedExpr[B] = te match {
      case Generic(tv, in) => Generic(tv, map(in)(fn))
      case Annotation(term, tpe) => Annotation(map(term)(fn), tpe)
      case AnnotatedLambda(args, expr, tag) => AnnotatedLambda(args, map(expr)(fn), fn(tag))
      case l@Local(_, _, _) => l.copy(tag = fn(l.tag))
      case g@Global(_, _, _, _) => g.copy(tag = fn(g.tag))
      case App(fnT, args, tpe, tag) => App(map(fnT)(fn), args.map(map(_)(fn)), tpe, fn(tag))
      case Let(b, e, in, r, t) => Let(b, map(e)(fn), map(in)(fn), r, fn(t))
      case lit@Literal(_, _, _) => lit.copy(tag = fn(lit.tag))
      case Match(arg, branches, tag) =>
        Match(map(arg)(fn), branches.map { case (p, t) => (p, map(t)(fn)) }, fn(tag))
    }
  }

  type Coerce = FunctionK[TypedExpr, TypedExpr]

  private def pushDownCovariant(tpe: Type, kinds: Type => Option[Kind]): Type = {
    import Type._
    tpe match {
      case ForAll(targs, in) =>
        val (cons, cargs) = Type.unapplyAll(in)
        kinds(cons) match {
          case None =>
            sys.error(s"unknown kind of $cons in $tpe")
          case Some(kind) =>

            val kindArgs = kind.toArgs
            val kindArgsWithArgs = kindArgs.zip(cargs).map { case (ka, a) => (Some(ka), a) } :::
              cargs.drop(kindArgs.length).map((None, _))

            val argsVectorIdx = kindArgsWithArgs
              .iterator
              .zipWithIndex
              .map { case ((optKA, tpe), idx) =>
                (Type.freeBoundTyVars(tpe :: Nil).toSet, optKA, tpe, idx)
              }
              .toVector

            // if an arg is covariant, it can pull all it's unique freeVars
            def uniqueFreeVars(idx: Int): Set[Type.Var.Bound] = {
              val (justIdx, optKA, _, _) = argsVectorIdx(idx)
              if (optKA.exists(_.variance == Variance.co)) {
                argsVectorIdx.iterator.filter(_._4 != idx)
                  .foldLeft(justIdx) { case (acc, (s, _, _, _)) => acc -- s }
              }
              else Set.empty
            }
            val withPulled = argsVectorIdx.map { case rec@(_, _, _, idx) =>
              (rec, uniqueFreeVars(idx))
            }
            val allPulled: Set[Type.Var.Bound] = withPulled.foldMap(_._2)
            val nonpulled = targs.filterNot { case (v, _) => allPulled(v) }
            val pulledArgs = withPulled.iterator.map { case ((_, _, tpe, _), uniques) =>
              val keep: Type.Var.Bound => Boolean = uniques
              Type.forAll(targs.filter { case (t, _) => keep(t) }, tpe)
            }
            .toList
            Type.forAll(nonpulled, Type.applyAll(cons, pulledArgs))
          }
        
      case _ => tpe
    }
  }

  // We know initTpe <:< instTpe, we may be able to simply
  // fix some of the universally quantified variables
  def instantiateTo[A](gen: Generic[A], instTpe: Type.Rho, kinds: Type => Option[Kind]): TypedExpr[A] = {
    import Type._

    def solve(left: Type, right: Type, state: Map[Type.Var, Type], solveSet: Set[Type.Var]): Option[Map[Type.Var, Type]] =
      (left, right) match {
        case (TyVar(v), right) if solveSet(v) =>
          Some(state.updated(v, right))
        case (ForAll(b, i), r) =>
          // this will mask solving for the inside values:
          solve(i, r, state, solveSet -- b.toList.iterator.map(_._1))
        case (_, fa@ForAll(_, _)) =>
          val fa1 = pushDownCovariant(fa, kinds)
          if (fa1 != fa) solve(left, fa1, state, solveSet)
          else {
            // not clear what to do here,
            // the examples that come up look like un-unified
            // types, as if coerceRho is called before we have
            // finished unifying
            None
          }
        case (TyApply(on, arg), TyApply(on2, arg2)) =>
          for {
            s1 <- solve(on, on2, state, solveSet)
            s2 <- solve(arg, arg2, s1, solveSet)
          } yield s2
        case (TyConst(_) | TyMeta(_) | TyVar(_), _) =>
          if (left == right) {
            // can't recurse further into left
            Some(state)
          }
          else None
        case (TyApply(_, _), _) => None
      }

    val Type.ForAll(bs, in) = gen.forAllType
    val solveSet: Set[Var] = bs.toList.iterator.map(_._1).toSet

    val result =
      solve(in, instTpe, Map.empty, solveSet)
        .map { subs =>
          val freeVars = solveSet -- subs.keySet
          val subBody = substituteTypeVar(gen.in, subs)
          val freeTypeVars = gen.typeVars.filter { case (t, _) => freeVars(t) }
          NonEmptyList.fromList(freeTypeVars) match {
            case None => subBody
            case Some(frees) =>
              val newGen = Generic(frees, subBody)
              pushGeneric(newGen) match {
                case badOpt @ (None | Some(Generic(_, _)))=>
                  // just wrap
                  Annotation(badOpt.getOrElse(newGen), instTpe)
                case Some(notGen) => notGen
              }
          }
        }

    result match {
      case None =>
        // TODO some of these just don't look fully unified yet, for instance:
        // could not solve instantiate:
        // 
        // forall b: *. Bosatsu/Predef::Order[b] -> forall a: *. Bosatsu/Predef::Dict[b, a]
        // 
        // to
        // 
        // Bosatsu/Predef::Order[?338] -> Bosatsu/Predef::Dict[$k$303, $v$304]
        // but those two types aren't the same. It seems like we have to later
        // learn that ?338 == $k$303, but we don't seem to know that yet

        // just add an annotation:
        Annotation(gen, instTpe)
      case Some(res) => res
    }
  }

  private def allPatternTypes[N](p: Pattern[N, Type]): SortedSet[Type] =
    p.traverseType { t => Writer[SortedSet[Type], Type](SortedSet(t), t) }.run._1

  private def pushGeneric[A](g: Generic[A]): Option[TypedExpr[A]] =
    g.in match {
      case AnnotatedLambda(args, body, a) =>
        val argFree = Type.freeBoundTyVars(args.toList.map(_._2)).toSet
        val (outer, inner) = g.typeVars.toList.partition { case (b, _) => argFree(b) }
        NonEmptyList.fromList(inner).map { inner =>
          val gbody = Generic(inner, body)
          val pushedBody = pushGeneric(gbody).getOrElse(gbody)
          val lam = AnnotatedLambda(args, gbody, a)
          NonEmptyList.fromList(outer) match {
            case None => lam
            case Some(outer) => forAll(outer, lam)
          }
        }
      // we can do the same thing on Match
      case Match(arg, branches, tag) =>
        val preTypes = arg.allTypes | branches.foldLeft(arg.allTypes) { case (ts, (p, _)) => ts | allPatternTypes(p) }    
        val argFree = Type.freeBoundTyVars(preTypes.toList).toSet
        if (g.typeVars.exists { case (b, _) => argFree(b) }) {
          None
        }
        else {
          // the only the branches have generics
          val b1 = branches.map { case (p, b) =>
            val gb = Generic(g.typeVars, b)  
            val gb1 = pushGeneric(gb).getOrElse(gb)
            (p, gb1)
          }
          Some(Match(arg, b1, tag))
        }
      case Let(b, v, in, rec, tag) =>
        val argFree = Type.freeBoundTyVars(v.getType :: Nil).toSet
        if (g.typeVars.exists { case (b, _) => argFree(b) }) {
          None
        }
        else {
          val gin = Generic(g.typeVars, in)
          val gin1 = pushGeneric(gin).getOrElse(gin)
          Some(Let(b, v, gin1, rec, tag))
        }
      case _ => None
    }

  // This can assume that the coercion is safe, since it will
  // only matter when type-checking succeeds. It does not need to
  // type-check again
  def coerceRho(tpe: Type.Rho, kinds: Type => Option[Kind]): Coerce =
    tpe match {
      case Type.Fun(args, b: Type.Rho) =>
        val cb = coerceRho(b, kinds)
        val cas = args.map {
          case aRho: Type.Rho => Some(coerceRho(aRho, kinds))
          case _ => None
        }

        coerceFn1(args, b, cas, cb, kinds)
      case _ =>
        new FunctionK[TypedExpr, TypedExpr] { self =>
          def apply[A](expr: TypedExpr[A]) =
            expr match {
              case _ if expr.getType.sameAs(tpe) => expr
              case Annotation(t, _) => self(t)
              case Local(_, _, _) | Global(_, _, _, _) | AnnotatedLambda(_, _, _)| Literal(_, _, _) =>
                // All of these are widened. The lambda seems like we should be able to do
                // better, but the type isn't a Fun(Type, Type.Rho)... this is probably unreachable for
                // the AnnotatedLambda
                Annotation(expr, tpe)
              case gen@Generic(_, _) =>
                pushGeneric(gen) match {
                  case Some(e1) => self(e1)
                  case None =>
                    instantiateTo(gen, tpe, kinds)
                }
              case App(fn, aargs, _, tag) =>
                fn match {
                  case AnnotatedLambda(lamArgs, body, _) =>
                    //(\xs - res)(ys) == let x1 = y1 in let x2 = y2 in ... res
                    val binds = lamArgs.zip(aargs).map {
                      case ((n, rho: Type.Rho), arg) =>
                        (n, coerceRho(rho, kinds)(arg))
                      case ((n, _), arg) => (n, arg)
                    }
                    letAllNonRec(binds, self(body), tag)
                  case _ =>
                    fn.getType match {
                      case Type.Fun(argTs, _) =>
                        val cArgs = aargs.zip(argTs).map {
                          case (arg, rho: Type.Rho) =>
                            val carg = coerceRho(rho, kinds)
                            (carg(arg), rho, Some(carg))
                          case (arg, nonRho) =>
                            (arg, nonRho, None)
                        }
                        val fn1 = coerceFn1(cArgs.map(_._2), tpe, cArgs.map(_._3), self, kinds)(fn)
                        App(fn1, cArgs.map(_._1), tpe, tag)
                      case _ =>
                        // TODO, what should we do here?
                        // It is currently certainly wrong
                        // we have learned that the type is tpe
                        // but that implies something for fn and arg
                        // but we are ignoring that, which
                        // leaves them with potentially skolems or metavars
                        Annotation(expr, tpe)
                    }
                }
              case Let(arg, argE, in, rec, tag) =>
                Let(arg, argE, self(in), rec, tag)
              case Match(arg, branches, tag) =>
                // TODO: this may be wrong. e.g. we could leaving meta in the types
                // embedded in patterns, this does not seem to happen since we would
                // error if metas escape typechecking
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
        case Annotation(t, tpe) =>
          loop(t).map(Annotation(_, tpe))
        case AnnotatedLambda(args, res, tag) =>
          if (args.exists { case (n, _) => masks(n) }) None
          else if (args.exists { case (n, _) => shadows(n) }) Some(in)
          else loop(res).map(AnnotatedLambda(args, _, tag))
        case App(fn, args, tpe, tag) =>
          (loop(fn), args.traverse(loop(_))).mapN(App(_, _, tpe, tag))
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
        val paramSet: Set[Type.Var] = params.toList.iterator.map(_._1).toSet
        val env1 = env.iterator.filter { case (k, _) => !paramSet(k) }.toMap
        Generic(params, substituteTypeVar(expr, env1))
      case Annotation(of, tpe) =>
        Annotation(
          substituteTypeVar(of, env),
          Type.substituteVar(tpe, env))
      case AnnotatedLambda(args, res, tag) =>
        AnnotatedLambda(
          args.map { case (n, tpe) => 
            (n, Type.substituteVar(tpe, env))
          },
          substituteTypeVar(res, env),
          tag)
      case Local(v, tpe, tag) =>
        Local(v, Type.substituteVar(tpe, env), tag)
      case Global(p, v, tpe, tag) =>
        Global(p, v, Type.substituteVar(tpe, env), tag)
      case App(f, args, tpe, tag) =>
        App(
          substituteTypeVar(f, env),
          args.map(substituteTypeVar(_, env)),
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
      case Annotation(term, tpe) => Annotation(recur(term), tpe)
      case AnnotatedLambda(args, expr, tag) =>
        // this is a kind of let:
        if (args.exists(_._1 == name)) {
          // we are shadowing, so we are done:
          te
        }
        else {
          // no shadow
          AnnotatedLambda(args, recur(expr), tag)
        }
      case Local(nm, _, tag) if nm == name => Local(name, tpe, tag)
      case n: Name[A] => n
      case App(fnT, args, tpe, tag) =>
        App(recur(fnT), args.map(recur), tpe, tag)
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
  def coerceFn(args: NonEmptyList[Type], result: Type.Rho, coarg: NonEmptyList[Coerce], cores: Coerce, kinds: Type => Option[Kind]): Coerce =
    coerceFn1(args, result, coarg.map(Some(_)), cores, kinds)

  private def coerceFn1(arg: NonEmptyList[Type], result: Type.Rho, coargOpt: NonEmptyList[Option[Coerce]], cores: Coerce, kinds: Type => Option[Kind]): Coerce =
    new FunctionK[TypedExpr, TypedExpr] { self =>
      val fntpe = Type.Fun(arg, result)

      def apply[A](expr: TypedExpr[A]) = {
        expr match {
          case _ if expr.getType.sameAs(fntpe) => expr
          case Annotation(t, _) => self(t)
          case AnnotatedLambda(args0, res, tag) =>
            // note, Var(None, name, originalType, tag)
            // is hanging out in res, or it is unused
            val args1 = args0.zip(arg).map {
              case ((n, _), t) => (n, t)
            }
            val res1 = args1
              .toList
              .foldRight(res) {
                case ((name, arg), res) =>
                  replaceVarType(res, name, arg)
              }
            AnnotatedLambda(args1, cores(res1), tag)
          case gen@Generic(_, _) =>
            pushGeneric(gen) match {
              case Some(e1) => self(e1)
              case None =>
                instantiateTo(gen, fntpe, kinds)
              }
          case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
            Annotation(expr, fntpe)
          case Let(arg, argE, in, rec, tag) =>
            Let(arg, argE, self(in), rec, tag)
          case Match(arg, branches, tag) =>
            // TODO: this may be wrong. e.g. we could leaving meta in the types
            // embedded in patterns, this does not seem to happen since we would
            // error if metas escape typechecking
            Match(arg, branches.map { case (p, expr) => (p, self(expr)) }, tag)
          case App(AnnotatedLambda(lamArgs, body, _), aArgs, _, tag) =>
            //(\x - res)(y) == let x = y in res
            val arg1 = lamArgs.zip(aArgs).map {
              case ((n, rho: Type.Rho), arg) => (n, coerceRho(rho, kinds)(arg))
              case ((n, _), arg) => (n, arg)
            }
            letAllNonRec(arg1, self(body), tag)
          case App(_, _, _, _) =>
            /*
            * We have to be careful not to collide with the free vars in expr
            * TODO: it is unclear why we are doing this... it may have just been
            * a cute trick in the original rankn types paper, but I'm not
            * sure what is buying us.
            */
            val free = freeVarsSet(expr :: Nil)
            val nameGen = Type.allBinders.iterator.map { v => Identifier.Name(v.name) }.filterNot(free)
            val lamArgs = arg.map { t => (nameGen.next(), t) }
            val aArgs = lamArgs.map { case (n, t) => Local(n, t, expr.tag) }
            // name -> (expr((name: arg)): result)
            val result1 = cores(App(expr, aArgs, result, expr.tag))
            AnnotatedLambda(lamArgs, result1, expr.tag)
        }
      }
    }

  def forAll[A](params: NonEmptyList[(Type.Var.Bound, Kind)], expr: TypedExpr[A]): TypedExpr[A] =
    expr match {
      case Generic(ps, ex0) =>
        // if params and ps have duplicates, that
        // implies that those params weren't free, because
        // they have already been quantified by ps, so
        // they can be removed
        val innerSet = ps.toList.iterator.map(_._1).toSet
        val newParams = params.toList.filterNot { case (v, _) => innerSet(v) }
        val ps1 = NonEmptyList.fromList(newParams) match {
          case None => ps
          case Some(nep) => nep ::: ps
        }
        forAll(ps1, ex0)
      case expr =>
        val g = Generic(params, expr)
        expr match {
          // we not uncommonly add an annotation just to make a generic wrapper to get back where
          // we were
          case Annotation(term, _) if g.getType.sameAs(term.getType) => term
          case _ => g
        }
    }

  private def lambda[A](args: NonEmptyList[(Bindable, Type)], expr: TypedExpr[A], tag: A): TypedExpr[A] =
    expr match {
      // TODO: this branch is never exercised. There is probably some reason for that
      // that the types/invariants are losing
      case Generic(ps, ex0) =>
        // due to covariance in the return type, we can always lift
        // generics on the return value out of the lambda
        val frees = Type.freeBoundTyVars(args.toList.map(_._2)).toSet
        val quants = ps.toList.iterator.map(_._1).toSet
        val collisions = frees.intersect(quants)
        if (collisions.isEmpty) {
          Generic(ps, AnnotatedLambda(args, ex0, tag))
        }
        else {
            // don't replace with any existing type variable or any of the free variables
          val replacements = Type.allBinders.iterator.filterNot(quants | frees)
          val repMap: Map[Type.Var.Bound, Type.Var.Bound] =
            collisions
              .iterator
              .zip(replacements)
              .toMap

          val ps1 = ps.map { case (v, k) => (repMap.getOrElse(v, v), k) }
          val typeMap: Map[Type.Var, Type] =
            repMap.iterator.map { case (k, v) => (k, Type.TyVar(v)) }.toMap
          val ex1 = substituteTypeVar(ex0, typeMap)
          Generic(ps1,
            AnnotatedLambda(args, ex1, tag))
        }
      case notGen =>
        AnnotatedLambda(args, notGen, tag)
    }

  implicit def typedExprHasRegion[T: HasRegion]: HasRegion[TypedExpr[T]] =
    HasRegion.instance[TypedExpr[T]] { e => HasRegion.region(e.tag) }
}
