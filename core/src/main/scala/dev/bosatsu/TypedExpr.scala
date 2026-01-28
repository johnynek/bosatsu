package dev.bosatsu

import cats.{Applicative, Eq, Eval, Monad, Traverse, Order}
import cats.arrow.FunctionK
import cats.data.{NonEmptyList, Writer, State}
import cats.implicits._
import dev.bosatsu.rankn.Type
import org.typelevel.paiges.{Doc, Document}
import scala.collection.immutable.SortedSet
import scala.util.hashing.MurmurHash3

import Identifier.{Bindable, Constructor}
import dev.bosatsu.rankn.Type.Var.Skolem

sealed abstract class TypedExpr[+T] { self: Product =>
  import TypedExpr._

  // It is really important to cache the hashcode and these large dags if
  // we use them as hash keys
  final override val hashCode: Int =
    MurmurHash3.caseClassHash(this)

  def tag: T

  /** For any well typed expression, i.e. one that has already gone through type
    * inference, we should be able to get a type for each expression
    */
  lazy val getType: Type =
    this match {
      case g @ Generic(_, _)  => g.quantType
      case Annotation(_, tpe) =>
        tpe
      case AnnotatedLambda(args, res, _) =>
        Type.Fun(args.map(_._2), res.getType)
      case Local(_, tpe, _)     => tpe
      case Global(_, _, tpe, _) => tpe
      case App(_, _, tpe, _)    => tpe
      case Let(_, _, in, _, _)  =>
        in.getType
      case Literal(_, tpe, _) =>
        tpe
      case Match(_, branches, _) =>
        // all branches have the same type:
        branches.head._2.getType
    }

  lazy val size: Int =
    this match {
      case Generic(_, g)              => g.size
      case Annotation(a, _)           => a.size
      case AnnotatedLambda(_, res, _) =>
        res.size
      case Local(_, _, _) | Literal(_, _, _) | Global(_, _, _, _) => 1
      case App(fn, args, _, _)   => fn.size + args.foldMap(_.size)
      case Let(_, e, in, _, _)   => e.size + in.size
      case Match(a, branches, _) =>
        a.size + branches.foldMap(_._2.size)
    }

  // TODO: we need to make sure this parsable and maybe have a mode that has the compiler
  // emit these
  def repr: Doc = {
    def rept(t: Type): Doc = Type.fullyResolvedDocument.document(t)

    def block(d: Doc): Doc = d.nested(4).grouped

    def loop(te: TypedExpr[T]): Doc =
      te match {
        case g @ Generic(_, expr) =>
          block(
            Doc.text("(generic") + Doc.line + rept(
              g.quantType
            ) + Doc.line + loop(expr) + Doc.char(')')
          )
        case Annotation(expr, tpe) =>
          block(
            Doc.text("(ann") + Doc.line + rept(
              tpe
            ) + Doc.line + loop(expr) + Doc.char(')')
          )
        case AnnotatedLambda(args, res, _) =>
          block(
            Doc.text("(lambda") + Doc.line + (
              Doc.char('[') + block(
                Doc.intercalate(
                  Doc.line,
                  args.toList.map { case (arg, tpe) =>
                    Doc.text(arg.sourceCodeRepr) + Doc.line + rept(tpe)
                  }
                )
              ) + Doc.char(']')
            ) + Doc.line + loop(res) + Doc.char(')')
          )
        case Local(v, tpe, _) =>
          block(
            Doc.text("(var") + Doc.line + Doc.text(
              v.sourceCodeRepr
            ) + Doc.line + rept(tpe) + Doc.char(')')
          )
        case Global(p, v, tpe, _) =>
          val pstr = Doc.text(p.asString + "::" + v.sourceCodeRepr)
          block(
            Doc.text("(var") + Doc.line + pstr + Doc.line + rept(
              tpe
            ) + Doc.char(')')
          )
        case App(fn, args, tpe, _) =>
          val argsDoc = block(Doc.intercalate(Doc.line, args.toList.map(loop)))
          block(
            Doc.text("(ap") + Doc.line + loop(
              fn
            ) + Doc.line + argsDoc + Doc.line + rept(tpe) + Doc
              .char(')')
          )
        case Let(n, b, in, rec, _) =>
          val nm =
            if (rec.isRecursive) Doc.text("(letrec") else Doc.text("(let")
          block(
            nm + Doc.line + Doc.text(
              n.sourceCodeRepr
            ) + Doc.line + loop(b) + Doc.line + loop(in) + Doc.char(
              ')'
            )
          )
        case Literal(v, tpe, _) =>
          block(
            Doc.text("(lit") + Doc.line + Doc.text(
              v.repr
            ) + Doc.line + rept(tpe) + Doc.char(')')
          )
        case Match(arg, branches, _) =>
          implicit val docType: Document[Type] =
            Document.instance(tpe => rept(tpe))
          val cpat = Pattern.compiledDocument[Type]
          def pat(p: Pattern[(PackageName, Constructor), Type]): Doc =
            cpat.document(p)

          val bstr = branches.toList.map { case (p, t) =>
            block(
              Doc.char('[') + pat(p) + Doc.comma + Doc.line + loop(t) + Doc
                .char(']')
            )
          }
          block(
            Doc.text("(match") + Doc.line + loop(arg) + block(
              Doc.hardLine +
                Doc.intercalate(Doc.hardLine, bstr)
            ) + Doc.char(')')
          )
      }

    loop(this)
  }

  def reprString: String = repr.render(80)

  /** All the free variables in this expression in order encountered and with
    * duplicates (to see how often they appear)
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
          } else argFree0

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
        val branchFreeMax = branchFrees.zipWithIndex
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
  implicit def eqTypedExpr[A](implicit eqA: Eq[A]): Eq[TypedExpr[A]] =
    new Eq[TypedExpr[A]] {
      private def eqType(left: Type, right: Type): Boolean =
        Order[Type].eqv(left, right)

      private def eqQuant(
          left: Type.Quantification,
          right: Type.Quantification
      ): Boolean =
        Order[Type.Quantification].eqv(left, right)

      private def eqBindable(left: Bindable, right: Bindable): Boolean =
        Order[Bindable].eqv(left, right)

      private def eqIdentifier(left: Identifier, right: Identifier): Boolean =
        Order[Identifier].eqv(left, right)

      private def eqPackageName(
          left: PackageName,
          right: PackageName
      ): Boolean =
        Order[PackageName].eqv(left, right)

      private def eqPattern(
          left: Pattern[(PackageName, Constructor), Type],
          right: Pattern[(PackageName, Constructor), Type]
      ): Boolean =
        Order[Pattern[(PackageName, Constructor), Type]].eqv(left, right)

      private def eqNel[B](
          left: NonEmptyList[B],
          right: NonEmptyList[B]
      )(eqB: (B, B) => Boolean): Boolean = {
        val leftIt = left.iterator
        val rightIt = right.iterator
        var same = true
        while (same && leftIt.hasNext && rightIt.hasNext) {
          same = eqB(leftIt.next(), rightIt.next())
        }
        same && !leftIt.hasNext && !rightIt.hasNext
      }

      private def eqArgList(
          left: NonEmptyList[(Bindable, Type)],
          right: NonEmptyList[(Bindable, Type)]
      ): Boolean =
        eqNel(left, right) { case ((lb, lt), (rb, rt)) =>
          eqBindable(lb, rb) && eqType(lt, rt)
        }

      private def eqExprs(
          left: NonEmptyList[TypedExpr[A]],
          right: NonEmptyList[TypedExpr[A]]
      ): Boolean =
        eqNel(left, right)(loop)

      private def eqBranches(
          left: NonEmptyList[
            (Pattern[(PackageName, Constructor), Type], TypedExpr[A])
          ],
          right: NonEmptyList[
            (Pattern[(PackageName, Constructor), Type], TypedExpr[A])
          ]
      ): Boolean =
        eqNel(left, right) { case ((lp, le), (rp, re)) =>
          eqPattern(lp, rp) && loop(le, re)
        }

      private def loop(left: TypedExpr[A], right: TypedExpr[A]): Boolean =
        (left, right) match {
          case (Generic(lq, li), Generic(rq, ri)) =>
            eqQuant(lq, rq) && loop(li, ri)
          case (Annotation(lt, lc), Annotation(rt, rc)) =>
            eqType(lc, rc) && loop(lt, rt)
          case (
                AnnotatedLambda(largs, lexpr, ltag),
                AnnotatedLambda(rargs, rexpr, rtag)
              ) =>
            eqArgList(largs, rargs) &&
              loop(lexpr, rexpr) &&
              eqA.eqv(ltag, rtag)
          case (Local(ln, lt, ltag), Local(rn, rt, rtag)) =>
            eqBindable(ln, rn) && eqType(lt, rt) && eqA.eqv(ltag, rtag)
          case (Global(lp, ln, lt, ltag), Global(rp, rn, rt, rtag)) =>
            eqPackageName(lp, rp) &&
              eqIdentifier(ln, rn) &&
              eqType(lt, rt) &&
              eqA.eqv(ltag, rtag)
          case (App(lf, largs, lres, ltag), App(rf, rargs, rres, rtag)) =>
            loop(lf, rf) &&
              eqExprs(largs, rargs) &&
              eqType(lres, rres) &&
              eqA.eqv(ltag, rtag)
          case (Let(ln, le, lin, lrec, ltag), Let(rn, re, rin, rrec, rtag)) =>
            eqBindable(ln, rn) &&
              loop(le, re) &&
              loop(lin, rin) &&
              Eq[RecursionKind].eqv(lrec, rrec) &&
              eqA.eqv(ltag, rtag)
          case (Literal(llit, lt, ltag), Literal(rlit, rt, rtag)) =>
            Eq[Lit].eqv(llit, rlit) &&
              eqType(lt, rt) &&
              eqA.eqv(ltag, rtag)
          case (Match(larg, lbranches, ltag), Match(rarg, rbranches, rtag)) =>
            loop(larg, rarg) &&
              eqBranches(lbranches, rbranches) &&
              eqA.eqv(ltag, rtag)
          case _ => false
        }

      override def eqv(left: TypedExpr[A], right: TypedExpr[A]): Boolean =
        loop(left, right)
    }

  type Rho[A] =
    TypedExpr[A] // an expression with a Rho type (no top level forall)

  sealed abstract class Name[+A] extends TypedExpr[A] with Product

  /** This says that the resulting term is generic on a given param
    *
    * The paper says to add TyLam and TyApp nodes, but it never mentions what to
    * do with them
    */
  case class Generic[T](quant: Type.Quantification, in: TypedExpr[T])
      extends TypedExpr[T] {
    lazy val quantType: Type.Quantified =
      Type.quantify(quant, in.getType)
    def tag: T = in.tag
  }
  // Annotation really means "widen", the term has a type that is a subtype of coerce, so we are widening
  // to the given type. This happens on Locals/Globals also in their tpe
  case class Annotation[T](term: TypedExpr[T], coerce: Type)
      extends TypedExpr[T] {
    def tag: T = term.tag
  }
  case class AnnotatedLambda[+T](
      args: NonEmptyList[(Bindable, Type)],
      expr: TypedExpr[T],
      tag: T
  ) extends TypedExpr[T] {

    // This makes sure the args don't shadow any of the items in freeSet
    def unshadow(freeSet: Set[Bindable]): AnnotatedLambda[T] = {
      val clashIdent =
        if (freeSet.isEmpty) Set.empty[Bindable]
        else
          args.iterator.flatMap {
            case (n, _) if freeSet(n) => n :: Nil
            case _                    => Nil
          }.toSet

      if (clashIdent.isEmpty) this
      else {
        // we have to allocate new variables
        type I = Bindable
        def inc(n: I, idx: Int): I =
          n match {
            case Identifier.Name(n) => Identifier.Name(n + idx.toString)
            case _                  => Identifier.Name("a" + idx.toString)
          }

        def alloc(
            head: (I, Type),
            tail: List[(I, Type)],
            avoid: Set[I]
        ): NonEmptyList[(I, Type)] = {
          val (ident, tpe) = head
          val ident1 =
            if (clashIdent(ident)) {
              // the following iterator is infinite and distinct, and the avoid
              // set is finite, so the get here must terminate in at most avoid.size
              // steps
              Iterator
                .from(0)
                .map(i => inc(ident, i))
                .collectFirst { case n if !avoid(n) => n }
                .get

            } else ident

          tail match {
            case Nil    => NonEmptyList.one((ident1, tpe))
            case h :: t =>
              (ident1, tpe) :: alloc(h, t, avoid + ident1)
          }
        }

        val avoids = freeSet | freeVarsSet(expr :: Nil)
        val newArgs = alloc(args.head, args.tail, avoids)
        val resSub = args.iterator
          .map(_._1)
          .zip(newArgs.iterator.map { case (n1, _) =>

          { (loc: Local[T]) => Local(n1, loc.tpe, loc.tag) }
          })
          .toMap

        // calling .get is safe when enterLambda = true
        val expr1 = substituteAll(resSub, expr, enterLambda = true).get

        AnnotatedLambda(newArgs, expr1, tag)
      }
    }
  }

  case class Local[+T](name: Bindable, tpe: Type, tag: T) extends Name[T]
  case class Global[T](pack: PackageName, name: Identifier, tpe: Type, tag: T)
      extends Name[T]
  case class App[T](
      fn: TypedExpr[T],
      args: NonEmptyList[TypedExpr[T]],
      result: Type,
      tag: T
  ) extends TypedExpr[T]
  case class Let[T](
      arg: Bindable,
      expr: TypedExpr[T],
      in: TypedExpr[T],
      recursive: RecursionKind,
      tag: T
  ) extends TypedExpr[T] {
    def unshadowResult(freeSet: Set[Bindable]): Let[T] = {
      val clashIdent =
        if (freeSet(arg)) Set(arg)
        else Set.empty

      if (clashIdent.isEmpty) this
      else {
        // we have to allocate new let
        val avoids = freeSet | freeVarsSet(in :: expr :: Nil)
        type I = Bindable
        def inc(n: I, idx: Int): I =
          n match {
            case Identifier.Name(n) => Identifier.Name(n + idx.toString)
            case _                  => Identifier.Name("a" + idx.toString)
          }

        val arg1 = Iterator
          .from(0)
          .map(i => inc(arg, i))
          .collectFirst { case n if !avoids(n) => n }
          .get

        val resSub = Map(arg -> { (loc: Local[T]) =>
          Local(arg1, loc.tpe, loc.tag)
        })

        // calling .get is safe when enterLambda = true
        val in1 = substituteAll(resSub, in, enterLambda = true).get

        copy(arg = arg1, in = in1)
      }
    }

    def unshadowBoth(freeSet: Set[Bindable]): Let[T] = {
      val clashIdent =
        if (freeSet(arg)) Set(arg)
        else Set.empty

      if (clashIdent.isEmpty) this
      else {
        // we have to allocate new let
        val avoids = freeSet | freeVarsSet(in :: expr :: Nil)
        type I = Bindable
        def inc(n: I, idx: Int): I =
          n match {
            case Identifier.Name(n) => Identifier.Name(n + idx.toString)
            case _                  => Identifier.Name("a" + idx.toString)
          }

        val arg1 = Iterator
          .from(0)
          .map(i => inc(arg, i))
          .collectFirst { case n if !avoids(n) => n }
          .get

        val resSub = Map(arg -> { (loc: Local[T]) =>
          Local(arg1, loc.tpe, loc.tag)
        })

        // calling .get is safe when enterLambda = true
        val expr1 = substituteAll(resSub, expr, enterLambda = true).get
        val in1 = substituteAll(resSub, in, enterLambda = true).get

        copy(arg = arg1, expr = expr1, in = in1)
      }
    }
  }
  // TODO, this shouldn't have a type, we know the type from Lit currently
  case class Literal[T](lit: Lit, tpe: Type, tag: T) extends TypedExpr[T]
  case class Match[T](
      arg: TypedExpr[T],
      branches: NonEmptyList[
        (Pattern[(PackageName, Constructor), Type], TypedExpr[T])
      ],
      tag: T
  ) extends TypedExpr[T]

  def letAllNonRec[T](
      binds: NonEmptyList[(Bindable, TypedExpr[T])],
      in: TypedExpr[T],
      tag: T
  ): Let[T] = {
    val in1 = binds.tail match {
      case Nil      => in
      case h1 :: t1 => letAllNonRec(NonEmptyList(h1, t1), in, tag)
    }
    val (n, ne) = binds.head
    Let(n, ne, in1, RecursionKind.NonRecursive, tag)
  }

  /** If we expect expr to be a lambda of the given arity, return the parameter
    * names and types and the rest of the body
    */
  def toArgsBody[A](
      arity: Int,
      expr: TypedExpr[A]
  ): Option[(NonEmptyList[(Bindable, Type)], TypedExpr[A])] =
    expr match {
      case Generic(_, e)                  => toArgsBody(arity, e)
      case Annotation(e, _)               => toArgsBody(arity, e)
      case AnnotatedLambda(args, expr, _) =>
        if (args.length == arity) {
          Some((args, expr))
        } else {
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
          } else {
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
            } else {
              Some((n, (p, b1)))
            }
          }
        }

        argSetO.flatMap { argSet =>
          if (argSet.map(_._1).toList.toSet.size == 1) {
            Some((argSet.head._1, Match(arg, argSet.map(_._2), tag)))
          } else {
            None
          }
        }
      case _ => None
    }

  private val emptyBound: SortedSet[Type.Var.Bound] =
    SortedSet.empty

  implicit class InvariantTypedExpr[A](val self: TypedExpr[A]) extends AnyVal {
    def allTypes: SortedSet[Type] =
      traverseType { t =>
        Writer[SortedSet[Type], Type](SortedSet(t), t)
      }.run._1

    def allBound: SortedSet[Type.Var.Bound] =
      traverseType {
        case t @ Type.TyVar(b: Type.Var.Bound) =>
          Writer[SortedSet[Type.Var.Bound], Type](SortedSet(b), t)
        case t =>
          Writer[SortedSet[Type.Var.Bound], Type](emptyBound, t)
      }.run._1

    def freeTyVars: List[Type.Var] = {
      def loop(self: TypedExpr[A]): Set[Type.Var] =
        self match {
          case Generic(quant, expr) =>
            loop(expr) -- quant.vars.iterator.map(_._1)
          case Annotation(of, tpe) =>
            loop(of) ++ Type.freeTyVars(tpe :: Nil)
          case AnnotatedLambda(args, res, _) =>
            loop(res) ++ Type.freeTyVars(args.toList.map { case (_, t) => t })
          case Local(_, tpe, _) =>
            Type.freeTyVars(tpe :: Nil).toSet
          case Global(_, _, tpe, _) =>
            // this shouldn't happen but does in generated tests
            Type.freeTyVars(tpe :: Nil).toSet
          case App(f, args, tpe, _) =>
            args.foldLeft(loop(f))(_ | loop(_)) ++
              Type.freeTyVars(tpe :: Nil)
          case Let(_, exp, in, _, _) =>
            loop(exp) | loop(in)
          case Literal(_, tpe, _) =>
            // this shouldn't happen but does in generated tests
            Type.freeTyVars(tpe :: Nil).toSet
          case Match(expr, branches, _) =>
            // all branches have the same type:
            branches.foldLeft(loop(expr)) { case (acc, (p, t)) =>
              (acc | loop(t)) ++ allPatternTypes(p).iterator.collect {
                case Type.TyVar(v) => v
              }
            }
        }

      loop(self).toList.sorted
    }

    /** Traverse all the *non-shadowed* types inside the TypedExpr
      */
    def traverseType[F[_]: Applicative](fn: Type => F[Type]): F[TypedExpr[A]] =
      self match {
        case gen @ Generic(quant, expr) =>
          // params shadow below, so they are not free values
          // and can easily create bugs if passed into fn
          val params = quant.vars
          val shadowed: Set[Type.Var.Bound] =
            params.toList.iterator.map(_._1).toSet
          val shadowFn: Type => F[Type] = {
            case tvar @ Type.TyVar(v: Type.Var.Bound) if shadowed(v) =>
              Applicative[F].pure(tvar)
            case notShadowed => fn(notShadowed)
          }

          val paramsF = params.traverse_(v => fn(Type.TyVar(v._1)))
          (paramsF *> fn(gen.getType) *> expr.traverseType(shadowFn))
            .map(Generic(quant, _))
        case Annotation(of, tpe) =>
          (of.traverseType(fn), fn(tpe)).mapN(Annotation(_, _))
        case lam @ AnnotatedLambda(args, res, tag) =>
          val a1 = args.traverse { case (n, t) => fn(t).map(n -> _) }
          fn(lam.getType) *> (a1, res.traverseType(fn)).mapN {
            AnnotatedLambda(_, _, tag)
          }
        case Local(v, tpe, tag) =>
          fn(tpe).map(Local(v, _, tag))
        case Global(p, v, tpe, tag) =>
          fn(tpe).map(Global(p, v, _, tag))
        case App(f, args, tpe, tag) =>
          (f.traverseType(fn), args.traverse(_.traverseType(fn)), fn(tpe))
            .mapN {
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
          val tbranch = branches.traverse { case (p, t) =>
            p.traverseType(fn).product(t.traverseType(fn))
          }
          (expr.traverseType(fn), tbranch).mapN(Match(_, _, tag))
      }

    /** This applies fn on all the contained types, replaces the elements, then
      * calls on the resulting. This is "bottom up"
      */
    def traverseUp[F[_]: Monad](
        fn: TypedExpr[A] => F[TypedExpr[A]]
    ): F[TypedExpr[A]] = {
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
        case v @ (Global(_, _, _, _) | Local(_, _, _) | Literal(_, _, _)) =>
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
          val tbranch = branches.traverse { case (p, t) =>
            loop(t).map((p, _))
          }
          (loop(expr), tbranch)
            .mapN(Match(_, _, tag))
            .flatMap(fn)
      }
    }

    /** Here are all the global names inside this expression
      */
    def globals: Set[(PackageName, Identifier)] =
      {
        type GlobalsWriter[A] = Writer[Set[(PackageName, Identifier)], A]
        traverseUp[GlobalsWriter] {
        case g @ Global(p, i, _, _) =>
          Writer.tell(Set[(PackageName, Identifier)]((p, i))).as(g)
        case notG => Monad[GlobalsWriter].pure(notG)
        }.written
      }
  }

  def zonkMeta[F[_]: Applicative, A](te: TypedExpr[A])(
      fn: Type.Meta => F[Option[Type.Rho]]
  ): F[TypedExpr[A]] =
    te.traverseType(Type.zonkMeta[F](_)(fn))

  /** quantify every meta variable that is not escaped into the outer
    * environment.
    *
    * TODO: This can probably be optimized. I think it is currently quadradic in
    * depth of the TypedExpr
    */
  def quantify[F[_]: Monad, A](
      env: Map[(Option[PackageName], Identifier), Type],
      rho: TypedExpr.Rho[A],
      readFn: Type.Meta => F[Option[Type.Rho]],
      writeFn: (Type.Meta, Type.Rho) => F[Unit]
  ): F[TypedExpr[A]] = {

    val zFn = Type.zonk(SortedSet.empty, readFn, writeFn)
    // we need to zonk before so any known metas are removed
    // some of the meta-variables may point to the same values
    def getMetaTyVars(tpes: List[Type]): F[SortedSet[Type.Meta]] =
      tpes.traverse(Type.zonkMeta[F](_)(zFn)).map { zonked =>
        Type.metaTvs(zonked)
      }

    def quantify0(
        metaList: List[Type.Meta],
        rho: TypedExpr[A]
    ): F[TypedExpr[A]] =
      NonEmptyList.fromList(metaList) match {
        case None        => Applicative[F].pure(rho)
        case Some(metas) =>
          val used: Set[Type.Var.Bound] = rho.allBound
          val aligned = Type.alignBinders(metas, used)
          val bound = aligned.traverse { case (m, n) =>
            writeFn(m, Type.TyVar(n)).as(((n, m.kind), m.existential))
          }
          // we only need to zonk after doing a write:
          // it isnot clear that zonkMeta correctly here because the existentials
          // here have been realized to Type.Var now, and and meta pointing at them should
          // become visible (no longer hidden)
          val zFn = Type.zonk(
            metas.iterator.filter(_.existential).to(SortedSet),
            readFn,
            writeFn
          )
          (bound, zonkMeta[F, A](rho)(zFn))
            .mapN { (typeArgs, r) =>
              val forAlls = typeArgs.collect { case (nk, false) => nk }
              val exists = typeArgs.collect { case (nk, true) => nk }
              quantVars(forallList = forAlls, existList = exists, r)
            }
      }

    def quantifyMetas(
        envList: => List[Type],
        metas: SortedSet[Type.Meta],
        te: TypedExpr[A]
    ): F[TypedExpr[A]] =
      if (metas.isEmpty) Applicative[F].pure(te)
      else {
        for {
          envTypeVars <- getMetaTyVars(envList)
          localMetas = metas.diff(envTypeVars)
          q <- quantify0(localMetas.toList, te)
        } yield q
      }

    def quantifyFree(env: Set[Type], te: TypedExpr[A]): F[TypedExpr[A]] = {
      // this is lazy because we only evaluate it if there is an existential skolem
      lazy val envList = env.toList
      lazy val envExistSkols = Type
        .freeTyVars(envList)
        .iterator
        .collect { case ex @ Skolem(_, _, true, _) =>
          ex
        }
        .toSet[Type.Var.Skolem]

      val tyVars = te.freeTyVars
      val teSkols = tyVars
        .collect {
          case ex @ Skolem(_, _, true, _) if !envExistSkols(ex) => ex
        }

      val te1 = NonEmptyList.fromList(teSkols) match {
        case None      => te
        case Some(nel) =>
          val used: Set[Type.Var.Bound] = tyVars.iterator.collect {
            case b @ Type.Var.Bound(_) => b
          }.toSet

          val names = Type.alignBinders(nel, used)
          val aligned = names.iterator
            .map { case (v, b) =>
              (v, Type.TyVar(b))
            }
            .toMap[Type.Var, Type]

          quantVars(
            Nil,
            names.toList.map { case (sk, b) => (b, sk.kind) },
            substituteTypeVar(te, aligned)
          )
      }

      getMetaTyVars(te1.allTypes.toList)
        .flatMap(quantifyMetas(envList, _, te1))
    }

    /*
     * By only quantifying the outside
     * the inside may still have some metas that don't
     * make it all the way out.
     *
     * This algorithm isn't great. It is quadratic in depth
     * because we have to do work linear in depth at each
     * level.
     */
    def deepQuantify(env: Set[Type], te: TypedExpr[A]): F[TypedExpr[A]] =
      quantifyFree(env, te).flatMap {
        case Generic(quant, in) =>
          deepQuantify(env + te.getType, in).map { in1 =>
            quantVars(quant.forallList, quant.existList, in1)
          }
        case Annotation(term, coerce) =>
          deepQuantify(env + coerce, term).map { t1 =>
            ann(t1, coerce)
          }
        case AnnotatedLambda(args, expr, tag) =>
          val env1 = env ++ args.iterator.map(_._2)
          deepQuantify(env1 + te.getType, expr)
            .map { e1 =>
              lambda(args, e1, tag)
            }
        case Let(arg, expr, in, rec, tag) =>
          // this introduces something into the env
          val inEnv = env + expr.getType
          val exprEnv = if (rec.isRecursive) inEnv else env
          (
            deepQuantify(exprEnv + te.getType, expr),
            deepQuantify(inEnv + te.getType, in)
          )
            .mapN { (e1, i1) =>
              Let(arg, e1, i1, rec, tag)
            }
        case App(fn, args, tpe, tag) =>
          val env1 = env + te.getType
          (deepQuantify(env1, fn), args.traverse(deepQuantify(env1, _)))
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
          val allMatchMetas: F[SortedSet[Type.Meta]] =
            getMetaTyVars(arg.getType :: branches.foldMap { case (p, _) =>
              allPatternTypes(p)
            }.toList)

          val env1 = env + te.getType
          def handleBranch(br: Branch[A]): F[Branch[A]] = {
            val (p, expr) = br
            val branchEnv = env1 ++ Pattern
              .envOf(p, Map.empty)(ident => (None, ident))
              .values
            deepQuantify(branchEnv, expr).map((p, _))
          }

          val noArg = for {
            br1 <- branches.traverse(handleBranch(_))
            ms <- allMatchMetas
            quant <- quantifyMetas(env1.toList, ms, Match(arg, br1, tag))
          } yield quant

          def finish(te: TypedExpr[A]): F[TypedExpr[A]] =
            te match {
              case Match(arg, branches, tag) =>
                // we still need to recurse on arg
                deepQuantify(env1, arg).map(Match(_, branches, tag))
              case Generic(quants, expr) =>
                finish(expr).map(
                  quantVars(quants.forallList, quants.existList, _)
                )
              // $COVERAGE-OFF$
              case unreach =>
                sys.error(
                  s"Match quantification yielded neither Generic nor Match: $unreach"
                )
              // $COVERAGE-ON$
            }

          noArg.flatMap(finish)

        case nonest @ (Global(_, _, _, _) | Local(_, _, _) |
            Literal(_, _, _)) =>
          Applicative[F].pure(nonest)
      }

    deepQuantify(env.values.toSet, rho)
  }

  implicit val traverseTypedExpr: Traverse[TypedExpr] =
    new Traverse[TypedExpr] {
      def traverse[F[_]: Applicative, T, S](
          typedExprT: TypedExpr[T]
      )(fn: T => F[S]): F[TypedExpr[S]] =
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
            val tbranch = branches.traverse { case (p, t) =>
              t.traverse(fn).map((p, _))
            }
            (expr.traverse(fn), tbranch, fn(tag)).mapN(Match(_, _, _))
        }

      def foldLeft[A, B](typedExprA: TypedExpr[A], b: B)(f: (B, A) => B): B =
        typedExprA match {
          case Generic(_, e) =>
            foldLeft(e, b)(f)
          case Annotation(e, _) =>
            foldLeft(e, b)(f)
          case AnnotatedLambda(_, e, tag) =>
            val b1 = foldLeft(e, b)(f)
            f(b1, tag)
          case n: Name[A]            => f(b, n.tag)
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
            val b2 = branches.foldLeft(b1) { case (bn, (_, t)) =>
              foldLeft(t, bn)(f)
            }
            f(b2, tag)
        }

      def foldRight[A, B](typedExprA: TypedExpr[A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]
      ): Eval[B] = typedExprA match {
        case Generic(_, e) =>
          foldRight(e, lb)(f)
        case Annotation(e, _) =>
          foldRight(e, lb)(f)
        case AnnotatedLambda(_, e, tag) =>
          val lb1 = f(tag, lb)
          foldRight(e, lb1)(f)
        case n: Name[A]            => f(n.tag, lb)
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
          val b2 = branches.foldRight(b1) { case ((_, t), bn) =>
            foldRight(t, bn)(f)
          }
          foldRight(arg, b2)(f)
      }

      override def map[A, B](te: TypedExpr[A])(fn: A => B): TypedExpr[B] =
        te match {
          case Generic(tv, in)       => Generic(tv, map(in)(fn))
          case Annotation(term, tpe) => Annotation(map(term)(fn), tpe)
          case AnnotatedLambda(args, expr, tag) =>
            AnnotatedLambda(args, map(expr)(fn), fn(tag))
          case l @ Local(_, _, _)       => l.copy(tag = fn(l.tag))
          case g @ Global(_, _, _, _)   => g.copy(tag = fn(g.tag))
          case App(fnT, args, tpe, tag) =>
            App(map(fnT)(fn), args.map(map(_)(fn)), tpe, fn(tag))
          case Let(b, e, in, r, t) => Let(b, map(e)(fn), map(in)(fn), r, fn(t))
          case lit @ Literal(_, _, _)    => lit.copy(tag = fn(lit.tag))
          case Match(arg, branches, tag) =>
            Match(
              map(arg)(fn),
              branches.map { case (p, t) => (p, map(t)(fn)) },
              fn(tag)
            )
        }
    }

  type Coerce = FunctionK[TypedExpr, TypedExpr]

  private def pushDownCovariant(
      tpe: Type.Quantified,
      kinds: Type => Option[Kind]
  ): Type =
    tpe match {
      case Type.ForAll(targs, in) =>
        val (cons, cargs) = Type.unapplyAll(in)
        kinds(cons) match {
          case None =>
            // this can happen because the cons is some kind of type variable
            // we have lost track of (we need to track the type variables in
            // recursions)
            tpe
          case Some(kind) =>
            val kindArgs = kind.toArgs
            val kindArgsWithArgs =
              kindArgs.zip(cargs).map { case (ka, a) => (Some(ka), a) } :::
                cargs.drop(kindArgs.length).map((None, _))

            val argsVectorIdx = kindArgsWithArgs.iterator.zipWithIndex.map {
              case ((optKA, tpe), idx) =>
                (Type.freeBoundTyVars(tpe :: Nil).toSet, optKA, tpe, idx)
            }.toVector

            // if an arg is covariant, it can pull all it's unique freeVars
            def uniqueFreeVars(idx: Int): Set[Type.Var.Bound] = {
              val (justIdx, optKA, _, _) = argsVectorIdx(idx)
              if (optKA.exists(_.variance == Variance.co)) {
                argsVectorIdx.iterator
                  .filter(_._4 != idx)
                  .foldLeft(justIdx) { case (acc, (s, _, _, _)) => acc -- s }
              } else Set.empty
            }
            val withPulled = argsVectorIdx.map { case rec @ (_, _, _, idx) =>
              (rec, uniqueFreeVars(idx))
            }
            val allPulled: Set[Type.Var.Bound] = withPulled.foldMap(_._2)
            val nonpulled = targs.filterNot { case (v, _) => allPulled(v) }
            val pulledArgs = withPulled.iterator.map {
              case ((_, _, tpe, _), uniques) =>
                val keep: Type.Var.Bound => Boolean = uniques
                Type.forAll(targs.filter { case (t, _) => keep(t) }, tpe)
            }.toList
            Type.forAll(nonpulled, Type.applyAll(cons, pulledArgs))
        }
      case notForAll =>
        // TODO: we can push down existentials too
        notForAll
    }

  // We know initTpe <:< instTpe, we may be able to simply
  // fix some of the universally quantified variables
  def instantiateTo[A](
      gen: Generic[A],
      instTpe: Type.Rho,
      kinds: Type => Option[Kind]
  ): TypedExpr[A] = {
    import Type._

    def solve(
        left: Type,
        right: Type,
        state: Map[Type.Var, Type],
        solveSet: Set[Type.Var],
        varKinds: Map[Type.Var, Kind]
    ): Option[Map[Type.Var, Type]] =
      (left, right) match {
        case (TyVar(v), right) if solveSet(v) =>
          state.get(v) match {
            case None      => Some(state.updated(v, right))
            case Some(tpe) =>
              if (tpe.sameAs(right)) Some(state)
              else None
          }
        case (fa: Type.Quantified, r) =>
          if (fa.sameAs(r)) Some(state)
          // this will mask solving for the inside values:
          else {
            val vlist = fa.vars.toList

            solve(
              fa.in,
              r,
              state,
              solveSet -- vlist.iterator.map(_._1),
              varKinds ++ vlist
            )
          }
        case (_, fa: Type.Quantified) =>
          val kindsWithVars: Type => Option[Kind] = {
            case v: Type.TyVar => varKinds.get(v.toVar)
            case t             => kinds(t)
          }
          val fa1 = pushDownCovariant(fa, kindsWithVars)
          if (fa1 =!= fa) solve(left, fa1, state, solveSet, varKinds)
          else {
            // not clear what to do here,
            // the examples that come up look like un-unified
            // types, as if coerceRho is called before we have
            // finished unifying
            None
          }
        case (TyApply(on, arg), TyApply(on2, arg2)) =>
          for {
            s1 <- solve(on, on2, state, solveSet, varKinds)
            s2 <- solve(arg, arg2, s1, solveSet, varKinds)
          } yield s2
        case (TyConst(_) | TyMeta(_) | TyVar(_), _) =>
          if (left === right) {
            // can't recurse further into left
            Some(state)
          } else None
        case (TyApply(_, _), _) => None
      }

    val (bs, in) = gen.quantType match {
      case Type.ForAll(a, in) => (a.toList, in)
      case notForAll          => (Nil, notForAll)
    }

    val solveSet: Set[Var] = bs.iterator.map(_._1).toSet

    val result =
      solve(in, instTpe, Map.empty, solveSet, bs.toList.toMap)
        .map { subs =>
          val freeVars = solveSet -- subs.keySet
          val subBody = substituteTypeVar(gen.in, subs)
          val freeExists = gen.quantType.existList.filter { case (t, _) =>
            freeVars(t)
          }
          val freeForall = gen.quantType.forallList.filter { case (t, _) =>
            freeVars(t)
          }
          val q = Type.quantify(
            forallList = freeForall,
            existList = freeExists,
            subBody.getType
          )
          q match {
            case _: Type.Rho         => subBody
            case tq: Type.Quantified =>
              val newGen = Generic(tq.quant, subBody)
              pushGeneric(newGen) match {
                case badOpt @ (None | Some(Generic(_, _))) =>
                  // just wrap
                  ann(badOpt.getOrElse(newGen), instTpe)
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
        ann(gen, instTpe)
      case Some(res) => res
    }
  }

  private def allPatternTypes[N](p: Pattern[N, Type]): SortedSet[Type] =
    p.traverseType(t => Writer[SortedSet[Type], Type](SortedSet(t), t)).run._1

  // Invariant, nel must have at least one item in common with quant.vars
  private def filterQuant(
      nel: NonEmptyList[Type.Var],
      quant: Type.Quantification
  ): Type.Quantification = {
    val innerSet = nel.toList.toSet
    Type.Quantification
      .fromLists(
        forallList = quant.forallList.filter { case (v, _) => innerSet(v) },
        existList = quant.existList.filter { case (v, _) => innerSet(v) }
      )
      // this get is safe because at least one var is present
      .get
  }

  private def pushGeneric[A](g: Generic[A]): Option[TypedExpr[A]] =
    g.in match {
      case AnnotatedLambda(args, body, a) =>
        val argFree = Type.freeBoundTyVars(args.toList.map(_._2)).toSet
        val (outer, inner) = g.quantType.vars.toList.partition { case (b, _) =>
          argFree(b)
        }
        NonEmptyList.fromList(inner).map { inner =>
          // we know this has at least one item
          val inners = inner.map(_._1)
          val innerType = filterQuant(inners, g.quant)
          val gbody = Generic(innerType, body)
          val pushedBody = pushGeneric(gbody).getOrElse(gbody)
          val lam = AnnotatedLambda(args, pushedBody, a)
          NonEmptyList.fromList(outer) match {
            case None        => lam
            case Some(outer) => forAll(outer, lam)
          }
        }
      // we can do the same thing on Match
      case Match(arg, branches, tag) =>
        val preTypes = branches.foldLeft(arg.allTypes) { case (ts, (p, _)) =>
          ts | allPatternTypes(p)
        }
        val argFree = Type.freeBoundTyVars(preTypes.toList).toSet
        if (g.quantType.vars.exists { case (b, _) => argFree(b) }) {
          None
        } else {
          // the only the branches have generics
          val b1 = branches.map { case (p, b) =>
            val gb = Generic(g.quant, b)
            val gb1 = pushGeneric(gb).getOrElse(gb)
            (p, gb1)
          }
          Some(Match(arg, b1, tag))
        }
      case Let(b, v, in, rec, tag) =>
        val argFree = Type.freeBoundTyVars(v.getType :: Nil).toSet
        if (g.quantType.vars.exists { case (b, _) => argFree(b) }) {
          None
        } else {
          val gin = Generic(g.quant, in)
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
          case _              => None
        }

        coerceFn1(args, b, cas, cb, kinds)
      case _ =>
        new FunctionK[TypedExpr, TypedExpr] { self =>
          def apply[A](expr: TypedExpr[A]) =
            expr match {
              case _ if expr.getType.sameAs(tpe) => expr
              case Annotation(t, _)              => self(t)
              case Local(_, _, _) | Global(_, _, _, _) |
                  AnnotatedLambda(_, _, _) | Literal(_, _, _) =>
                // All of these are widened. The lambda seems like we should be able to do
                // better, but the type isn't a Fun(Type, Type.Rho)... this is probably unreachable for
                // the AnnotatedLambda
                Annotation(expr, tpe)
              case gen @ Generic(_, _) =>
                pushGeneric(gen) match {
                  case Some(e1) => self(e1)
                  case None     =>
                    instantiateTo(gen, tpe, kinds)
                }
              case App(fn, aargs, _, tag) =>
                fn match {
                  case AnnotatedLambda(lamArgs, body, _) =>
                    // (\xs - res)(ys) == let x1 = y1 in let x2 = y2 in ... res
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
                        val fn1 = coerceFn1(
                          cArgs.map(_._2),
                          tpe,
                          cArgs.map(_._3),
                          self,
                          kinds
                        )(fn)
                        App(fn1, cArgs.map(_._1), tpe, tag)
                      case _ =>
                        // TODO, what should we do here?
                        // It is currently certainly wrong
                        // we have learned that the type is tpe
                        // but that implies something for fn and arg
                        // but we are ignoring that, which
                        // leaves them with potentially skolems or metavars
                        ann(expr, tpe)
                    }
                }
              case Let(arg, argE, in, rec, tag) =>
                Let(arg, argE, self(in), rec, tag)
              case Match(arg, branches, tag) =>
                // TODO: this may be wrong. e.g. we could leaving meta in the types
                // embedded in patterns, this does not seem to happen since we would
                // error if metas escape typechecking
                Match(
                  arg,
                  branches.map { case (p, expr) => (p, self(expr)) },
                  tag
                )
            }
        }
    }

  /** Return the list of the free vars
    */
  def freeVars[A](ts: List[TypedExpr[A]]): List[Bindable] =
    freeVarsDup(ts).distinct

  def freeVarsSet[A](ts: List[TypedExpr[A]]): SortedSet[Bindable] =
    SortedSet(freeVarsDup(ts)*)

  private def freeVarsDup[A](ts: List[TypedExpr[A]]): List[Bindable] =
    ts.flatMap(_.freeVarsDup)

  /** Try to substitute ex for ident in the expression: in
    *
    * This can fail if the free variables in ex are shadowed above ident in in.
    *
    * this code is very similar to Declaration.substitute if bugs are found in
    * one, consult the other
    */
  def substitute[A](
      ident: Bindable,
      ex: TypedExpr[A],
      in: TypedExpr[A],
      enterLambda: Boolean = true
  ): Option[TypedExpr[A]] =
    substituteAll(Map(ident -> { (_: Local[A]) => ex }), in, enterLambda)

  // Invariant, if enterLambda == true, we always return Some
  def substituteAll[A](
      table: Map[Bindable, Local[A] => TypedExpr[A]],
      in: TypedExpr[A],
      enterLambda: Boolean = true
  ): Option[TypedExpr[A]] = {
    def loop(
        table: Map[Bindable, Local[A] => TypedExpr[A]],
        in: TypedExpr[A]
    ): Option[TypedExpr[A]] =
      in match {
        case local @ Local(i, _, _) =>
          table.get(i) match {
            case Some(te) => Some(te(local))
            case None     => Some(in)
          }
        case Global(_, _, _, _) | Literal(_, _, _) => Some(in)
        case Generic(a, expr)                      =>
          loop(table, expr).map(Generic(a, _))
        case Annotation(t, tpe) =>
          loop(table, t).map(Annotation(_, tpe))
        case lam @ AnnotatedLambda(args, res, tag) =>
          if (!enterLambda) None
          else {
            // this is the same algorithm as python/Code.Expression.substitute
            //
            // the args here can shadow, so we have to remove any
            // items from subMap that have the same Ident
            val argsSet = args.iterator.map(_._1).toSet
            val nonShadowed = table.filterNot { case (i, _) => argsSet(i) }
            // if subFrees is empty, unshadow is a no-op.
            // but that is efficiently handled by unshadow
            val subFrees = nonShadowed.iterator
              .map { case (n, v) =>
                // TODO this isn't great but we just need to get the free vars from the function
                // this assumes the replacement free variables is constant over the type
                // which it should be otherwise we can make ill-typed TypedExpr
                val dummyTpe = res.getType
                freeVarsSet(v(Local(n, tpe = dummyTpe, tag)) :: Nil)
              }
              .foldLeft(nonShadowed.keySet)(_ | _)

            val AnnotatedLambda(args1, res1, tag1) = lam.unshadow(subFrees)
            // now we know that none of args1 shadow anything in subFrees
            // so we can just directly substitute nonShadowed on res1
            // put another way: unshadow make substitute "commute" with lambda.
            val subRes =
              substituteAll(nonShadowed, res1, enterLambda = true).get
            Some(AnnotatedLambda(args1, subRes, tag1))
          }
        case App(fn, args, tpe, tag) =>
          (loop(table, fn), args.traverse(loop(table, _)))
            .mapN(App(_, _, tpe, tag))
        case let @ Let(arg, _, _, _, _) =>
          if (let.recursive.isRecursive) {
            // arg is in scope for argE and in
            // the args here can shadow, so we have to remove any
            // items from subMap that have the same Ident
            val nonShadowed = table - arg
            // if subFrees is empty, unshadow is a no-op.
            // but that is efficiently handled by unshadow
            val subFrees = nonShadowed.iterator
              .map { case (n, v) =>
                // TODO this isn't great but we just need to get the free vars from the function
                // this assumes the replacement free variables is constant over the type
                // which it should be otherwise we can make ill-typed TypedExpr
                val dummyTpe = in.getType
                freeVarsSet(v(Local(n, tpe = dummyTpe, let.tag)) :: Nil)
              }
              .foldLeft(nonShadowed.keySet)(_ | _)

            val Let(arg1, argE1, in1, rec1, tag1) = let.unshadowBoth(subFrees)
            // now we know that none of args1 shadow anything in subFrees
            // so we can just directly substitute nonShadowed on res1
            // put another way: unshadow make substitute "commute" with lambda.
            (
              substituteAll(nonShadowed, argE1, enterLambda),
              substituteAll(nonShadowed, in1, enterLambda)
            )
              .mapN(Let(arg1, _, _, rec1, tag1))
          } else {
            // the scopes are different the binding and the result
            // the args here can shadow, so we have to remove any
            // items from subMap that have the same Ident
            val argsSet = Set(arg)
            val nonShadowed = table.filterNot { case (i, _) => argsSet(i) }
            // if subFrees is empty, unshadow is a no-op.
            // but that is efficiently handled by unshadow
            val subFrees = nonShadowed.iterator
              .map { case (n, v) =>
                // TODO this isn't great but we just need to get the free vars from the function
                // this assumes the replacement free variables is constant over the type
                // which it should be otherwise we can make ill-typed TypedExpr
                val dummyTpe = in.getType
                freeVarsSet(v(Local(n, tpe = dummyTpe, let.tag)) :: Nil)
              }
              .foldLeft(nonShadowed.keySet)(_ | _)

            val Let(arg1, argE1, in1, rec1, tag1) = let.unshadowResult(subFrees)
            // now we know that none of args1 shadow anything in subFrees
            // so we can just directly substitute nonShadowed on res1
            // put another way: unshadow make substitute "commute" with lambda.
            (loop(table, argE1), loop(nonShadowed, in1))
              .mapN(Let(arg1, _, _, rec1, tag1))
          }
        case Match(arg, branches, tag) =>
          // Maintain the order we encounter things:
          val arg1 = loop(table, arg)
          val b1 = branches.traverse { case in @ (p, b) =>
            // these are not free variables in this branch
            val ns = p.names

            val (table1, (p1, b1)) =
              if (ns.isEmpty) (table, in)
              else {
                // the args here can shadow, so we have to remove any
                // items from subMap that have the same Ident
                val argsSet = ns.toSet
                val nonShadowed =
                  if (argsSet.isEmpty) table
                  else table.filterNot { case (i, _) => argsSet(i) }

                // if subFrees is empty, unshadow is a no-op.
                // but that is efficiently handled by unshadow
                val subFrees = nonShadowed.iterator
                  .map { case (n, v) =>
                    // TODO this isn't great but we just need to get the free vars from the function
                    // this assumes the replacement free variables is constant over the type
                    // which it should be otherwise we can make ill-typed TypedExpr
                    val dummyTpe = b.getType
                    freeVarsSet(v(Local(n, tpe = dummyTpe, tag)) :: Nil)
                  }
                  .foldLeft(nonShadowed.keySet)(_ | _)

                (nonShadowed, unshadowBranch[A](subFrees, in))
              }
            // now we know that none of args1 shadow anything in subFrees
            // so we can just directly substitute nonShadowed on res1
            // put another way: unshadow make substitute "commute" with lambda.
            loop(table1, b1).map((p1, _))
          }
          (arg1, b1).mapN(Match(_, _, tag))
      }

    loop(table, in)
  }

  private def unshadowBranch[A](
      freeSet: Set[Bindable],
      branch: Branch[A]
  ): Branch[A] = {
    // we only get in here when p has some names
    val (p, b) = branch
    val args = NonEmptyList.fromList(p.names) match {
      case None          => return branch
      case Some(argsNel) => argsNel
    }

    val clashIdent =
      if (freeSet.isEmpty) Set.empty[Bindable]
      else args.iterator.filter(freeSet).toSet

    if (clashIdent.isEmpty) branch
    else {
      // we have to allocate new variables
      type I = Bindable
      def inc(n: I, idx: Int): I =
        n match {
          case Identifier.Name(n) => Identifier.Name(n + idx.toString)
          case _                  => Identifier.Name("a" + idx.toString)
        }

      def alloc(ident: I, tail: List[I], avoid: Set[I]): NonEmptyList[I] = {
        val ident1 =
          if (clashIdent(ident)) {
            // the following iterator is infinite and distinct, and the avoid
            // set is finite, so the get here must terminate in at most avoid.size
            // steps
            Iterator
              .from(0)
              .map(i => inc(ident, i))
              .collectFirst { case n if !avoid(n) => n }
              .get

          } else ident

        tail match {
          case Nil    => NonEmptyList.one(ident1)
          case h :: t =>
            ident1 :: alloc(h, t, avoid + ident1)
        }
      }

      val avoids = freeSet | freeVarsSet(b :: Nil)
      val newArgs = alloc(args.head, args.tail, avoids)
      val resSub = args.iterator
        .zip(newArgs.iterator.map { n1 => (loc: Local[A]) =>
          Local(n1, loc.tpe, loc.tag)
        })
        .toMap

      // calling .get is safe when enterLambda = true
      val b1 = substituteAll(resSub, b, enterLambda = true).get
      val p1 = p.substitute(args.iterator.zip(newArgs.iterator).toMap)

      (p1, b1)
    }
  }

  def substituteTypeVar[A](
      typedExpr: TypedExpr[A],
      env: Map[Type.Var, Type]
  ): TypedExpr[A] =
    if (env.isEmpty) typedExpr
    else
      typedExpr match {
        case Generic(quant, expr) =>
          // we need to remove the params which are shadowed below
          val paramSet: Set[Type.Var] =
            quant.vars.toList.iterator.map(_._1).toSet
          val env1 = env.iterator.filter { case (k, _) => !paramSet(k) }.toMap
          Generic(quant, substituteTypeVar(expr, env1))
        case Annotation(of, tpe) =>
          Annotation(substituteTypeVar(of, env), Type.substituteVar(tpe, env))
        case AnnotatedLambda(args, res, tag) =>
          AnnotatedLambda(
            args.map { case (n, tpe) =>
              (n, Type.substituteVar(tpe, env))
            },
            substituteTypeVar(res, env),
            tag
          )
        case Local(v, tpe, tag) =>
          Local(v, Type.substituteVar(tpe, env), tag)
        case Global(p, v, tpe, tag) =>
          Global(p, v, Type.substituteVar(tpe, env), tag)
        case App(f, args, tpe, tag) =>
          App(
            substituteTypeVar(f, env),
            args.map(substituteTypeVar(_, env)),
            Type.substituteVar(tpe, env),
            tag
          )
        case Let(v, exp, in, rec, tag) =>
          Let(
            v,
            substituteTypeVar(exp, env),
            substituteTypeVar(in, env),
            rec,
            tag
          )
        case Literal(lit, tpe, tag) =>
          Literal(lit, Type.substituteVar(tpe, env), tag)
        case Match(expr, branches, tag) =>
          val branches1 = branches.map { case (p, t) =>
            val p1 = p.mapType(Type.substituteVar(_, env))
            val t1 = substituteTypeVar(t, env)
            (p1, t1)
          }
          val expr1 = substituteTypeVar(expr, env)
          Match(expr1, branches1, tag)
      }

  private def replaceVarType[A](
      te: TypedExpr[A],
      name: Bindable,
      tpe: Type
  ): TypedExpr[A] = {
    def recur(t: TypedExpr[A]) = replaceVarType(t, name, tpe)

    te match {
      case Generic(tv, in)                  => Generic(tv, recur(in))
      case Annotation(term, tpe)            => Annotation(recur(term), tpe)
      case AnnotatedLambda(args, expr, tag) =>
        // this is a kind of let:
        if (args.exists(_._1 === name)) {
          // we are shadowing, so we are done:
          te
        } else {
          // no shadow
          AnnotatedLambda(args, recur(expr), tag)
        }
      case Local(nm, _, tag) if nm === name => Local(name, tpe, tag)
      case n: Name[A]                      => n
      case App(fnT, args, tpe, tag)        =>
        App(recur(fnT), args.map(recur), tpe, tag)
      case Let(b, e, in, r, t) =>
        if (b === name) {
          if (r.isRecursive) {
            // in this case, b is in scope for e
            // so it shadows a the previous definition
            te // shadow
          } else {
            // then b is not in scope for e
            // but b does shadow inside `in`
            Let(b, recur(e), in, r, t)
          }
        } else Let(b, recur(e), recur(in), r, t)
      case lit @ Literal(_, _, _)    => lit
      case Match(arg, branches, tag) =>
        Match(recur(arg), branches.map { case (p, t) => (p, recur(t)) }, tag)
    }
  }

  private def ann[A](te: TypedExpr[A], tpe: Type): TypedExpr[A] =
    if (te.getType.sameAs(tpe)) te
    else Annotation(te, tpe)

  /** TODO this seems pretty expensive to blindly apply: we are deoptimizing the
    * nodes pretty heavily
    */
  def coerceFn(
      args: NonEmptyList[Type],
      result: Type.Rho,
      coarg: NonEmptyList[Coerce],
      cores: Coerce,
      kinds: Type => Option[Kind]
  ): Coerce =
    coerceFn1(args, result, coarg.map(Some(_)), cores, kinds)

  private def coerceFn1(
      arg: NonEmptyList[Type],
      result: Type.Rho,
      coargOpt: NonEmptyList[Option[Coerce]],
      cores: Coerce,
      kinds: Type => Option[Kind]
  ): Coerce =
    new FunctionK[TypedExpr, TypedExpr] { self =>
      val fntpe = Type.Fun(arg, result)

      def apply[A](expr: TypedExpr[A]) =
        expr match {
          case _ if expr.getType.sameAs(fntpe)  => expr
          case Annotation(t, _)                 => self(t)
          case AnnotatedLambda(args0, res, tag) =>
            // note, Var(None, name, originalType, tag)
            // is hanging out in res, or it is unused
            val args1 = args0.zip(arg).map { case ((n, _), t) =>
              (n, t)
            }
            val res1 = args1.toList
              .foldRight(res) { case ((name, arg), res) =>
                replaceVarType(res, name, arg)
              }
            AnnotatedLambda(args1, cores(res1), tag)
          case gen @ Generic(_, _) =>
            pushGeneric(gen) match {
              case Some(e1) => self(e1)
              case None     =>
                instantiateTo(gen, fntpe, kinds)
            }
          case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
            ann(expr, fntpe)
          case Let(arg, argE, in, rec, tag) =>
            Let(arg, argE, self(in), rec, tag)
          case Match(arg, branches, tag) =>
            // TODO: this may be wrong. e.g. we could leaving meta in the types
            // embedded in patterns, this does not seem to happen since we would
            // error if metas escape typechecking
            Match(arg, branches.map { case (p, expr) => (p, self(expr)) }, tag)
          case App(AnnotatedLambda(lamArgs, body, _), aArgs, _, tag) =>
            // (\x - res)(y) == let x = y in res
            val arg1 = lamArgs.zip(aArgs).map {
              case ((n, rho: Type.Rho), arg) => (n, coerceRho(rho, kinds)(arg))
              case ((n, _), arg)             => (n, arg)
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
            val nameGen = Type.allBinders.iterator
              .map(v => Identifier.Name(v.name))
              .filterNot(free)
            val lamArgs = arg.map(t => (nameGen.next(), t))
            val aArgs = lamArgs.map { case (n, t) => Local(n, t, expr.tag) }
            // name -> (expr((name: arg)): result)
            val result1 = cores(App(expr, aArgs, result, expr.tag))
            AnnotatedLambda(lamArgs, result1, expr.tag)
        }
    }

  def forAll[A](
      params: NonEmptyList[(Type.Var.Bound, Kind)],
      expr: TypedExpr[A]
  ): TypedExpr[A] =
    quantVars(forallList = params.toList, Nil, expr)

  def normalizeQuantVars[A](
      q: Type.Quantification,
      expr: TypedExpr[A]
  ): TypedExpr[A] =
    expr match {
      case Generic(oldQuant, ex0) =>
        normalizeQuantVars(q.concat(oldQuant), ex0)
      case Annotation(term, tpe)
          if Type.quantify(q, tpe).sameAs(term.getType) =>
        // we not uncommonly add an annotation just to make a generic wrapper to get back where
        term
      case Annotation(term, tpe)
          if !q.vars.iterator
            .map(_._1)
            .exists(
              Type.freeBoundTyVars(expr.getType :: Nil).toSet
            ) =>
        // the variables may be free lower, but not here
        val genTerm = normalizeQuantVars(q, term)
        if (genTerm.getType.sameAs(tpe)) genTerm
        else Annotation(normalizeQuantVars(q, term), tpe)
      case _ =>
        import Type.Quantification._
        // We cannot rebind to any used typed inside of expr, but we can reuse
        // any that are q
        val frees: Set[Type.Var.Bound] =
          expr.freeTyVars.iterator.collect { case b: Type.Var.Bound =>
            b
          }.toSet

        q.filter(frees) match {
          case None    => expr
          case Some(q) =>
            val varSet = q.vars.iterator.map { case (b, _) => b }.toSet

            val avoid: Set[Type.Var.Bound] =
              expr.allBound.diff(varSet)

            q match {
              case ForAll(vars) =>
                val fa1 = Type.alignBinders(vars, avoid)
                val subs = fa1.iterator
                  .collect {
                    case ((b, _), b1) if b =!= b1 =>
                      (b, Type.TyVar(b1))
                  }
                  .toMap[Type.Var, Type]

                Generic(
                  ForAll(fa1.map { case ((_, k), b) => (b, k) }),
                  substituteTypeVar(expr, subs)
                )
              case Exists(vars) =>
                val ex1 = Type.alignBinders(vars, avoid)
                val subs = ex1.iterator
                  .collect {
                    case ((b, _), b1) if b =!= b1 =>
                      (b, Type.TyVar(b1))
                  }
                  .toMap[Type.Var, Type]

                Generic(
                  Exists(ex1.map { case ((_, k), b) => (b, k) }),
                  substituteTypeVar(expr, subs)
                )
              case Dual(foralls, exists) =>
                val fa1 = Type.alignBinders(foralls, avoid)
                val ex1 =
                  Type.alignBinders(exists, avoid ++ fa1.iterator.map(_._2))
                val subs = (fa1.iterator ++ ex1.iterator)
                  .collect {
                    case ((b, _), b1) if b =!= b1 =>
                      (b, Type.TyVar(b1))
                  }
                  .toMap[Type.Var, Type]

                Generic(
                  Dual(
                    fa1.map { case ((_, k), b) => (b, k) },
                    ex1.map { case ((_, k), b) => (b, k) }
                  ),
                  substituteTypeVar(expr, subs)
                )
            }
        }
    }

  type Branch[A] =
    (Pattern[(PackageName, Constructor), Type], TypedExpr[A])

  def quantVars[A](
      forallList: List[(Type.Var.Bound, Kind)],
      existList: List[(Type.Var.Bound, Kind)],
      expr: TypedExpr[A]
  ): TypedExpr[A] =
    Type.Quantification.fromLists(
      forallList = forallList,
      existList = existList
    ) match {
      case Some(q) => Generic(q, expr)
      case None    => expr
    }

  private def lambda[A](
      args: NonEmptyList[(Bindable, Type)],
      expr: TypedExpr[A],
      tag: A
  ): TypedExpr[A] =
    AnnotatedLambda(args, expr, tag)

  implicit def typedExprHasRegion[T: HasRegion]: HasRegion[TypedExpr[T]] =
    HasRegion.instance[TypedExpr[T]](e => HasRegion.region(e.tag))

  // noshadow must include any free vars of args
  def liftQuantification[A](
      args: NonEmptyList[TypedExpr[A]],
      noshadow: Set[Type.Var.Bound]
  ): (
      Option[Type.Quantification],
      NonEmptyList[TypedExpr[A]]
  ) = {

    val htype = args.head.getType
    val (oq, rest) = NonEmptyList.fromList(args.tail) match {
      case Some(neTail) =>
        val (oq, rest) = liftQuantification(neTail, noshadow)
        (oq, rest.toList)
      case None =>
        (None, Nil)
    }

    htype match {
      case Type.Quantified(q, rho) =>
        oq match {
          case Some(qtail) =>
            // we have to unshadow with noshadow + all the vars in the tail
            val (map, q1) =
              q.unshadow(noshadow ++ qtail.vars.toList.iterator.map(_._1))
            val rho1 = Type.substituteRhoVar(rho, map)
            (
              Some(q1.concat(qtail)),
              NonEmptyList(TypedExpr.Annotation(args.head, rho1), rest)
            )
          case None =>
            val (map, q1) = q.unshadow(noshadow)
            val rho1 = Type.substituteRhoVar(rho, map)
            (
              Some(q1),
              NonEmptyList(TypedExpr.Annotation(args.head, rho1), rest)
            )
        }

      case _ =>
        (oq, NonEmptyList(args.head, rest))
    }
  }

  def usedGlobals[A](
      te: TypedExpr[A]
  ): State[Set[(PackageName, Identifier)], TypedExpr[A]] = {
    type VSet = Set[(PackageName, Identifier)]
    type VState[X] = State[VSet, X]
    te.traverseUp[VState] {
      case g @ TypedExpr.Global(p, n, _, _) =>
        State(s => (s + ((p, n)), g))
      case m @ Match(_, branches, _) =>
        branches
          .traverse_ { case (pat, _) =>
            pat
              .traverseStruct[
                VState,
                (PackageName, Identifier.Constructor)
              ] { (n, parts) =>
                State.modify[VSet](_ + n) *>
                  parts.map { inner =>
                    Pattern.PositionalStruct(n, inner)
                  }
              }
              .void
          }
          .as(m)
      case te => Monad[VState].pure(te)
    }
  }
}
