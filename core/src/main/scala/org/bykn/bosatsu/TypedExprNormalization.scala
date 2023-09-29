package org.bykn.bosatsu

import cats.Foldable
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.{Type, TypeEnv}

import Identifier.{Bindable, Constructor}

import cats.syntax.all._

object TypedExprNormalization {
  import TypedExpr._

  type ScopeT[A, S] = Map[(Option[PackageName], Bindable), (RecursionKind, TypedExpr[A], S)]
  type Scope[A] = FixType.Fix[ScopeT[A, *]]

  def emptyScope[A]: Scope[A] =
    FixType.fix[ScopeT[A, *]](Map.empty)

  implicit final class ScopeOps[A](private val scope: Scope[A]) extends AnyVal {
    def updated(key: Bindable, value: (RecursionKind, TypedExpr[A], Scope[A])): Scope[A] =
      FixType.fix[ScopeT[A, *]](FixType.unfix[ScopeT[A, *]](scope).updated((None, key), value))

    def updatedGlobal(pack: PackageName, key: Bindable, value: (RecursionKind, TypedExpr[A], Scope[A])): Scope[A] =
      FixType.fix[ScopeT[A, *]](FixType.unfix[ScopeT[A, *]](scope).updated((Some(pack), key), value))

    def -(key: Bindable): Scope[A] =
      FixType.fix[ScopeT[A, *]](FixType.unfix[ScopeT[A, *]](scope) - (None -> key))

    def --(keys: Iterable[Bindable]): Scope[A] =
      keys.foldLeft(scope)(_ - _)

    def getLocal(key: Bindable): Option[(RecursionKind, TypedExpr[A], Scope[A])] =
      FixType.unfix[ScopeT[A, *]](scope).get((None, key))

    def getGlobal(pack: PackageName, n: Bindable): Option[(RecursionKind, TypedExpr[A], Scope[A])] =
      FixType.unfix[ScopeT[A, *]](scope).get((Some(pack), n))
  }

  private def nameScope[A](b: Bindable, r: RecursionKind, scope: Scope[A]): (Option[Bindable], Scope[A]) =
    if (r.isRecursive) (Some(b), scope - b)
    else (None, scope)

  def normalizeAll[A, V](pack: PackageName, lets: List[(Bindable, RecursionKind, TypedExpr[A])], typeEnv: TypeEnv[V])(implicit ev: V <:< Kind.Arg): List[(Bindable, RecursionKind, TypedExpr[A])] = {
    @annotation.tailrec
    def loop(scope: Scope[A], lets: List[(Bindable, RecursionKind, TypedExpr[A])], acc: List[(Bindable, RecursionKind, TypedExpr[A])]): List[(Bindable, RecursionKind, TypedExpr[A])] =
      lets match {
        case Nil => acc.reverse
        case (b, r, t) :: tail =>
          // if we have a recursive value it shadows the scope
          val (optName, s0) = nameScope(b, r, scope)
          val normTE = normalize1(optName, t, s0, typeEnv).get
          val scope1 = scope.updatedGlobal(pack, b, (r, normTE, s0))
          loop(scope1, tail, (b, r, normTE) :: acc)
      }

    loop(emptyScope, lets, Nil)
  }

  def normalizeProgram[A, V](
    p: PackageName,
    fullTypeEnv: TypeEnv[V],
    prog: Program[TypeEnv[V], TypedExpr[Declaration], A])(implicit ev: V <:< Kind.Arg): Program[TypeEnv[V], TypedExpr[Declaration], A] = {
      val Program(typeEnv, lets, extDefs, stmts) = prog
      val normalLets = normalizeAll(p, lets, fullTypeEnv)
      Program(typeEnv, normalLets, extDefs, stmts)
    }

  // if you have made one step of progress, use this to recurse
  // so we don't throw away if we don't progress more
  private def normalize1[A, V](namerec: Option[Bindable], te: TypedExpr[A], scope: Scope[A], typeEnv: TypeEnv[V])(implicit ev: V <:< Kind.Arg): Some[TypedExpr[A]] =
    normalizeLetOpt(namerec, te, scope, typeEnv) match {
      case None => Some(te)
      case s@Some(_) => s
    }

  private def setType[A](expr: TypedExpr[A], tpe: Type): TypedExpr[A] =
    if (!tpe.sameAs(expr.getType)) Annotation(expr, tpe) else expr

  /**
   * if the te is not in normal form, transform it into normal form
   */
  private def normalizeLetOpt[A, V](namerec: Option[Bindable], te: TypedExpr[A], scope: Scope[A], typeEnv: TypeEnv[V])(implicit ev: V <:< Kind.Arg): Option[TypedExpr[A]] = {
    val kindOf: Type => Option[Kind] =
      Type.kindOfOption { case const @ Type.TyConst(_) => typeEnv.getType(const).map(_.kindOf) }

    te match {
      case g@Generic(_, Annotation(term, _)) if g.getType.sameAs(term.getType) =>
        normalize1(namerec, term, scope, typeEnv)
      case g@Generic(vars, in) =>
        // normalize the inside, then get all the freeBoundTyVars and
        // and if we can reallocate typevars to be the a, b, ... do so,
        // if they are the same, return none
        //
        // Also, Generic(Generic(...)) => Generic
        // Generic(vs, te, _) but vs are not bound in te, we can remove Generic

        // if tpe has no Var.Bound in common,
        // then we don't need the forall
        val tpe = in.getType
        val frees = Type.freeBoundTyVars(tpe :: Nil).toSet
        val freeVars = vars.toList.filter { case (t, _) => frees(t) }
        NonEmptyList.fromList(freeVars) match {
          case None => normalize1(namerec, in, scope, typeEnv)
          case Some(nonEmpty) =>
            normalizeLetOpt(namerec, in, scope, typeEnv) match {
              case None =>
                if (freeVars == vars.toList) None
                else Some(Generic(nonEmpty, in))
              case Some(gen@Generic(_, _)) =>
                // in1 could be a generic in a
                Some(forAll(nonEmpty, gen))
              case Some(Annotation(term, _)) if g.getType.sameAs(term.getType) =>
                Some(term)
              case Some(notGen) =>
                Some(Generic(nonEmpty, notGen))
            }
        }
      case Annotation(term, tpe) =>
        // if we annotate twice, we can ignore the inner annotation
        // we should have type annotation where we normalize type parameters
        val e1 = normalize1(namerec, term, scope, typeEnv).get
        (e1, tpe) match {
          case _ if e1.getType.sameAs(tpe) =>
            // the type is already right
            Some(e1)
          case (gen@Generic(_, _), rho: Type.Rho) =>
            val inst = TypedExpr.instantiateTo(gen, rho, kindOf)
            // we compare thes to te because instantiate
            // can add an Annotation back
            if (inst != te) Some(inst)
            else None
          case (notSameTpe, _) =>
            val nt = Type.normalize(tpe)
            if (notSameTpe eq term) {
              if (nt == tpe) None
              else Some(Annotation(term, nt))
            }
            else Some(Annotation(notSameTpe, nt))
        }

      case AnnotatedLambda(lamArgs0, expr, tag) =>
        lazy val anons: Iterator[Bindable] = Expr.nameIterator()
          .filterNot(expr.freeVarsDup.toSet)

        val bodyScope = scope -- lamArgs0.toList.map(_._1)
        val e1 = normalize1(None, expr, bodyScope, typeEnv).get

        var changed = false
        val lamArgs = lamArgs0.map { case (n, t) =>
          val n1 =
            if (e1.notFree(n)) {
              // n is not used.
              val next = anons.next()
              changed = changed || (next != n)
              next
            }
            else {
              n
            }
          (n1, Type.normalize(t))
        }

        if (changed) {
          normalize1(namerec,
            AnnotatedLambda(lamArgs, e1, tag),
            scope,
            typeEnv)

        }
        else {

        def doesntUseArgs(te: TypedExpr[A]): Boolean =
          lamArgs.forall { case (n, _) => te.notFree(n) }

        // assuming b is bound below lamArgs, return true if it doesn't shadow an arg
        def doesntShadow(b: Bindable): Boolean =
          !lamArgs.exists { case (n, _) => n === b }
        
        def matchesArgs(nel: NonEmptyList[TypedExpr[A]]): Boolean =
          (nel.length == lamArgs.length) && lamArgs.iterator.zip(nel.iterator).forall {
            case ((lamN, _), Local(argN, _, _)) => lamN === argN
            case _ => false
          }

        val ws = Impl.WithScope(scope, ev.substituteCo[TypeEnv](typeEnv))
        e1 match {
          case App(fn, aargs, _, _) if matchesArgs(aargs) && doesntUseArgs(fn) =>
            // x -> f(x) == f (eta conversion)
            normalize1(None, setType(fn, te.getType), scope, typeEnv)
          case App(ws.ResolveToLambda(Nil, args1, body, ftag), aargs, resT, atag) if namerec.isEmpty =>
            // args -> (args1 -> e1)(...)
            // this is inlining, which we do only when nested directly inside another lambda
            // TODO: this is possibly very expensive to always apply. It can really increase
            // code size. We probably need better hueristics for when to inline,
            // or remove inlining from here unless it can hever hurt and put inlining at a
            // different phase.
            val fn1 = AnnotatedLambda(args1, body, ftag)
            val e2 = App(fn1, aargs, resT, atag)
            if (e1 != e2) {
              // in this case we have inlined, vs there already being
              // a literal lambda being applied
              // by normalizing this, it will become a let binding
              val e3 = normalize1(None, e2, bodyScope, typeEnv).get
              
              if (e3.size <= expr.size) {
                // we haven't made the code larger
                normalize1(namerec,
                  AnnotatedLambda(lamArgs, e3, tag),
                  scope, typeEnv)
              }
              else {
                // inlining will make the code larger that it was originally
                if ((e1 eq expr) && (lamArgs === lamArgs0)) None
                else Some(AnnotatedLambda(lamArgs, e1, tag))
              }
            }
            else {
              if ((e1 eq expr) && (lamArgs === lamArgs0)) None
              else Some(AnnotatedLambda(lamArgs, e1, tag))
            }
          case Let(arg1, ex, in, rec, tag1) if doesntUseArgs(ex) && doesntShadow(arg1) =>
            // x ->
            //   y = z
            //   f(y)
            //same as:
            //y = z
            //x -> f(y)
            //avoid recomputing y
            //TODO: we could reorder Lets if we have several in a row
            normalize1(None, Let(arg1, ex, AnnotatedLambda(lamArgs, in, tag), rec, tag1), scope, typeEnv)
          case m@Match(arg1, branches, tag1) if lamArgs.forall { case (arg, _) => arg1.notFree(arg) } =>
            // same as above: if match does not depend on lambda arg, lift it out
              val b1 = branches.traverse { case (p, b) =>
                if (!lamArgs.exists { case (arg, _) => p.names.contains(arg) }) {
                  Some((p, AnnotatedLambda(lamArgs, b, tag)))
                }
                else None
              }
              b1 match {
                case None =>
                  if ((m eq expr) && (lamArgs === lamArgs0)) None
                  else Some(AnnotatedLambda(lamArgs, m, tag))
                case Some(bs) =>
                  val m1 = Match(arg1, bs, tag1)
                  normalize1(namerec, m1, scope, typeEnv)
              }
          case notApp =>
            if ((notApp eq expr) && (lamArgs === lamArgs0)) None
            else Some(AnnotatedLambda(lamArgs, notApp, tag))
        }
      }
      case Literal(_, _, _) =>
        // these are fundamental
        None
      case Global(p, n: Constructor, tpe0, tag) =>
        val tpe = Type.normalize(tpe0)
        if (tpe == tpe0) None
        else Some(Global(p, n, tpe, tag))
      case Global(p, n: Bindable, tpe0, tag) =>
        scope.getGlobal(p, n).flatMap {

          case (RecursionKind.NonRecursive, te, _) if Impl.isSimple(te, lambdaSimple = false) =>
            // inlining lambdas naively can cause an exponential blow up in size
            Some(te)
          case _ =>
            val tpe = Type.normalize(tpe0)
            if (tpe == tpe0) None
            else Some(Global(p, n, tpe, tag))
        }
      case Local(n, tpe0, tag) =>
        // TODO we could look in the scope
        // and potentially simplify, but maybe it
        // is too late here, we want to do that when
        // we have another potential optimization?
        val tpe = Type.normalize(tpe0)
        if (tpe == tpe0) None
        else Some(Local(n, tpe, tag))
      case App(fn, args, tpe0, tag) =>
        val tpe = Type.normalize(tpe0)
        val f1 = normalize1(None, fn, scope, typeEnv).get
        // the second and third branches use this but the first doesn't
        // make it lazy so we don't recurse more than needed
        lazy val a1 = ListUtil.mapConserveNel(args) { a =>
          normalize1(None, a, scope, typeEnv).get
        }

        f1 match {
          case AnnotatedLambda(lamArgs, expr, _) =>
            // (y -> z)(x) = let y = x in z
            val lets = lamArgs.zip(args).map {
              case ((n, ltpe), arg) => (n, setType(arg, ltpe))
            }
            val expr2 = setType(expr, tpe)
            val l = TypedExpr.letAllNonRec(lets, expr2, tag)
            normalize1(namerec, l, scope, typeEnv)
          case Let(arg1, ex, in, rec, tag1) if a1.forall(_.notFree(arg1)) =>
              // (app (let x y z) w) == (let x y (app z w)) if w does not have x free
              normalize1(namerec, Let(arg1, ex, App(in, a1, tpe, tag), rec, tag1), scope, typeEnv)
          case _ =>
            if ((f1 eq fn) && (tpe == tpe0) && (a1 eq args)) None
            else Some(App(f1, a1, tpe, tag))
        }
      case Let(arg, ex, in, rec, tag) =>
        // note, Infer has already checked
        // to make sure rec is accurate
        val (ni, si) = nameScope(arg, rec, scope)
        val ex1 = normalize1(ni, ex, si, typeEnv).get
        ex1 match {
          case Let(ex1a, ex1ex, ex1in, RecursionKind.NonRecursive, ex1tag) if !rec.isRecursive && in.notFree(ex1a) =>
            // according to a SPJ paper, it is generally better
            // to float lets out of nesting inside in:
            // let foo = let bar = x in bar in foo
            //
            // is better to write:
            // let bar = x in let foo = bar in foo
            // since you are going to evaluate and keep in scope
            // the expression
            // we can lift
            val l1 = Let(ex1a, ex1ex, Let(arg, ex1in, in, RecursionKind.NonRecursive, tag), RecursionKind.NonRecursive, ex1tag)
            normalize1(namerec, l1, scope, typeEnv)
          case _ =>
            val scopeIn = si.updated(arg, (rec, ex1, si))

            val in1 = normalize1(namerec, in, scopeIn, typeEnv).get
            in1 match {
              case Match(marg, branches, mtag) if !rec.isRecursive && marg.notFree(arg) && branches.exists { case (p, r) => p.names.contains(arg) || r.notFree(arg) } =>
                // x = y
                // match z:
                //   case w: ww
                //
                // can be rewritten as
                // match z:
                //   case w:
                //     x = y
                //     ww
                //
                // when z is not free in x, and at least one branch is not free in x
                val b1 = branches.map { case (p, r) =>
                  if (p.names.contains(arg) || r.notFree(arg)) (p, r)
                  else (p, Let(arg, ex1, r, rec, tag))
                }
                normalize1(namerec, Match(marg, b1, mtag), scope, typeEnv)
              case _ =>
                val cnt = in1.freeVarsDup.count(_ === arg)
                if (cnt > 0) {
                  // the arg is needed
                  val shouldInline = (!rec.isRecursive) && {
                    (cnt == 1) || Impl.isSimple(ex1, lambdaSimple = true)
                  }
                  val inlined = if (shouldInline) substitute(arg, ex1, in1) else None
                  inlined match {
                    case Some(il) =>
                      normalize1(namerec, il, scope, typeEnv)
                    case None =>
                      if ((in1 eq in) && (ex1 eq ex)) None
                      else normalize1(namerec, Let(arg, ex1, in1, rec, tag), scope, typeEnv)
                  }
                }
                else {
                  // let x = y in z if x isn't free in z = z
                  Some(in1)
                }
            }
        }

      case Match(_, NonEmptyList((p, e), Nil), _) if !e.freeVarsDup.exists(p.names.toSet) =>
        // match x:
        //   foo: fn
        //
        // where foo has no names can become just fn
        normalize1(namerec, e, scope, typeEnv)
      case Match(arg, NonEmptyList((Pattern.SinglyNamed(y), e), Nil), tag) =>
        // match x:
        //   y: fn
        // let y = x in fn
        normalize1(namerec, Let(y, arg, e, RecursionKind.NonRecursive, tag), scope, typeEnv)
      case Match(arg, branches, tag) =>

        def ncount(shadows: Iterable[Bindable], e: TypedExpr[A]): (Int, TypedExpr[A]) =
          // the final result of the branch is what is assigned to the name
          normalizeLetOpt(None, e, scope -- shadows, typeEnv) match {
            case None => (0, e)
            case Some(e) => (1, e)
          }
        // we can remove any bindings that aren't used in branches
        val (changed0, branches1) =
          branches
            .traverse { case (p, t) =>
              val (c, t1) = ncount(p.names, t)
              val freeT1 = t1.freeVarsDup.toSet
              // we don't need to keep any variables that aren't free
              // TODO: we can still replace total matches with _
              // such as Foo(_, _, _) for structs or unions that are total
              val p1 = p.filterVars(freeT1)
              val c1 = if (p1 == p) c else (c + 1)
              (c1, (p1, t1))
            }
        // due to total matches, the last branch without any bindings
        // can always be rewritten as _
        val (changed1, branches1a) =
          branches1.last._1 match {
            case Pattern.WildCard =>
              (changed0, branches1)
            case notWild if notWild.names.isEmpty =>
              val newb = branches1.init ::: ((Pattern.WildCard, branches1.last._2) :: Nil)
              // this newb list clearly has more than 0 elements
              (changed0 + 1, NonEmptyList.fromListUnsafe(newb))
            case _ =>
              (changed0, branches1)
          }
        val a1 = normalize1(None, arg, scope, typeEnv).get
        if (changed1 == 0) {
          val m1 = Match(a1, branches, tag)
          Impl.maybeEvalMatch(m1, scope) match {
            case None =>
              // if only the arg changes, there
              // is no need to rerun the normalization
              // because normalization of branches
              // does not depend on the arg
              if (a1 eq arg) None
              else Some(m1)
            case Some(m2) =>
              // TODO: we may not have a proof that m2 is smaller
              // than m1. requiring m2.size < m1.size fails some tests
              // we can possibly simplify this now:
              normalize1(namerec, m2, scope, typeEnv)
            case _ => None
          }
        }
        else {
          // there has been some change, so
          // see if that unlocked any new changes
          normalize1(namerec, Match(a1, branches1a, tag), scope, typeEnv)
        }
    }
  }

  def normalize[A](te: TypedExpr[A]): Option[TypedExpr[A]] =
    normalizeLetOpt(None, te, emptyScope, TypeEnv.empty)

  private object Impl {

    def scopeMatches[A](names: Set[Bindable], scope: Scope[A], scope1: Scope[A]): Boolean =
      names.forall { b =>
        (scope.getLocal(b), scope1.getLocal(b)) match {
          case (None, None) => true
          case (Some((r1, t1, s1)), Some((r2, t2, s2))) =>
            (r1 == r2) &&
              (t1.void == t2.void) &&
              scopeMatches(t1.freeVarsDup.toSet, s1, s2)
          case _ => false
        }
      }

    case class WithScope[A](scope: Scope[A], typeEnv: TypeEnv[Kind.Arg]) {
      private lazy val kindOf: Type => Option[Kind] =
        Type.kindOfOption { case const @ Type.TyConst(_) => typeEnv.getType(const).map(_.kindOf) }

      object ResolveToLambda {
        // TODO: don't we need to worry about the type environment for locals? They
        // can also capture type references to outer Generics
        def unapply(te: TypedExpr[A]): Option[(List[(Type.Var.Bound, Kind)], NonEmptyList[(Bindable, Type)], TypedExpr[A], A)] =
          te match {
            case Annotation(ResolveToLambda((h :: t), args, ex, tag), rho: Type.Rho) =>
              val asGen =
                Generic(NonEmptyList(h, t), AnnotatedLambda(args, ex, tag))

              TypedExpr.instantiateTo(asGen, rho, kindOf) match {
                case AnnotatedLambda(a, e, t) => Some((Nil, a, e, t))
                case Generic(nel, AnnotatedLambda(a, e, t)) => Some((nel.toList, a, e, t))
                case _ => None
              }
            case Generic(frees, ResolveToLambda(f1, args, ex, tag)) =>
              Some((frees.toList ::: f1, args, ex, tag))
            case AnnotatedLambda(args, expr, ltag) => Some((Nil, args, expr, ltag))
            case Global(p, n: Bindable, _, _) =>
              scope.getGlobal(p, n).flatMap {
                case (RecursionKind.NonRecursive, te, scope1) =>
                  val s1 = WithScope(scope1, typeEnv)
                  te match {
                    case s1.ResolveToLambda(frees, args, expr, ltag) =>
                      // we can't just replace variables if the scopes don't match.
                      // we could also repair the scope by making a let binding
                      // for any names that don't match (which has to be done recursively
                      if (scopeMatches(expr.freeVarsDup.toSet -- args.iterator.map(_._1), scope, scope1)) {
                        Some((frees, args, expr, ltag))
                      }
                      else None
                    case _ => None
                  }
                case _ => None
              }
            case Local(nm, _, _) =>
              scope.getLocal(nm).flatMap {
                case (RecursionKind.NonRecursive, te, scope1) =>
                  val s1 = WithScope(scope1, typeEnv)
                  te match {
                    case s1.ResolveToLambda(frees, args, expr, ltag) =>
                      // we can't just replace variables if the scopes don't match.
                      // we could also repair the scope by making a let binding
                      // for any names that don't match (which has to be done recursively
                      if (scopeMatches(expr.freeVarsDup.toSet -- args.iterator.map(_._1), scope, scope1)) {
                        Some((frees, args, expr, ltag))
                      }
                      else None
                    case _ => None
                  }
                case _ => None
              }
            case _ => None
          }
      }
    }

    @annotation.tailrec
    final def isSimple[A](ex: TypedExpr[A], lambdaSimple: Boolean): Boolean =
      ex match {
        case Literal(_, _, _) | Local(_, _, _) | Global(_, _, _, _) => true
        case Annotation(t, _) => isSimple(t, lambdaSimple)
        case Generic(_, t) => isSimple(t, lambdaSimple)
        case AnnotatedLambda(_, _, _) =>
          // maybe inline lambdas so we can possibly
          // apply (x -> f)(g) => let x = g in f
          lambdaSimple
        case _ => false
      }

    sealed abstract class EvalResult[A]
    object EvalResult {
      case class Cons[A](pack: PackageName, cons: Constructor, args: List[TypedExpr[A]]) extends EvalResult[A]
      case class Constant[A](lit: Lit) extends EvalResult[A]
    }

    object FnArgs {
      def unapply[A](te: TypedExpr[A]): Option[(TypedExpr[A], NonEmptyList[TypedExpr[A]])] =
        te match {
          case App(fn, args, _, _) => Some((fn, args))
          case _ => None
        }
    }

    def evaluate[A](te: TypedExpr[A], scope: Scope[A]): Option[EvalResult[A]] =
      te match {
        case Literal(lit, _, _) => Some(EvalResult.Constant(lit))
        case Local(b, _, _) =>
          scope.getLocal(b).flatMap {
            case (RecursionKind.NonRecursive, t, s) =>
              // local values may have free values defined in
              // their scope. we could handle these with let bindings
              if (scopeMatches(t.freeVarsDup.toSet, s, scope)) evaluate(t, s)
              else None
            case _ => None
          }
        case Let(arg, expr, in, RecursionKind.NonRecursive, _) =>
          evaluate(in, scope.updated(arg, (RecursionKind.NonRecursive, expr, scope)))
        case FnArgs(fn, args) =>
          evaluate(fn, scope).map {
            case EvalResult.Cons(p, c, ahead) => EvalResult.Cons(p, c, ahead ::: args.toList)
            // $COVERAGE-OFF$
            case EvalResult.Constant(c) =>
              // this really shouldn't happen,
              sys.error(s"unreachable: cannot apply a constant: $te => ${fn.repr} => $c")
            // $COVERAGE-ON$
          }
        case Global(pack, cons: Constructor, _, _) => Some(EvalResult.Cons(pack, cons, Nil))
        case Global(pack, n: Bindable, _, _) =>
          scope.getGlobal(pack, n).flatMap {
            case (RecursionKind.NonRecursive, t, s) =>
              // Global values never have free values,
              // so it is safe to substitute into our current scope
              evaluate(t, s)
            case _ => None
          }
        case Generic(_, in) =>
          // if we can evaluate, we are okay
          evaluate(in, scope)
        case Annotation(te, _) =>
          evaluate(te, scope)
        case _ =>
          None
      }

    type Pat = Pattern[(PackageName, Constructor), Type]
    type Branch[A] = (Pat, TypedExpr[A])

    def maybeEvalMatch[A](m: Match[_ <: A], scope: Scope[A]): Option[TypedExpr[A]] =
      evaluate(m.arg, scope).flatMap {
        case EvalResult.Cons(p, c, args) =>

          val alen = args.length

          def isTotal(p: Pat): Boolean =
            p match {
              case Pattern.WildCard | Pattern.Var(_) => true
              case Pattern.Named(_, p) => isTotal(p)
              case Pattern.Annotation(p, _) => isTotal(p)
              case Pattern.Union(h, t) => isTotal(h) || t.exists(isTotal)
              case _ => false
            }

          // The Option signals we can't complete
          def filterPat(pat: Pat): Option[Option[Pat]] =
            pat match {
              case ps@Pattern.PositionalStruct((p0, c0), args0) =>
                if (p0 == p && c0 == c && args0.length == alen) Some(Some(ps))
                else Some(None) // we definitely don't match this branch
              case Pattern.Named(n, p) =>
                filterPat(p).map { p1 =>
                  p1.map { bp => Pattern.Named(n, bp) }
                }
              case Pattern.Annotation(p, _) =>
                // The annotation is only used at inference time, the values have already been typed
                filterPat(p)
              case Pattern.Union(h, t) =>
                (filterPat(h), t.traverse(filterPat))
                  .mapN { (optP1, p2s) =>
                      val flatP2s: List[Pat] = p2s.toList.flatten
                      optP1 match {
                        case None =>
                          flatP2s match {
                            case Nil => None
                            case h :: t => Some(Pattern.union(h, t))
                          }
                        case Some(p1) => Some(Pattern.union(p1, flatP2s))
                      }
                  }
              case Pattern.WildCard | Pattern.Var(_) => Some(Some(pat))
              case Pattern.ListPat(_) =>
                // TODO some of these patterns we could evaluate
                None
              case _ => None
            }

          object MaybeNamedStruct {
            def unapply(p: Pat): Option[(List[Bindable], List[Pat])] =
              p match {
                case Pattern.Named(n, MaybeNamedStruct(ns, pats)) =>
                  Some((n :: ns, pats))
                case Pattern.PositionalStruct(_, pats) =>
                  Some((Nil, pats))
                case Pattern.WildCard => Some((Nil, args.as(Pattern.WildCard)))
                case Pattern.Var(n) => Some((n :: Nil, args.as(Pattern.WildCard)))
                case _ =>
                  None
              }
          }

          m.branches
            .traverse { case (p, r) => filterPat(p).map((_, r)) }
            // if we can check all the branches for a match, maybe we can evaluate
            .flatMap { branches =>
              val candidates: List[(Pat, TypedExpr[A])] =
                branches.collect { case (Some(p), r) => (p, r)}

              candidates match {
                  // $COVERAGE-OFF$
                case Nil =>
                  // TODO hitting this looks like a bug
                  sys.error(s"no branch matched in ${m.repr} matched: $p::$c(${args.map(_.repr)})")
                  // $COVERAGE-ON$
                case (MaybeNamedStruct(b, pats), r) :: rest if rest.isEmpty || pats.forall(isTotal) =>
                  // If there are no more items, or all inner patterns are total, we are done
                  // exactly one matches, this can be a sequential match
                  def matchAll(argPat: List[(TypedExpr[A], Pattern[(PackageName, Constructor), Type])]): TypedExpr[A] =
                    argPat match {
                      case Nil => r
                      case (a, p) :: tail =>
                        val tr = matchAll(tail)
                        p match {
                          case Pattern.WildCard =>
                            // we don't care about this value
                            tr
                          case Pattern.Var(b) =>
                            Let(b, a, tr, RecursionKind.NonRecursive, m.tag)
                          case _ =>
                            // This will get simplified later
                            Match(a, NonEmptyList.one((p, tr)), m.tag)
                        }
                    }

                  val res = matchAll(args.zip(pats))
                  Some(b.foldRight(res)(Let(_, m.arg, _, RecursionKind.NonRecursive, m.tag)))
                case h :: t =>
                  // more than one branch might match, wait till runtime
                  val m1 = Match(m.arg, NonEmptyList(h, t), m.tag)
                  if (m1 == m) None
                  else Some(m1)
              }
            }

        case EvalResult.Constant(li @ Lit.Integer(i)) =>
          def makeLet(p: Pattern[(PackageName, Constructor), Type]): Option[List[Bindable]] =
            p match {
              case Pattern.Named(v, p) =>
                makeLet(p).map(v :: _)
              case Pattern.WildCard => Some(Nil)
              case Pattern.Var(v) => Some(v :: Nil)
              case Pattern.Annotation(p, _) => makeLet(p)
              case Pattern.Literal(Lit.Integer(j)) =>
                if (j == i) Some(Nil)
                else None
              case Pattern.Union(h, t) =>
                (h :: t).toList.iterator.map(makeLet).reduce(_.orElse(_))
              // $COVERAGE-OFF$ this is ill-typed so should be unreachable
              case Pattern.PositionalStruct(_, _) | Pattern.ListPat(_) | Pattern.StrPat(_) | Pattern.Literal(Lit.Str(_)) => None
              // $COVERAGE-ON$
            }

          Foldable[NonEmptyList]
            .collectFirstSome[Branch[A], TypedExpr[A]](m.branches) { case (p, r) =>
              makeLet(p).map { names =>
                val lit = Literal[A](li, Type.getTypeOf(li), m.tag)
                // all these names are bound to the lit
                names.distinct.foldLeft(r) { case (r, n) =>
                  Let(n, lit, r, RecursionKind.NonRecursive, m.tag)
                }
              }
            }
        case EvalResult.Constant(Lit.Str(_)) =>
          None
      }
  }

}
