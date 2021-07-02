package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._
import org.bykn.bosatsu.rankn.{Type, TypeEnv}

import Identifier.{Bindable, Constructor}

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

    def getLocal(key: Bindable): Option[(RecursionKind, TypedExpr[A], Scope[A])] =
      FixType.unfix[ScopeT[A, *]](scope).get((None, key))

    def getGlobal(pack: PackageName, n: Bindable): Option[(RecursionKind, TypedExpr[A], Scope[A])] =
      FixType.unfix[ScopeT[A, *]](scope).get((Some(pack), n))
  }

  private def nameScope[A](b: Bindable, r: RecursionKind, scope: Scope[A]): (Option[Bindable], Scope[A]) =
    if (r.isRecursive) (Some(b), scope - b)
    else (None, scope)

  def normalizeAll[A](pack: PackageName, lets: List[(Bindable, RecursionKind, TypedExpr[A])], typeEnv: TypeEnv[Variance]): List[(Bindable, RecursionKind, TypedExpr[A])] = {
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

  // if you have made one step of progress, use this to recurse
  // so we don't throw away if we don't progress more
  private def normalize1[A](namerec: Option[Bindable], te: TypedExpr[A], scope: Scope[A], typeEnv: TypeEnv[Variance]): Some[TypedExpr[A]] =
    normalizeLetOpt(namerec, te, scope, typeEnv) match {
      case None => Some(te)
      case s@Some(te) => s
    }

  /**
   * if the te is not in normal form, transform it into normal form
   */
  def normalizeLetOpt[A](namerec: Option[Bindable], te: TypedExpr[A], scope: Scope[A], typeEnv: TypeEnv[Variance]): Option[TypedExpr[A]] =
    te match {
      case Generic(vars, in, tag) =>
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
        val freeVars = vars.toList.filter(frees)
        NonEmptyList.fromList(freeVars) match {
          case None => normalize1(namerec, in, scope, typeEnv)
          case Some(nonEmpty) =>
            normalizeLetOpt(namerec, in, scope, typeEnv) match {
              case None =>
                if (freeVars == vars.toList) None
                else Some(Generic(nonEmpty, in, tag))
              case Some(in1) =>
                Some(Generic(nonEmpty, in1, tag))
            }
        }
      case Annotation(term, tpe, tag) =>
        // if we annotate twice, we can ignore the inner annotation
        // we should have type annotation where we normalize type parameters
        val e1 = normalize1(namerec, term, scope, typeEnv).get
        e1 match {
          case _ if e1.getType == tpe =>
            // the type is already right
            Some(e1)
          case Annotation(t1, _, tag) =>
            Some(Annotation(t1, tpe, tag))
          case notAnn =>
            if (notAnn eq term) None
            else Some(Annotation(notAnn, tpe, tag))
        }

      case AnnotatedLambda(arg, tpe, expr, tag) =>
        // we can normalize the arg to the smallest non-free var
        // \x -> f(x) == f (eta conversion)
        // \x -> generic(g) = generic(\x -> g) if the type of x doesn't have free types with vars
        val e1 = normalize1(None, expr, scope, typeEnv).get
        e1 match {
          case App(fn, Local(ident, _, _), _, _) if ident === arg && fn.notFree(ident) =>
            // if ident is not free in fn we can return fn
            val tetpe = te.getType
            normalize1(None, {
              if (fn.getType == tetpe) fn
              else Annotation(fn, tetpe, te.tag)
            },
            scope,
            typeEnv)
          case Let(arg1, ex, in, rec, tag1) if ex.notFree(arg) =>
            // \x ->
            //   y = z
            //   f(y)
            //same as:
            //y = z
            //\x -> f(y)
            //avoid recomputing y
            //TODO: we could reorder Lets if we have several in a row
            normalize1(None, Let(arg1, ex, AnnotatedLambda(arg, tpe, in, tag), rec, tag1), scope, typeEnv)
          case m@Match(arg1, branches, tag1) if arg1.notFree(arg) =>
            // same as above: if match does not depend on lambda arg, lift it out
              val b1 = branches.traverse { case (p, b) =>
                if (!p.names.contains(arg)) Some((p, AnnotatedLambda(arg, tpe, b, tag)))
                else None
              }
              b1 match {
                case None =>
                  if (m eq expr) None
                  else Some(AnnotatedLambda(arg, tpe, m, tag))
                case Some(bs) =>
                  val m1 = Match(arg1, bs, tag1)
                  normalize1(namerec, m1, scope, typeEnv)
              }
          case notApp =>
            if (notApp eq expr) None
            else Some(AnnotatedLambda(arg, tpe, notApp, tag))
        }
      case Global(_, _, _, _) | Literal(_, _, _) =>
        // these are fundamental
        None
      case Local(_, _, _) =>
        // TODO we could look in the scope
        // and potentially simplify, but maybe it
        // is too late here, we want to do that when
        // we have another potential optimization?
        None
      case App(fn, arg, tpe, tag) =>
        val f1 = normalize1(None, fn, scope, typeEnv).get
        lazy val a1 = normalize1(None, arg, scope, typeEnv).get
        f1 match {
          case AnnotatedLambda(b, ltpe, expr, ltag) =>
            // (\y -> z)(x) = let y = x in z
            val a2 = if (ltpe != arg.getType) Annotation(arg, ltpe, ltag) else arg
            val expr2 = if (tpe != expr.getType) Annotation(expr, tpe, expr.tag) else expr
            val l = Let(b, a2, expr2, RecursionKind.NonRecursive, tag)
            normalize1(namerec, l, scope, typeEnv)
          case Let(arg1, ex, in, rec, tag1) if a1.notFree(arg1) =>
              // (app (let x y z) w) == (let x y (app z w)) if w does not have x free
              normalize1(namerec, Let(arg1, ex, App(in, a1, tpe, tag), rec, tag1), scope, typeEnv)
          case _ =>
            if ((f1 eq fn) && (a1 eq arg)) None
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
            val cnt = in1.freeVarsDup.count(_ === arg)
            if (cnt > 0) {
              // the arg is needed
              def isSimple(ex: TypedExpr[A]): Boolean =
                ex match {
                  case Literal(_, _, _) | Local(_, _, _) | Global(_, _, _, _) => true
                  case Annotation(t, _, _) => isSimple(t)
                  case Generic(_, t, _) => isSimple(t)
                  case AnnotatedLambda(_, _, _, _) =>
                    // always inline lambdas so we can possibly
                    // apply (\x -> f)(g) => let x = g in f
                    true
                  case _ => false
                }
              val shouldInline = (!rec.isRecursive) && {
                (cnt == 1) || isSimple(ex1)
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
          normalizeLetOpt(None, e, shadows.foldLeft(scope)(_ - _), typeEnv) match {
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
          Impl.maybeEvalMatch(m1, scope, typeEnv) match {
            case None =>
              // if only the arg changes, there
              // is no need to rerun the normalization
              // because normalization of branches
              // does not depend on the arg
              if (a1 eq arg) None
              else Some(m1)
            case Some(m2) =>
              // we can possibly simplify this now:
              normalize1(namerec, m2, scope, typeEnv)
          }
        }
        else {
          // there has been some change, so
          // see if that unlocked any new changes
          normalize1(namerec, Match(a1, branches1a, tag), scope, typeEnv)
        }
    }

  def normalize[A](te: TypedExpr[A]): Option[TypedExpr[A]] =
    normalizeLetOpt(None, te, emptyScope, TypeEnv.empty)

  private object Impl {
    sealed abstract class EvalResult[A]
    object EvalResult {
      case class Cons[A](pack: PackageName, cons: Constructor, args: List[TypedExpr[A]]) extends EvalResult[A]
      case class Constant[A](lit: Lit) extends EvalResult[A]
    }

    object FnArgs {
      def unapply[A](te: TypedExpr[A]): Option[(TypedExpr[A], NonEmptyList[TypedExpr[A]])] =
        te match {
          case App(fn, arg, _, _) =>
            unapply(fn) match {
              case None =>
                Some((fn, NonEmptyList(arg, Nil)))
              case Some((fn0, args)) =>
                Some((fn0, args :+ arg))
            }
          case notApp => None
        }
    }

    def evaluate[A](te: TypedExpr[A], scope: Scope[A]): Option[EvalResult[A]] =
      te match {
        case Literal(lit, _, _) => Some(EvalResult.Constant(lit))
        case Local(b, _, _) =>
          scope.getLocal(b).flatMap {
            case (_, t, s) =>
              evaluate(t, s)
          }
        case Let(arg, expr, in, RecursionKind.NonRecursive, _) =>
          evaluate(in, scope.updated(arg, (RecursionKind.NonRecursive, expr, scope)))
        case FnArgs(fn, args) =>
          evaluate(fn, scope).map {
            case EvalResult.Cons(p, c, ahead) => EvalResult.Cons(p, c, ahead ::: args.toList)
            case EvalResult.Constant(c) =>
              // this really shouldn't happen,
              // $COVERAGE-OFF$
              sys.error(s"unreachable: cannot apply a constant: $te => ${fn.repr} => $c")
              // $COVERAGE-ON$
          }
        case Global(pack, cons: Constructor, _, _) => Some(EvalResult.Cons(pack, cons, Nil))
        case Global(pack, n: Bindable, _, _) =>
          scope.getGlobal(pack, n).flatMap {
            case (_, t, s) =>
              evaluate(t, s)
          }
        case Generic(_, in, _) =>
          // if we can evaluate, we are okay
          evaluate(in, scope)
        case Annotation(te, _, _) =>
          evaluate(te, scope)
        case _ =>
          None
      }

    
    def maybeEvalMatch[A](m: Match[_ <: A], scope: Scope[A], typeEnv: TypeEnv[Variance]): Option[TypedExpr[A]] =
      evaluate(m.arg, scope).flatMap {
        case EvalResult.Cons(p, c, args) =>

          val alen = args.length
          type Branch = (Pattern[(PackageName, Constructor), Type], TypedExpr[A])

          def isTotal(p: Pattern[(PackageName, Constructor), Type]): Boolean =
            p match {
              case Pattern.WildCard | Pattern.Var(_) => true
              case Pattern.Named(_, p) => isTotal(p)
              case _ => false
            }

          // The Option signals we can't complete
          def expandMatches(br: Branch): Option[List[Branch]] =
            br match {
              case (ps@Pattern.PositionalStruct((p0, c0), args0), res) =>
                if (p0 == p && c0 == c && args0.length == alen) Some((ps, res) :: Nil)
                else Some(Nil)
              case (Pattern.Union(h, t), r) =>
                (h :: t.toList).traverse { p => expandMatches((p, r)) }.map(_.flatten)
              case br@(p, _) if isTotal(p) => Some(br :: Nil)
              case (Pattern.ListPat(_), _) =>
                // TODO some of these patterns we could evaluate
                None
              case _ => None
            }

          m.branches.toList.traverse(expandMatches).map(_.flatten).flatMap {
            case Nil =>
              // $COVERAGE-OFF$
              sys.error(s"no branch matched in ${m.repr} matched: $p::$c(${args.map(_.repr)})")
              // $COVERAGE-ON$
            case (Pattern.PositionalStruct(_, pats), r) :: Nil =>
              // exactly one matches, this can be a sequential match
              def matchAll(argPat: List[(TypedExpr[A], Pattern[(PackageName, Constructor), Type])]): TypedExpr[A] =
                argPat match {
                  case Nil => r
                  case (a, p) :: tail =>
                    val tr = matchAll(tail)
                    // This will get simplified later
                    Match(a, NonEmptyList((p, tr), Nil), m.tag)
                }

              Some(matchAll(args.zip(pats)))
            case h :: t =>
              // more than one branch might match, wait till runtime
              val m1 = Match(m.arg, NonEmptyList(h, t), m.tag)
              if (m1 == m) None
              else Some(m1)
          }

        case EvalResult.Constant(lit) =>
          // TODO we can evaluate literal matches at compile time
          None
      }
  }

}
