package org.bykn.bosatsu

import cats.Applicative
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.implicits._
import org.bykn.bosatsu.rankn.Type
import scala.collection.immutable.SortedSet
import scala.util.hashing.MurmurHash3

import Identifier.{Bindable, Constructor}

sealed abstract class TypedExpr[T] { self: Product =>
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
  def getType: Type =
    this match {
      case Generic(params, expr, _) =>
        Type.forAll(params.toList, expr.getType)
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

  def repr: String = {
    def rept(t: Type): String =
      TypeRef.fromTypes(None, t :: Nil)(t).toDoc.renderWideStream.mkString

    this match {
      case Generic(params, expr, _) =>
        val pstr = params.toList.map(_.name).mkString(",")
        s"(generic [$pstr] ${expr.repr})"
      case Annotation(expr, tpe, _) =>
        s"(ann ${rept(tpe)} ${expr.repr})"
      case a@AnnotatedLambda(arg, tpe, res, _) =>
        s"(lambda $arg ${rept(tpe)} ${res.repr})"
      case Var(p, v, tpe, _) =>
        s"(var $p $v ${rept(tpe)})"
      case App(fn, arg, tpe, _) =>
        s"(ap ${fn.repr} ${arg.repr} ${rept(tpe)})"
      case Let(n, b, in, rec, _) =>
        val nm = if (rec.isRecursive) "letrec" else "let"
        s"($nm $n ${b.repr} ${in.repr})"
      case Literal(v, tpe, _) =>
        s"(lit ${v.repr} ${rept(tpe)})"
      case Match(arg, branches, _) =>
        // TODO print the pattern
        val bstr = branches.toList.map { case (_, t) => t.repr }.mkString
        s"(match ${arg.repr} $bstr)"
    }
  }

  def traverseType[F[_]: Applicative](fn: Type => F[Type]): F[TypedExpr[T]] =
    this match {
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

  def updatedTag(t: T): TypedExpr[T] =
    this match {
      case g@Generic(_, _, _) => g.copy(tag=t)
      case a@Annotation(_, _, _) => a.copy(tag=t)
      case al@AnnotatedLambda(_, _, _, _) => al.copy(tag=t)
      case v@Var(_, _, _, _) => v.copy(tag=t)
      case a@App(_, _, _, _) => a.copy(tag=t)
      case let@Let(_, _, _, _, _) => let.copy(tag=t)
      case lit@Literal(_, _, _) => lit.copy(tag=t)
      case m@Match(_, _, _) => m.copy(tag=t)
    }
}

object TypedExpr {

  type Rho[A] = TypedExpr[A] // an expression with a Rho type (no top level forall)
  /**
   * This says that the resulting term is generic on a given param
   *
   * The paper says to add TyLam and TyApp nodes, but it never mentions what to do with them
   */
  case class Generic[T](typeVars: NonEmptyList[Type.Var.Bound], in: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Annotation[T](term: TypedExpr[T], coerce: Type, tag: T) extends TypedExpr[T]
  case class AnnotatedLambda[T](arg: Bindable, tpe: Type, expr: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Var[T](pack: Option[PackageName], name: Identifier, tpe: Type, tag: T) extends TypedExpr[T]
  case class App[T](fn: TypedExpr[T], arg: TypedExpr[T], result: Type, tag: T) extends TypedExpr[T]
  case class Let[T](arg: Bindable, expr: TypedExpr[T], in: TypedExpr[T], recursive: RecursionKind, tag: T) extends TypedExpr[T]
  case class Literal[T](lit: Lit, tpe: Type, tag: T) extends TypedExpr[T]
  case class Match[T](arg: TypedExpr[T], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])], tag: T) extends TypedExpr[T]

  type Coerce = FunctionK[TypedExpr, TypedExpr]
  def coerceRho(tpe: Type.Rho): Coerce =
    new FunctionK[TypedExpr, TypedExpr] { self =>
      def apply[A](expr: TypedExpr[A]) =
        expr match {
          case Annotation(t, _, _) => self(t)
          case Generic(params, expr, tag) =>
            // This definitely feels wrong,
            // but without this, I don't see what else we can do
            Generic(params, self(expr), tag)
          case Var(p, name, _, t) => Var(p, name, tpe, t)
          case AnnotatedLambda(arg, argT, res, tag) =>
            // only some coercions would make sense here
            // how to handle?
            // one way out could be to return a type to Annotation
            // and just wrap it in this case, could it be that simple?
            Annotation(expr, tpe, expr.tag)
          case App(fn, arg, _, tag) =>
            // do we need to coerce fn into the right shape?
            App(fn, arg, tpe, tag)
          case Let(arg, argE, in, rec, tag) =>
            Let(arg, argE, self(in), rec, tag)
          case Literal(l, _, tag) => Literal(l, tpe, tag)
          case Match(arg, branches, tag) =>
            Match(arg, branches.map { case (p, expr) => (p, self(expr)) }, tag)
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
        case Let(arg, argE, in, _, _) :: tail =>
          val acc1 = cheat(in, bound + arg, acc)
          go(argE :: tail, bound, acc1)
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
   * TODO this seems pretty expensive to blindly apply: we are deoptimizing
   * the nodes pretty heavily
   */
  def coerceFn(arg: Type, result: Type.Rho, coarg: Coerce, cores: Coerce): Coerce =
    new FunctionK[TypedExpr, TypedExpr] { self =>
      def apply[A](expr: TypedExpr[A]) = {
        /*
         * We have to be careful not to collide with the free vars in expr
         */
        val free = SortedSet(freeVars(expr :: Nil): _*)
        val name = Type.allBinders.iterator.map { v => Identifier.Name(v.name) }.filterNot(free).next
        AnnotatedLambda(name, arg, cores(App(expr, coarg(Var(None, name, arg, expr.tag)), result, expr.tag)), expr.tag)
      }
    }


  def forAll[A](params: NonEmptyList[Type.Var.Bound], expr: TypedExpr[A]): TypedExpr[A] =
    Generic(params, expr, expr.tag)

  implicit def typedExprHasRegion[T: HasRegion]: HasRegion[TypedExpr[T]] =
    HasRegion.instance[TypedExpr[T]] { e => HasRegion.region(e.tag) }
}
