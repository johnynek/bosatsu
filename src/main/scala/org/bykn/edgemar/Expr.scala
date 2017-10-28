package org.bykn.edgemar

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.implicits._
import cats.{Applicative, Eval, Traverse}

sealed abstract class Lit
object Lit {
  case class Integer(toInt: Int) extends Lit
  case class Bool(toBool: Boolean) extends Lit
}

sealed abstract class Expr[T] {
  def tag: T
}

object Expr {
  case class Var[T](name: String, tag: T) extends Expr[T]
  case class App[T](fn: Expr[T], arg: Expr[T], tag: T) extends Expr[T]
  case class Lambda[T](arg: String, expr: Expr[T], tag: T) extends Expr[T]
  case class Ffi[T](lang: String, callsite: String, scheme: Scheme, tag: T) extends Expr[T]
  case class Let[T](arg: String, expr: Expr[T], in: Expr[T], tag: T) extends Expr[T]
  case class Literal[T](lit: Lit, tag: T) extends Expr[T]
  case class If[T](arg: Expr[T], ifTrue: Expr[T], ifFalse: Expr[T], tag: T) extends Expr[T]
  case class Op[T](left: Expr[T], binOp: Operator, right: Expr[T], tag: T) extends Expr[T]

  /**
   * Return a value so next(e).tag == e and also this is true
   * recursively
   */
  def nest[T](e: Expr[T]): Expr[Expr[T]] =
    e match {
      case Var(s, _) =>
        Var(s, e)
      case App(fn, a, _) =>
        App(nest(fn), nest(a), e)
      case Lambda(arg, expr, _) =>
        Lambda(arg, nest(expr), e)
      case Ffi(lang, call, scheme, _) =>
        Ffi(lang, call, scheme, e)
      case Let(arg, exp, in, _) =>
        Let(arg, nest(exp), nest(in), e)
      case Literal(lit, _) =>
        Literal(lit, e)
      case If(arg, ifTrue, ifFalse, _) =>
        If(nest(arg), nest(ifTrue), nest(ifFalse), e)
      case Op(left, op, right, _) =>
        Op(nest(left), op, nest(right), e)
    }

  implicit val exprTraverse: Traverse[Expr] =
    new Traverse[Expr] {
      def traverse[G[_]: Applicative, A, B](fa: Expr[A])(f: A => G[B]): G[Expr[B]] =
        fa match {
          case Var(s, t) =>
            f(t).map(Var(s, _))
          case App(fn, a, t) =>
            (fn.traverse(f), a.traverse(f), f(t)).mapN { (fn1, a1, b) =>
              App(fn1, a1, b)
            }
          case Lambda(arg, expr, t) =>
            (expr.traverse(f), f(t)).mapN { (e1, t1) =>
              Lambda(arg, e1, t1)
            }
          case Ffi(lang, call, scheme, tag) =>
            f(tag).map(Ffi(lang, call, scheme, _))
          case Let(arg, exp, in, tag) =>
            (exp.traverse(f), in.traverse(f), f(tag)).mapN { (e1, i1, t1) =>
              Let(arg, e1, i1, t1)
            }
          case Literal(lit, tag) =>
            f(tag).map(Literal(lit, _))
          case If(arg, ifTrue, ifFalse, tag) =>
            (arg.traverse(f), ifTrue.traverse(f), ifFalse.traverse(f), f(tag)).mapN { (a1, t1, f1, tag1) =>
              If(a1, t1, f1, tag1)
            }
          case Op(left, op, right, tag) =>
            (left.traverse(f), right.traverse(f), f(tag)).mapN { (l, r, t) =>
              Op(l, op, r, t)
            }
        }

      def foldLeft[A, B](fa: Expr[A], b: B)(f: (B, A) => B): B =
        fa match {
          case Var(_, tag) => f(b, tag)
          case App(fn, a, tag) =>
            val b1 = foldLeft(fn, b)(f)
            val b2 = foldLeft(a, b1)(f)
            f(b2, tag)
          case Lambda(_, expr, tag) =>
            val b1 = foldLeft(expr, b)(f)
            f(b1, tag)
          case Ffi(_, _, _, tag) =>
            f(b, tag)
          case Let(_, exp, in, tag) =>
            val b1 = foldLeft(exp, b)(f)
            val b2 = foldLeft(in, b1)(f)
            f(b2, tag)
          case Literal(_, tag) =>
            f(b, tag)
          case If(arg, ifTrue, ifFalse, tag) =>
            val b1 = foldLeft(arg, b)(f)
            val b2 = foldLeft(ifTrue, b1)(f)
            val b3 = foldLeft(ifFalse, b2)(f)
            f(b3, tag)
          case Op(left, _, right, tag) =>
            val b1 = foldLeft(left, b)(f)
            val b2 = foldLeft(right, b1)(f)
            f(b2, tag)
        }

      def foldRight[A, B](fa: Expr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case Var(_, tag) => f(tag, lb)
          case App(fn, a, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(a, b1)(f)
            foldRight(fn, b2)(f)
          case Lambda(_, expr, tag) =>
            val b1 = f(tag, lb)
            foldRight(expr, b1)(f)
          case Ffi(_, _, _, tag) =>
            f(tag, lb)
          case Let(_, exp, in, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(in, b1)(f)
            foldRight(exp, b2)(f)
          case Literal(_, tag) =>
            f(tag, lb)
          case If(arg, ifTrue, ifFalse, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(ifFalse, b1)(f)
            val b3 = foldRight(ifTrue, b2)(f)
            foldRight(arg, b3)(f)
          case Op(left, _, right, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(right, b1)(f)
            foldRight(left, b2)(f)
        }
    }

  def evaluate[T](e: Expr[T]): Either[TypeError, (Any, Scheme)] =
    Inference.inferExpr(e).map { scheme =>
      // if we type check, we can always evaluate
      (evaluateUnsafe(e, Map.empty), scheme)
    }

  private def getJavaType(t: Type): List[Class[_]] =
    t match {
      case Type.Primitive("Int") => classOf[java.lang.Integer] :: Nil
      case Type.Primitive("Bool") => classOf[java.lang.Boolean] :: Nil
      case Type.Arrow(a, b) =>
        getJavaType(a) match {
          case at :: Nil => at :: getJavaType(b)
          case function => sys.error(s"unsupported function type $function in $t")
        }
      case t => sys.error(s"unsupported java ffi type: $t")
    }

  private def evaluateUnsafe[T](e: Expr[T], env: Map[String, Any]): Any =
    e match {
      case Var(v, _) => env(v)
      case App(Lambda(name, fn, _), arg, _) =>
        val env1 = env + (name -> evaluateUnsafe(arg, env))
        evaluateUnsafe(fn, env1)
      case App(fn, arg, _) =>
        evaluateUnsafe(fn, env).asInstanceOf[Any => Any](evaluateUnsafe(arg, env))
      case Lambda(name, expr, _) =>
        { x: Any => evaluateUnsafe(expr, env + (name -> x)) }
      case Ffi(lang, callsite, Scheme(_, t), _) =>
        val parts = callsite.split("\\.", -1).toList
        val clsName0 = parts.init.mkString(".")
        val clsName = lang match {
          case "java" => clsName0
          case "scala" => clsName0 + "$"
          case _ => sys.error(s"unknown lang: $lang")
        }
        val cls = Class.forName(clsName)
        val args = getJavaType(t).toArray
        val m = cls.getMethod(parts.last, args.init :_*)
        val inst = lang match {
          case "java" =>
            null
          case "scala" =>
            cls.getDeclaredField("MODULE$").get(null)
          case _ => ???
        }

        def invoke(tpe: Type, args: List[AnyRef]): Any =
          tpe match {
            case Type.Primitive(_) =>
              m.invoke(inst, args.reverse.toArray: _*)
            case Type.Var(_) =>
              m.invoke(inst, args.reverse.toArray: _*)
            case Type.Arrow(_, tail) => { x: Any =>
              invoke(tail, (x.asInstanceOf[AnyRef]) :: args)
            }
            case unsupported =>
              sys.error(s"unsupported ffi type $unsupported")
          }
        invoke(t, Nil)
      case Let(arg, e, in, _) =>
        evaluateUnsafe(in, env + (arg -> evaluateUnsafe(e, env)))
      case Literal(Lit.Integer(i), _) => i
      case Literal(Lit.Bool(b), _) => b
      case If(arg, t, f, _) =>
        if (evaluateUnsafe(arg, env).asInstanceOf[Boolean]) evaluateUnsafe(t, env)
        else evaluateUnsafe(f, env)
      case Op(a, op, b, _) =>
        val ai = evaluateUnsafe(a, env).asInstanceOf[Int]
        val bi = evaluateUnsafe(b, env).asInstanceOf[Int]
        import Operator._
        op match {
          case Plus => ai + bi
          case Mul => ai * bi
          case Sub => ai - bi
          case Eql => ai == bi
        }
    }
}
