package org.bykn.edgemar

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.implicits._

sealed abstract class Lit
object Lit {
  case class Integer(toInt: Int) extends Lit
  case class Bool(toBool: Boolean) extends Lit
}

sealed abstract class Operator
object Operator {
  case object Plus extends Operator
  case object Sub extends Operator
  case object Mul extends Operator
  case object Eql extends Operator

  def typeOf(o: Operator): Type =
    o match {
      case Eql => Type.Arrow(Type.intT, Type.Arrow(Type.intT, Type.boolT))
      case _ => Type.Arrow(Type.intT, Type.Arrow(Type.intT, Type.intT))
    }
}

sealed abstract class Expr
object Expr {
  case class Var(name: String) extends Expr
  case class App(fn: Expr, arg: Expr) extends Expr
  case class Lambda(arg: String, expr: Expr) extends Expr
  case class Ffi(lang: String, callsite: String, scheme: Scheme) extends Expr
  case class Let(arg: String, expr: Expr, in: Expr) extends Expr
  case class Literal(lit: Lit) extends Expr
  case class If(arg: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr
  case class Op(left: Expr, binOp: Operator, right: Expr) extends Expr

  def evaluate(e: Expr): Either[TypeError, (Any, Scheme)] =
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

  private def evaluateUnsafe(e: Expr, env: Map[String, Any]): Any =
    e match {
      case Var(v) => env(v)
      case App(Lambda(name, fn), arg) =>
        val env1 = env + (name -> evaluateUnsafe(arg, env))
        evaluateUnsafe(fn, env1)
      case App(fn, arg) =>
        evaluateUnsafe(fn, env).asInstanceOf[Any => Any](evaluateUnsafe(arg, env))
      case Lambda(name, expr) =>
        { x: Any => evaluateUnsafe(expr, env + (name -> x)) }
      case Ffi(lang, callsite, Scheme(_, t)) =>
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
      case Let(arg, e, in) =>
        evaluateUnsafe(in, env + (arg -> evaluateUnsafe(e, env)))
      case Literal(Lit.Integer(i)) => i
      case Literal(Lit.Bool(b)) => b
      case If(arg, t, f) =>
        if (evaluateUnsafe(arg, env).asInstanceOf[Boolean]) evaluateUnsafe(t, env)
        else evaluateUnsafe(f, env)
      case Op(a, op, b) =>
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

case class Program(lets: List[(String, Expr)], result: Expr)
