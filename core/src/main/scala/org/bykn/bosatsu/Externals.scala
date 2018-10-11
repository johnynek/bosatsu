package org.bykn.bosatsu

import cats.Eval
import fastparse.all._

sealed abstract class FfiCall {
  def call(t: rankn.Type): Eval[Any] = {
    def breakDots(m: String): List[String] =
      m.split("\\.", -1).toList

    def defaultClassName(parts: List[String]): String =
      parts.init.mkString(".")

    val (parts, clsName, instFn) =
      this match {
        case FfiCall.ScalaCall(m) =>
          val parts = breakDots(m)
          val clsName = defaultClassName(parts) + "$"
          (parts, clsName, { c: Class[_] =>
            c.getDeclaredField("MODULE$").get(null)
          })
        case FfiCall.JavaCall(m) =>
          val parts = breakDots(m)
          val clsName = defaultClassName(parts)
          (parts, clsName, { c: Class[_] => null})
      }

    Eval.later {
      val cls = Class.forName(clsName)
      val args = FfiCall.getJavaType(t).toArray
      val m = cls.getMethod(parts.last, args.init :_*)
      val inst = instFn(cls)

      def invoke(tpe: rankn.Type, args: List[Any]): Any =
        tpe match {
          case rankn.Type.ForAll(_, t) => invoke(t, args)
          case rankn.Type.Fun(a, tail) =>
            new Fn[Any, Any] {
              val tpeStr = TypeRef.fromType(tpe).get.toDoc.render(80)
              override def toString = s"$tpeStr, args: $args"
              def apply(x: Any) = {
                invoke(tail, x :: args)
              }
            }
          case _ =>
            m.invoke(inst, args.reverse.toArray.asInstanceOf[Array[AnyRef]]: _*)
        }

      invoke(t, Nil)
    }
  }
}

object FfiCall {
  case class ScalaCall(methodName: String) extends FfiCall
  case class JavaCall(methodName: String) extends FfiCall

  val parser: P[FfiCall] = {
    val whitespace = Set(' ', '\t', '\n')
    val rest = Parser.spaces ~/ P(CharsWhile { c => !whitespace(c) }.!)
    val lang = P("scala").map(_ => ScalaCall(_)) |
      P("java").map(_ => JavaCall(_))

    (lang ~ rest).map { case (l, m) => l(m) }
  }

  def getJavaType(t: rankn.Type): List[Class[_]] = {
    def loop(t: rankn.Type, top: Boolean): List[Class[_]] = {
      t match {
        //case t if t == rankn.Type.IntType => classOf[java.lang.Integer] :: Nil
        //case t if t == rankn.Type.BoolType => classOf[java.lang.Boolean] :: Nil
        case rankn.Type.Fun(a, b) if top =>
          loop(a, false) match {
            case at :: Nil => at :: loop(b, top)
            case function => sys.error(s"unsupported function type $function in $t")
          }
        case rankn.Type.ForAll(_, t) =>
          loop(t, top)
        case _ => classOf[AnyRef] :: Nil
      }
    }
    loop(t, true)
  }
}

case class Externals(toMap: Map[(PackageName, String), FfiCall]) {
  def add(pn: PackageName, value: String, f: FfiCall): Externals =
    Externals(toMap + ((pn, value) -> f))

  def ++(that: Externals): Externals =
    Externals(toMap ++ that.toMap)
}

object Externals {
  def empty: Externals = Externals(Map.empty)

  val parser: P[Externals] = {
    val comment = CommentStatement.commentPart
    val row = PackageName.parser ~ Parser.spaces ~/ Parser.lowerIdent ~ Parser.spaces ~ FfiCall.parser ~ Parser.toEOL

    val optRow = (comment | Parser.toEOL).map(_ => None) | row.map(Some(_))

    optRow.rep().map { rows =>
      Externals(rows.collect { case Some((p, v, ffi)) => ((p, v), ffi) }.toMap)
    }
  }
}
