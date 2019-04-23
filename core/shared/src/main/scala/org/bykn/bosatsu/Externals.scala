package org.bykn.bosatsu

import cats.Eval
import fastparse.all._

import cats.implicits._

import Evaluation.Value

sealed abstract class FfiCall {
  def call(t: rankn.Type): Eval[Value]
}

object FfiCall {
  final case class Fn1(fn: Value => Value) extends FfiCall {
    import Value.FnValue

    private[this] val evalFn: Eval[FnValue] =
      Eval.now(FnValue { e1 => Eval.defer(e1.map(fn)) })

    def call(t: rankn.Type): Eval[Value] = evalFn
  }
  final case class Fn2(fn: (Value, Value) => Value) extends FfiCall {
    import Value.FnValue

    private[this] val evalFn: Eval[FnValue] =
      Eval.now(FnValue { e1 =>
        Eval.now(FnValue { e2 =>
          Eval.defer((e1, e2).mapN(fn(_, _)))
        })
      })

    def call(t: rankn.Type): Eval[Value] = evalFn
  }
  final case class Fn3(fn: (Value, Value, Value) => Value) extends FfiCall {
    import Value.FnValue

    private[this] val evalFn: Eval[FnValue] =
      Eval.now(FnValue { e1 =>
        Eval.now(FnValue { e2 =>
          Eval.now(FnValue { e3 =>
            Eval.defer((e1, e2, e3).mapN(fn(_, _, _)))
          })
        })
      })

    def call(t: rankn.Type): Eval[Value] = evalFn
  }

  sealed abstract class Reflective extends FfiCall {
    def call(t: rankn.Type): Eval[Evaluation.Value] = {
      def breakDots(m: String): List[String] =
        m.split("\\.", -1).toList

      def defaultClassName(parts: List[String]): String =
        parts.init.mkString(".")

      val (parts, clsName, instFn) =
        this match {
          case ScalaCall(m) =>
            val parts = breakDots(m)
            val clsName = defaultClassName(parts) + "$"
            (parts, clsName, { c: Class[_] =>
              c.getDeclaredField("MODULE$").get(null)
            })
          case JavaCall(m) =>
            val parts = breakDots(m)
            val clsName = defaultClassName(parts)
            (parts, clsName, { c: Class[_] => null})
        }

      Eval.later {
        val cls = Class.forName(clsName)
        val args = getJavaType(t).toArray
        val m = cls.getMethod(parts.last, args.init :_*)
        val inst = instFn(cls)

        def invoke(tpe: rankn.Type, args: List[Value]): Value =
          tpe match {
            case rankn.Type.ForAll(_, t) => invoke(t, args)
            case rankn.Type.Fun(a, tail) =>
              Value.FnValue { ex =>
                ex.map { x => invoke(tail, x :: args) }
              }
            case _ =>
              m.invoke(inst, args.reverse.toArray: _*).asInstanceOf[Value]
          }

        invoke(t, Nil)
      }
    }
  }

  final case class ScalaCall(methodName: String) extends Reflective
  final case class JavaCall(methodName: String) extends Reflective

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
        case rankn.Type.Fun(a, b) if top =>
          loop(a, false) match {
            case at :: Nil => at :: loop(b, top)
            case function => sys.error(s"unsupported function type $function in $t")
          }
        case rankn.Type.ForAll(_, t) =>
          loop(t, top)
        case _ => classOf[Evaluation.Value] :: Nil
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
