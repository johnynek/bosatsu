package org.bykn.bosatsu.codegen.clang

import cats.{Monad, Traverse}
import cats.data.NonEmptyList
import org.bykn.bosatsu.PackageName
import org.bykn.bosatsu.Matchless
import org.bykn.bosatsu.Matchless.Expr
import org.bykn.bosatsu.Identifier.Bindable
import org.typelevel.paiges.Doc

import cats.syntax.all._

object ClangGen {
  sealed abstract class Error
  object Error {
    case class UnknownValue(pack: PackageName, value: Bindable) extends Error
    case class InvariantViolation(message: String, expr: Expr) extends Error
  }

  def renderMain(
      sortedEnv: Vector[NonEmptyList[(PackageName, List[(Bindable, Expr)])]],
      externals: Map[(PackageName, Bindable), (Code.Include, Code.Ident)],
      value: (PackageName, Bindable),
      evaluator: (Code.Include, Code.Ident)
  ): Either[Error, Doc] = {
    val env = Impl.Env.impl
    import env._

    val trav2 = Traverse[Vector].compose[NonEmptyList]

    val res =
      trav2.traverse_(sortedEnv) { case (pn, values) =>
        values.traverse_ { case (bindable, expr) =>
          renderTop(pn, bindable, expr)      
        }
      } *> env.renderMain(value._1, value._2, evaluator._1, evaluator._2)

    val allValues: Impl.AllValues =
      sortedEnv
        .iterator.flatMap(_.iterator)
        .toMap
    
    run(allValues, externals, res)
  }

  private object Impl {
    type AllValues = Map[PackageName, List[(Bindable, Expr)]]
    type Externals = Map[(PackageName, Bindable), (Code.Include, Code.Ident)]

    trait Env {
      import Matchless._

      type T[A]
      implicit val monadImpl: Monad[T]
      def run(pm: AllValues, externals: Externals, t: T[Unit]): Either[Error, Doc]
      def appendStatement(stmt: Code.Statement): T[Unit]
      def error[A](e: Error): T[A]
      def getBinding(bn: Bindable): T[Code.Ident]
      def getGlobal(pn: PackageName, bn: Bindable): T[Code.Ident]
      def bind[A](bn: Bindable)(in: T[A]): T[A]
      // a recursive function needs to remap the Bindable to the top-level mangling
      def recursiveName[A](pack: PackageName, bn: Bindable)(in: T[A]): T[A]
      // used for temporary variables of type BValue
      def newLocalName: T[Code.Ident]
      // record that this name is a top level function, so applying it can be direct
      def isTopLevelFn(p: PackageName, bn: Bindable): T[Unit]

      /////////////////////////////////////////
      // the below are independent of the environment implementation
      /////////////////////////////////////////
    
      // assign any results to result and set the condition to false
      // and replace any calls to nm(args) with assigning args to those values
      def toWhileBody(nm: Bindable, args: NonEmptyList[Bindable], cond: Code.Ident, result: Code.Ident, body: Code.ValueLike): T[Code.Block] =
        ???

      def bindAll[A](nel: NonEmptyList[Bindable])(in: T[A]): T[A] =
        bind(nel.head) {
          NonEmptyList.fromList(nel.tail) match {
            case None => in
            case Some(rest) => bindAll(rest)(in)
          }
        }

      def innerToValue(expr: Expr): T[Code.ValueLike] = ???

      def topFn(p: PackageName, b: Bindable, fn: FnExpr): T[Code.Statement] =
        isTopLevelFn(p, b) *> (fn match {
          case Lambda(_ :: _, _, _, _) | LoopFn(_ :: _, _, _, _) =>
            error(Error.InvariantViolation(s"unexpected top level captures: $p, $b", fn))
          case Lambda(Nil, name, args, expr) =>
            val body = bindAll(args) { innerToValue(expr).map(Code.returnValue(_)) }
            name match {
              case None => body
              case Some(rec) => recursiveName(p, rec)(body)
            }
          case LoopFn(Nil, nm, args, body) =>
            recursiveName(p, nm) {
              bindAll(args) {
                for {
                  cond <- newLocalName
                  res <- newLocalName
                  bodyVL <- innerToValue(body)
                  whileBody <- toWhileBody(nm, args, cond = cond, result = res, body = bodyVL)
                  fnName <- getGlobal(p, nm)
                  argParams <- args.traverse { b =>
                    getBinding(b).map { i => Code.Param(Code.TypeIdent.BValue, i) }
                  }
                  fnBody = Code.block(
                    Code.DeclareVar(Nil, Code.TypeIdent.Int, cond, Some(Code.IntLiteral.One)),
                    Code.DeclareVar(Nil, Code.TypeIdent.BValue, res, None),
                    Code.While(cond, whileBody),
                    Code.Return(Some(res))
                  )
                } yield Code.DeclareFn(Nil, Code.TypeIdent.BValue, fnName, argParams.toList, Some(fnBody))
              }
            }
        })

      def topStatement(p: PackageName, b: Bindable, expr: Expr): T[Code.Statement] =
        expr match {
          case fn: FnExpr => topFn(p, b, fn)
          case Global(_, _) => ???
          case Always(_, _) => ???
          case App(_, _) => ???
          case ClosureSlot(_) => ???
          case GetEnumElement(_, _, _, _) => ???
          case GetStructElement(_, _, _) => ???
          case If(_, _, _) => ???
          case Let(_, _, _) => ???
          case LetMut(_, _) => ???
          case Literal(_) => ???
          case Local(_) => ???
          case LocalAnon(_) => ???
          case LocalAnonMut(_) => ???
          case MakeEnum(_, _, _) => ???
          case MakeStruct(_) => ???
          case PrevNat(_) => ???
          case SuccNat => ???
          case ZeroNat => ???
        }

      def renderTop(p: PackageName, b: Bindable, expr: Expr): T[Unit] =
        topStatement(p, b, expr).flatMap { stmt =>
          appendStatement(stmt)
        }

      def renderMain(p: PackageName, b: Bindable, evalInc: Code.Include, evalFn: Code.Ident): T[Unit]
    }

    object Env {
      def impl: Env = ???
    }
  }
}