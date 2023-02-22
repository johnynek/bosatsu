package org.bykn.bosatsu

import Parser.{ Combinators, maybeSpace }
import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser => P}
import org.typelevel.paiges.{ Doc, Document }

import Identifier.{Bindable, Constructor}

case class DefStatement[A, B](
  name: Bindable,
  typeArgs: Option[NonEmptyList[TypeRef.TypeVar]],
  args: NonEmptyList[A],
  retType: Option[TypeRef], result: B) {

  /**
   * This ignores the name completely and just returns the lambda expression here
   */
  def toLambdaExpr[F[_]: Applicative, C](resultExpr: B => F[Expr[C]],
    tag: F[C])(argFn: A => F[Pattern[(PackageName, Constructor), rankn.Type]], trFn: TypeRef => F[rankn.Type]): F[Expr[C]] = {
    val unTypedBody = resultExpr(result)
    val bodyExp =
      retType.fold(unTypedBody) { t =>
        (unTypedBody, trFn(t), tag).mapN(Expr.Annotation(_, _, _))
      }

    (args.traverse(argFn), bodyExp, tag).mapN { (as, b, t) =>
      val lambda = Expr.buildPatternLambda(as, b, t)
      typeArgs match {
        case None => lambda
        case Some(args) =>
          val bs = args.map { case TypeRef.TypeVar(b) => rankn.Type.Var.Bound(b) }
          Expr.Generic(bs, lambda)
      }
    }
  }
}

object DefStatement {
  private[this] val defDoc = Doc.text("def ")
  private[this] val arrow = Doc.text(" -> ")
  private[this] val commaSpace = Doc.text(", ")

  implicit def document[A: Document, B: Document]: Document[DefStatement[A, B]] =
    Document.instance[DefStatement[A, B]] { defs =>
      import defs._
      val res = retType.fold(Doc.empty) { t => arrow + t.toDoc }
      val taDoc = typeArgs match {
        case None => Doc.empty
        case Some(ta) => TypeRef.docTypeArgs(ta.toList)
      }
      val argDoc =
        Doc.char('(') +
          Doc.intercalate(commaSpace, args.map(Document[A].document(_)).toList) +
          Doc.char(')')
      val line0 = defDoc + Document[Bindable].document(name) + taDoc + argDoc + res + Doc.char(':')

      line0 + Document[B].document(result)
    }

  /**
   * The resultTParser should parse some indentation any newlines
   */
  def parser[A, B](argParser: P[A], resultTParser: P[B]): P[DefStatement[A, B]] = {
      val args = argParser.parensLines1Cut
      val result = (P.string("->") *> maybeSpace *> TypeRef.parser).?
      (Parser.keySpace("def") *> (Identifier.bindableParser ~ TypeRef.typeParams.? ~ args) <* maybeSpace,
        result.with1 <* (maybeSpace.with1 ~ P.char(':')),
        resultTParser)
          .mapN { case (((name, tps), args), resType, res) =>
            DefStatement(name, tps, args, resType, res)
          }
    }
}
