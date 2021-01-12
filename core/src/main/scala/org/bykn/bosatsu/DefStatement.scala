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
  args: List[A],
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
    NonEmptyList.fromList(args) match {
      case None => bodyExp
      case Some(neargs) =>
        (neargs.traverse(argFn), bodyExp, tag).mapN { (as, b, t) =>
          Expr.buildPatternLambda(as, b, t)
        }
    }
  }
}

object DefStatement {
  private[this] val defDoc = Doc.text("def ")

  implicit def document[A: Document, B: Document]: Document[DefStatement[A, B]] =
    Document.instance[DefStatement[A, B]] { defs =>
      import defs._
      val res = retType.fold(Doc.empty) { t => Doc.text(" -> ") + t.toDoc }
      val argDoc =
        if (args.isEmpty) Doc.empty
        else {
          val docA = Document[A]
          Doc.char('(') +
            Doc.intercalate(Doc.text(", "), args.map(docA.document(_))) +
            Doc.char(')')
        }
      val line0 = defDoc + Document[Bindable].document(name) + argDoc + res + Doc.text(":")

      line0 + Document[B].document(result)
    }

    /**
     * The resultTParser should parse some indentation any newlines
     */
    def parser[A, B](argParser: P[A], resultTParser: P[B]): P[DefStatement[A, B]] = {
      val args = argParser.parensLines1Cut
      val result = (P.string("->") *> maybeSpace *> TypeRef.parser).?
      (Parser.keySpace("def") *> (Identifier.bindableParser ~ args.?) <* maybeSpace,
        result.with1 <* (maybeSpace.with1 ~ P.char(':')),
        resultTParser)
          .mapN { case ((name, optArgs), resType, res) =>
            val args = optArgs match {
              case None => Nil
              case Some(ne) => ne.toList
            }
            DefStatement(name, args, resType, res)
        }
    }
}
