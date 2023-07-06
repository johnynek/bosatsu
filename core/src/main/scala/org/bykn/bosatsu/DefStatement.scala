package org.bykn.bosatsu

import Parser.{Combinators, maybeSpace}
import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import org.typelevel.paiges.{Doc, Document}

import Identifier.Bindable

import cats.syntax.all._

case class DefStatement[A, B](
    name: Bindable,
    typeArgs: Option[NonEmptyList[(TypeRef.TypeVar, Option[Kind])]],
    args: NonEmptyList[A],
    retType: Option[TypeRef],
    result: B
)

object DefStatement {
  private[this] val defDoc = Doc.text("def ")
  private[this] val arrow = Doc.text(" -> ")
  private[this] val commaSpace = Doc.text(", ")
  private[this] val colonSpace = Doc.text(": ")

  implicit def document[A: Document, B: Document]
      : Document[DefStatement[A, B]] =
    Document.instance[DefStatement[A, B]] { defs =>
      import defs._
      val res = retType.fold(Doc.empty) { t => arrow + t.toDoc }
      val taDoc = typeArgs match {
        case None => Doc.empty
        case Some(ta) =>
          TypeRef.docTypeArgs(ta.toList) {
            case None    => Doc.empty
            case Some(k) => colonSpace + Kind.toDoc(k)
          }
      }
      val argDoc =
        Doc.char('(') +
          Doc.intercalate(
            commaSpace,
            args.map(Document[A].document(_)).toList
          ) +
          Doc.char(')')
      val line0 =
        defDoc + Document[Bindable].document(name) + taDoc + argDoc + res + Doc
          .char(':')

      line0 + Document[B].document(result)
    }

  /** The resultTParser should parse some indentation any newlines
    */
  def parser[A, B](
      argParser: P[A],
      resultTParser: P[B]
  ): P[DefStatement[A, B]] = {
    val args = argParser.parensLines1Cut
    val result = (P.string("->") *> maybeSpace *> TypeRef.parser).?
    val kindAnnot: P[Kind] =
      (maybeSpace.soft.with1 *> (P.char(':') *> maybeSpace *> Kind.parser))

    (
      Parser.keySpace(
        "def"
      ) *> (Identifier.bindableParser ~ TypeRef
        .typeParams(kindAnnot.?)
        .? ~ args) <* maybeSpace,
      result.with1 <* (maybeSpace.with1 ~ P.char(':')),
      resultTParser
    )
      .mapN { case (((name, tps), args), resType, res) =>
        DefStatement(name, tps, args, resType, res)
      }
  }
}
