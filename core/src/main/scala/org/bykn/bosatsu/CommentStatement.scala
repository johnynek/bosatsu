package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.bykn.bosatsu.parser.{Parser => P, Parser1 => P1}
import org.typelevel.paiges.{ Doc, Document }

/**
 * Represents a commented thing. Commented[A] would probably
 * be a better name
 */
final case class CommentStatement[T](message: NonEmptyList[String], on: T)

object CommentStatement {
  type Maybe[T] = Either[T, CommentStatement[T]]

  implicit def document[T: Document]: Document[CommentStatement[T]] =
    Document.instance[CommentStatement[T]] { comment =>
      import comment._
      val block = Doc.intercalate(Doc.line, message.toList.map { mes => Doc.char('#') + Doc.text(mes) })
      block + Doc.line + Document[T].document(on)
    }

  /** on should make sure indent is matching
   * this is to allow a P[Unit] that does nothing for testing or other applications
   */
  def parser[T](onP: Parser.Indy[T]): Parser.Indy[CommentStatement[T]] = Parser.Indy { indent =>
    val sep = Parser.newline ~ Parser.indentation(indent)

    val commentBlock: P1[NonEmptyList[String]] =
      (commentPart ~ (sep *> commentPart).rep ~ Parser.newline.orElse(P.end))
        .map { case ((c1, cs), _) => NonEmptyList(c1, cs) }

    (commentBlock ~ onP(indent))
      .map { case (m, on) => CommentStatement(m, on) }
  }

  val commentPart: P1[String] =
    (P.char('#') ~ P.until(P.char('\n'))).map(_._2)
}


