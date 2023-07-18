package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.{Doc, Document}

/** Represents a commented thing. Commented[A] would probably be a better name
  */
final case class CommentStatement[T](message: NonEmptyList[String], on: T)

object CommentStatement {
  type Maybe[T] = Either[T, CommentStatement[T]]

  implicit def document[T: Document]: Document[CommentStatement[T]] =
    Document.instance[CommentStatement[T]] { comment =>
      import comment._
      val block = Doc.intercalate(
        Doc.line,
        message.toList.map { mes => Doc.char('#') + Doc.text(mes) }
      )
      block + Doc.line + Document[T].document(on)
    }

  /** on should make sure indent is matching this is to allow a P[Unit] that
    * does nothing for testing or other applications
    */
  def parser[T](onP: String => P0[T]): Parser.Indy[CommentStatement[T]] =
    Parser.Indy { indent =>
      val sep = Parser.newline

      val commentBlock: P[NonEmptyList[String]] =
        // if the next line is part of the comment until we see the # or not
        (Parser.maybeSpace.with1.soft *> commentPart)
          .repSep(sep = sep) <* Parser.newline.orElse(P.end)

      (commentBlock ~ onP(indent))
        .map { case (m, on) => CommentStatement(m, on) }
    }

  val commentPart: P[String] =
    P.char('#') *> P.until0(P.char('\n'))
}
