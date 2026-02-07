package dev.bosatsu

import org.typelevel.paiges.{Doc, Document}

import cats.parse.{Parser => P}

case class Indented[T](spaces: Int, value: T) {
  Require(spaces > 0, s"need non-empty indentation: $spaces")
}

object Indented {
  def spaceCount(str: String): Int =
    str.foldLeft(0) {
      case (s, ' ')  => s + 1
      case (s, '\t') => s + 4
      case (_, c)    => sys.error(s"unexpected space character($c) in $str")
    }

  implicit def document[T: Document]: Document[Indented[T]] =
    Document.instance[Indented[T]] { case Indented(i, t) =>
      Doc.spaces(i) + (Document[T].document(t).nested(i))
    }

  /** This reads a new line at a deeper indentation level than we currently are.
    *
    * So we are starting from the 0 column and read the current indentation
    * level plus at least one space more
    */
  def indy[T](p: Parser.Indy[T]): Parser.Indy[Indented[T]] =
    Parser.Indy { indent =>
      for {
        thisIndent <- P.string0(indent).with1.soft *> Parser.spaces.string
        t <- p.run(indent + thisIndent)
      } yield Indented(Indented.spaceCount(thisIndent), t)
    }
}
