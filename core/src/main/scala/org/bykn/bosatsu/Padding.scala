package org.bykn.bosatsu

import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Parser.maybeSpace

// Represents vertical padding
case class Padding[T](lines: Int, padded: T)
object Padding {
  implicit def document[T: Document]: Document[Padding[T]] =
    Document.instance[Padding[T]] { padding =>
      Doc.line.repeat(padding.lines) + Document[T].document(padding.padded)
    }

  def parser[T](p: P[T]): P[Padding[T]] =
    P((maybeSpace ~ "\n").!.rep() ~ p).map { case (vec, t) => Padding(vec.size, t) }

  def parser1[T](p: P[T]): P[Padding[T]] =
    P((maybeSpace ~ "\n").!.rep(1) ~ p).map { case (vec, t) => Padding(vec.size, t) }
}

