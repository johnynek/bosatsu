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

  def parseFn[A]: P[A => Padding[A]] =
    // if we have a P[Unit] then rep() is also P[Unit] due to weird shit
    P((maybeSpace ~ "\n").map(_ => 0).rep()).map { vec =>
      { a => Padding(vec.size, a) }
    }

  def parser[T](p: P[T]): P[Padding[T]] =
    (parseFn[T] ~ p).map { case (fn, t) => fn(t) }

  def parser1[T](p: P[T]): P[Padding[T]] =
    P((maybeSpace ~ "\n").!.rep(1) ~ p).map { case (vec, t) => Padding(vec.size, t) }
}

