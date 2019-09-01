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

  private def parser[A](p: P[A], min: Int): P[Padding[A]] =
    // if we have a P[Unit] then rep() is also P[Unit] due to weird shit
    // If p consumes nothing, toEOL is dangerous because
    // it can match End, and End.rep() will OOM when it hits
    // the end of the file
    (P(maybeSpace ~ "\n").map(_ => 0).rep(min) ~ p)
      .map { case (vec, t) => Padding(vec.size, t) }

  /**
   * This allows an empty padding
   * this is a bit dangerous with a p that consumes nothing
   * as parsers that succeed without consuming input
   * are dangerous to use with rep
   */
  def parser[T](p: P[T]): P[Padding[T]] =
    parser(p, 0)

  /**
   * Parses a padding of length 1 or more, then p
   */
  def parser1[T](p: P[T]): P[Padding[T]] =
    parser(p, 1)

  /**
   * This is parser1 by itself, with the padded value being ()
   */
  val nonEmptyParser: P[Padding[Unit]] =
    parser1(PassWith(()))
}

