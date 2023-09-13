package org.bykn.bosatsu

import cats.Functor
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.{Doc, Document}

import Parser.maybeSpace

// Represents vertical padding
case class Padding[T](lines: Int, padded: T) {
  def map[B](fn: T => B): Padding[B] =
    Padding(lines, fn(padded))

  def traverse[F[_]: Functor, B](fn: T => F[B]): F[Padding[B]] =
    Functor[F].map(fn(padded))(Padding(lines, _))
}
object Padding {
  implicit def document[T: Document]: Document[Padding[T]] =
    Document.instance[Padding[T]] { padding =>
      Doc.line.repeat(padding.lines) + Document[T].document(padding.padded)
    }

  /** This allows an empty padding
    */
  def parser[T](p: P[T]): P[Padding[T]] = {
    val spacing = (maybeSpace.with1.soft ~ Parser.newline).void.rep0

    (spacing.with1.soft ~ p)
      .map { case (vec, t) => Padding(vec.size, t) }
  }

  /** Parses a padding of length 1 or more, then p
    */
  def parser1[T](p: P0[T]): P[Padding[T]] =
    ((maybeSpace.with1.soft ~ Parser.newline).void.rep ~ p)
      .map { case (vec, t) => Padding(vec.size, t) }

  /** This is parser1 by itself, with the padded value being ()
    */
  val nonEmptyParser: P[Padding[Unit]] =
    parser1(P.unit)
}
