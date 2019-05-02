package org.bykn.bosatsu

import cats.Functor
import cats.implicits._
import org.typelevel.paiges.{Doc, Document}
import org.bykn.fastparse_cats.StringInstances._

import Parser.Indy

sealed abstract class OptIndent[A] {
  def get: A

  def sepDoc: Doc =
    this match {
      case OptIndent.SameLine(_) => Doc.space
      case OptIndent.NotSameLine(_) => Doc.empty
    }

  def traverse[F[_]: Functor, B](fn: A => F[B]): F[OptIndent[B]] =
    this match {
      case OptIndent.SameLine(a) => fn(a).map(OptIndent.SameLine(_))
      case OptIndent.NotSameLine(Padding(p, Indented(i, a))) =>
        fn(a).map { b =>
          OptIndent.NotSameLine(Padding(p, Indented(i, b)))
        }
    }
}

object OptIndent {
  def same[A](a: A): OptIndent[A] =
    SameLine(a)

  def notSame[A](toPadIndent: Padding[Indented[A]]): OptIndent[A] =
    NotSameLine(toPadIndent)

  case class SameLine[A](get: A) extends OptIndent[A]
  case class NotSameLine[A](toPadIndent: Padding[Indented[A]]) extends OptIndent[A] {
    def get: A = toPadIndent.padded.value
  }

  def paddedIndented[A](padding: Int, indenting: Int, a: A): OptIndent[A] =
    if (padding == 0 && indenting == 0) SameLine(a)
    else NotSameLine(Padding(padding, Indented(indenting, a)))

  implicit def document[A: Document]: Document[OptIndent[A]] =
    Document.instance[OptIndent[A]] {
      case SameLine(a) => Document[A].document(a)
      case NotSameLine(p) =>
        Document[Padding[Indented[A]]].document(p)
    }

  def indy[A](p: Indy[A]): Indy[OptIndent[A]] = {
    val ind = Indented.indy(p)
    // we need to read at least 1 new line here
    val not = ind.mapF(Padding.parser1(_)).map(notSame[A](_))
    val sm = p.map(same[A](_))
    not.combineK(sm)
  }
}
