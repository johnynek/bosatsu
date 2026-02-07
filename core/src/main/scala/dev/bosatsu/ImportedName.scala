package dev.bosatsu

import cats.{Eq, Functor}
import cats.parse.{Parser => P}
import org.typelevel.paiges.{Doc, Document}
import Parser.spaces

sealed abstract class ImportedName[+T] {
  def originalName: Identifier
  def localName: Identifier
  def tag: T
  def isRenamed: Boolean = originalName != localName
  def withTag[U](tag: U): ImportedName[U]

  def map[U](fn: T => U): ImportedName[U] =
    this match {
      case ImportedName.OriginalName(n, t) =>
        ImportedName.OriginalName(n, fn(t))
      case ImportedName.Renamed(o, l, t) =>
        ImportedName.Renamed(o, l, fn(t))
    }

  def traverse[F[_], U](
      fn: T => F[U]
  )(implicit F: Functor[F]): F[ImportedName[U]] =
    this match {
      case ImportedName.OriginalName(n, t) =>
        F.map(fn(t))(ImportedName.OriginalName(n, _))
      case ImportedName.Renamed(o, l, t) =>
        F.map(fn(t))(ImportedName.Renamed(o, l, _))
    }
}

object ImportedName {
  implicit def eqImportedName[T](implicit eqT: Eq[T]): Eq[ImportedName[T]] =
    Eq.instance {
      case (
            OriginalName(leftName, leftTag),
            OriginalName(rightName, rightTag)
          ) =>
        (leftName == rightName) && eqT.eqv(leftTag, rightTag)
      case (
            Renamed(leftOrig, leftLocal, leftTag),
            Renamed(rightOrig, rightLocal, rightTag)
          ) =>
        (leftOrig == rightOrig) &&
        (leftLocal == rightLocal) &&
        eqT.eqv(leftTag, rightTag)
      case _ => false
    }
  case class OriginalName[T](originalName: Identifier, tag: T)
      extends ImportedName[T] {
    def localName = originalName
    def withTag[U](tag: U): ImportedName[U] = copy(tag = tag)
  }
  case class Renamed[T](originalName: Identifier, localName: Identifier, tag: T)
      extends ImportedName[T] {
    def withTag[U](tag: U): ImportedName[U] = copy(tag = tag)
  }

  implicit val document: Document[ImportedName[Unit]] =
    Document.instance[ImportedName[Unit]] {
      case ImportedName.OriginalName(nm, _) => Document[Identifier].document(nm)
      case ImportedName.Renamed(from, to, _) =>
        Document[Identifier].document(from) + Doc.text(" as ") +
          Document[Identifier].document(to)
    }

  val parser: P[ImportedName[Unit]] = {
    def basedOn(of: P[Identifier]): P[ImportedName[Unit]] =
      (of ~ (spaces.soft *> P.string("as") *> spaces *> of).?)
        .map {
          case (from, Some(to)) => ImportedName.Renamed(from, to, ())
          case (orig, None)     => ImportedName.OriginalName(orig, ())
        }

    basedOn(Identifier.bindableParser)
      .orElse(basedOn(Identifier.consParser))
  }
}
