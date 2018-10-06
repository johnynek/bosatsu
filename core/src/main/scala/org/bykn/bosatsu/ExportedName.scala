package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}

import Parser.{lowerIdent, upperIdent}

sealed abstract class ExportedName[+T] {
  def name: String
  def tag: T
}
object ExportedName {
  case class Binding[T](name: String, tag: T) extends ExportedName[T]
  case class TypeName[T](name: String, tag: T) extends ExportedName[T]
  case class Constructor[T](name: String, tag: T) extends ExportedName[T]

  private[this] val consDoc = Doc.text("()")

  implicit val document: Document[ExportedName[Unit]] =
    Document.instance[ExportedName[Unit]] {
      case Binding(n, _) => Doc.text(n)
      case TypeName(n, _) => Doc.text(n)
      case Constructor(n, _) => Doc.text(n) + consDoc
    }

  val parser: P[ExportedName[Unit]] =
    lowerIdent.map(Binding(_, ())) |
      P(upperIdent ~ "()".!.?).map {
        case (n, None) => TypeName(n, ())
        case (n, Some(_)) => Constructor(n, ())
      }

  private[bosatsu] def buildExportMap[T](exs: List[ExportedName[T]]): Map[String, NonEmptyList[ExportedName[T]]] =
    exs match {
      case Nil => Map.empty
      case h :: tail => NonEmptyList(h, tail).groupBy(_.name)
    }
}

