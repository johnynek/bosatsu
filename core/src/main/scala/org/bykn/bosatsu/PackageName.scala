package org.bykn.bosatsu

import cats.Order
import cats.data.NonEmptyList
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}
import Parser.upperIdent

case class PackageName(parts: NonEmptyList[String]) {
  def asString: String = parts.toList.mkString("/")
}

object PackageName {

  def parts(first: String, rest: String*): PackageName =
    PackageName(NonEmptyList.of(first, rest :_*))

  implicit val document: Document[PackageName] =
    Document.instance[PackageName] { pn => Doc.text(pn.asString) }

  implicit val parser: P[PackageName] =
    P(upperIdent ~ ("/" ~ upperIdent).rep()).map { case (head, tail) =>
      PackageName(NonEmptyList(head, tail.toList))
    }

  def parse(s: String): Option[PackageName] =
    parser.parse(s) match {
      case Parsed.Success(pn, _) => Some(pn)
      case _ => None
    }

  implicit val order: Order[PackageName] =
    Order[NonEmptyList[String]].contramap[PackageName](_.parts)

  implicit val packageNameOrdering: Ordering[PackageName] =
    order.toOrdering

  val predef: PackageName =
    PackageName(NonEmptyList.of("Bosatsu", "Predef"))
}

