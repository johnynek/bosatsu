package dev.bosatsu

import cats.{Order, Show}
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser => P}
import com.monovore.decline.Argument
import org.typelevel.paiges.{Doc, Document}
import Parser.upperIdent

case class PackageName(parts: NonEmptyList[String]) derives CanEqual {
  lazy val asString: String = parts.toList.mkString("/")

  def isPredef: Boolean = parts === PackageName.PredefName.parts
}

object PackageName {

  def parts(first: String, rest: String*): PackageName =
    PackageName(NonEmptyList.of(first, rest*))

  implicit val document: Document[PackageName] =
    Document.instance[PackageName](pn => Doc.text(pn.asString))

  implicit val parser: P[PackageName] =
    (upperIdent ~ (P.char('/') *> upperIdent).rep0)
      .map { case (head, tail) =>
        PackageName(NonEmptyList(head, tail))
      }

  def parse(s: String): Option[PackageName] =
    parser.parse(s) match {
      case Right(("", pn)) => Some(pn)
      case _               => None
    }

  implicit val order: Order[PackageName] =
    Order[NonEmptyList[String]].contramap[PackageName](_.parts)

  implicit val packageNameOrdering: Ordering[PackageName] =
    order.toOrdering

  val PredefName: PackageName =
    PackageName(NonEmptyList.of("Bosatsu", "Predef"))

  implicit val argPack: Argument[PackageName] =
    new Argument[PackageName] {
      def defaultMetavar: String = "packageName"
      def read(string: String): cats.data.ValidatedNel[String, PackageName] =
        parser.parseAll(string) match {
          case Right(pn) => cats.data.Validated.valid(pn)
          case Left(err) =>
            val parserErr = err.toString
            cats.data.Validated.invalidNel(
              s"""could not parse $string as a package name. Must be capitalized strings separated by /.
                 |parser error: $parserErr
                 |hint: use the package declaration name (example: Euler/One), not the filename stem (e.g. euler1).""".stripMargin
            )
        }
    }

  implicit val showPackageName: Show[PackageName] =
    Show.show(_.asString)
}
