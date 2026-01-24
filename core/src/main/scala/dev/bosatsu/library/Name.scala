package dev.bosatsu.library

import dev.bosatsu.Json.{Reader, Writer}
import com.monovore.decline.Argument
import cats.data.{Validated, ValidatedNel}

case class Name(name: String)
object Name {
  implicit val showName: cats.Show[Name] = cats.Show.show(_.name)
  implicit val orderingName: Ordering[Name] = Ordering.by(_.name)
  implicit val orderName: cats.Order[Name] = cats.Order.by(_.name)
  implicit val readerName: Reader[Name] =
    Reader[String].mapEither("Libraries.Name")(s => Right(Name(s)))
  implicit val writerName: Writer[Name] = Writer[String].contramap[Name](_.name)
  implicit val argumentName: Argument[Name] =
    new Argument[Name] {
      def defaultMetavar: String = "lib_name"
      def read(string: String): ValidatedNel[String, Name] =
        Validated.valid(Name(string))
    }
}
