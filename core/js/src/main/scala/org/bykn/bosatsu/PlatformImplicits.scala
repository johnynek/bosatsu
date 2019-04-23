package org.bykn.bosatsu

import com.monovore.decline.Argument
import cats.data.{Validated, ValidatedNel}

object JSPath {
  def fromString(str: String) = {
    val isAbsolute = str.startsWith("/")
    val names = str.split("/").filter(!_.isEmpty).toList
    JSPath(isAbsolute, names)
  }
}

case class JSPath(isAbsolute: Boolean, names: List[String]) extends Path {
  def resolve(other: String) = JSPath.fromString(other) match {
    case absolute if absolute.isAbsolute => absolute
    case relative => JSPath(isAbsolute, names ::: relative.names)
  }
  lazy val getParent = names.reverse match {
    case _ :: rest => Some(JSPath(isAbsolute, rest.reverse))
    case Nil => None
  }
  def toJPath = ???
}

object PlatformImplicits {
  implicit val readPath: Argument[Path] = new Argument[Path] {
    override def read(string: String): ValidatedNel[String, Path] = Validated.valid(JSPath.fromString(string)) // we don't actually validate

    override def defaultMetavar: String = "path"
  }
}
