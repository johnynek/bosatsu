package org.bykn.bosatsu

import java.nio.file.{Path => JPath}
import java.nio.file.{InvalidPathException, Paths}
import com.monovore.decline.Argument
import cats.data.{Validated, ValidatedNel}
import scala.collection.JavaConverters._

case class JVMPath(toJPath: JPath) extends Path {
  def resolve(str: String) = JVMPath(toJPath.resolve(str))
  def getParent = Option(toJPath.getParent).map(JVMPath(_))
  def names = toJPath.iterator.asScala.toList.map(_.toString)
  def isAbsolute = toJPath.isAbsolute
}

object PlatformImplicits {
    implicit val readPath: Argument[Path] = new Argument[Path] {
      override def read(string: String): ValidatedNel[String, Path] =
        try { Validated.valid(JVMPath(Paths.get(string))) }
        catch { case ipe: InvalidPathException => Validated.invalidNel(s"Invalid path: $string (${ ipe.getReason })") }

      override def defaultMetavar: String = "path"
    }
}
