package org.bykn.bosatsu.codegen

import cats.data.{NonEmptyList, ValidatedNel, Validated}
import com.monovore.decline.Argument
import org.bykn.bosatsu.{PackageMap, Par}
import org.typelevel.paiges.Doc
import scala.util.Try

trait Transpiler {
  def name: String

  def renderAll(
      pm: PackageMap.Typed[Any],
      externals: List[String],
      evaluators: List[String]
  )(implicit ec: Par.EC): Try[List[(NonEmptyList[String], Doc)]]
}

object Transpiler {
  def argumentFromTranspilers(all: List[Transpiler]): Argument[Transpiler] =
    new Argument[Transpiler] {
      val nameTo = all.iterator.map(t => (t.name, t)).toMap
      lazy val keys = nameTo.keys.toList.sorted.mkString(",")

      def defaultMetavar: String = "transpiler"
      def read(string: String): ValidatedNel[String, Transpiler] =
        nameTo.get(string) match {
          case Some(t) => Validated.valid(t)
          case None =>
            Validated.invalidNel(
              s"unknown transpiler: $string, expected one of: $keys"
            )
        }
    }

}