package org.bykn.bosatsu.tool

import cats.Eval
import cats.data.NonEmptyList
import org.bykn.bosatsu.{Json, Package, PackageName, Test, Value, rankn}
import org.typelevel.paiges.Doc
import org.bykn.bosatsu.LocationMap.Colorize

sealed abstract class Output[+Path]
object Output {
  case class TestOutput(
      tests: List[(PackageName, Option[Eval[Test]])],
      colorize: Colorize
  ) extends Output[Nothing]

  case class EvaluationResult(
      value: Eval[Value],
      tpe: rankn.Type,
      doc: Eval[Doc]
  ) extends Output[Nothing]

  case class JsonOutput[Path](json: Json, output: Option[Path]) extends Output[Path]

  case class CompileOut[Path](
      packList: List[Package.Typed[Any]],
      ifout: Option[Path],
      output: Option[Path]
  ) extends Output[Path]

  case class TranspileOut[Path](outs: List[(NonEmptyList[String], Doc)], base: Path)
      extends Output[Path]

  case class ShowOutput[Path](
      packages: List[Package.Typed[Any]],
      ifaces: List[Package.Interface],
      output: Option[Path]
  ) extends Output[Path]

  case class DepsOutput[Path](
      depinfo: List[(Path, PackageName, FileKind, List[PackageName])],
      output: Option[Path],
      style: GraphOutput
  ) extends Output[Path]
}