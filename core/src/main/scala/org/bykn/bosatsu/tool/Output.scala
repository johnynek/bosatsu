package org.bykn.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.Eval
import cats.data.Chain
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

  case class TranspileOut[Path](outs: List[(Path, Doc)]) extends Output[Path]

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

  case class Basic[Path](toDoc: Doc, output: Option[Path]) extends Output[Path]
  case class Library[Path](lib: proto.Library, output: Path) extends Output[Path]

  case class Many[Path](outputs: Chain[Output[Path]]) extends Output[Path]

  def many[Path](outs: Output[Path]*): Output[Path] =
    Many(Chain.fromSeq(outs))
}