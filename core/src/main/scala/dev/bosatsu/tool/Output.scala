package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import dev.bosatsu.{Json, Package, PackageName, PlatformIO, Test, Value, rankn}
import org.typelevel.paiges.Doc
import dev.bosatsu.LocationMap.Colorize

sealed abstract class Output[+Path] {
  final def report[F[_], P >: Path](
      platformIO: PlatformIO[F, P]
  ): F[ExitCode] = {
    import platformIO._

    this match {
      case Output.TestOutput(resMap, color) =>
        val hasMissing = resMap.exists(_._2.isEmpty)
        // it would be nice to run in parallel, but
        // MatchlessToValue is not currently threadsafe
        val evalTest = resMap.map {
          case (p, Some(evalTest)) =>
            (p, Some(evalTest.value))
          case (p, None) => (p, None)
        }

        val testReport = Test.outputFor(evalTest, color)
        val success = !hasMissing && (testReport.fails == 0)
        val code = if (success) ExitCode.Success else ExitCode.Error
        println(testReport.doc.render(80)).as(code)
      case Output.EvaluationResult(_, tpe, resDoc) =>
        val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
        val doc =
          resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
        println(doc.render(100)).as(ExitCode.Success)
      case Output.JsonOutput(json, pathOpt) =>
        writeOut(json.toDoc, pathOpt)
          .as(ExitCode.Success)

      case Output.TranspileOut(outs) =>
        outs.toList
          .map { case (p, d) =>
            (p, platformIO.writeDoc(p, d))
          }
          .sortBy(_._1)(using platformIO.pathOrdering)
          .traverse_ { case (_, w) => w }
          .as(ExitCode.Success)

      case Output.CompileOut(packList, ifout, output) =>
        val ifres = ifout match {
          case None            => moduleIOMonad.unit
          case Some(ifacePath) =>
            val ifs = packList.map(Package.interfaceOf(_))
            writeInterfaces(ifs, ifacePath)
        }
        val out = output.fold(moduleIOMonad.unit)(writePackages(packList, _))

        (ifres *> out).as(ExitCode.Success)

      case Output.ShowOutput(packs, ifaces, output) =>
        val doc = ShowEdn.showDoc(packs, ifaces)
        writeOut(doc, output).as(ExitCode.Success)

      case Output.DepsOutput(depinfo, output, style) =>
        val tupleOrdering = Ordering.Tuple3(using
          platformIO.pathOrdering,
          Ordering[PackageName],
          Ordering.String
        )
        val sortedDepInfo =
          depinfo.sortBy { case (path, pn, fk, _) => (path, pn, fk.name) }(using
            tupleOrdering
          )

        style match {
          case GraphOutput.Json =>
            def toJson(
                dep: (Path, PackageName, FileKind, List[PackageName])
            ): Json =
              Json.JObject(
                ("path", Json.JString(platformIO.pathToString(dep._1))) ::
                  ("package", Json.JString(dep._2.asString)) ::
                  ("kind", Json.JString(dep._3.name)) ::
                  (
                    "dependsOn",
                    Json.JArray(
                      dep._4.map(pn => Json.JString(pn.asString)).toVector
                    )
                  ) ::
                  Nil
              )

            val asJson = Json.JArray(
              sortedDepInfo
                .map(toJson)
                .toVector
            )

            writeOut(asJson.toDoc, output).as(ExitCode.Success)
          case GraphOutput.Dot =>
            def shapeOf(fk: FileKind): String =
              fk match {
                case FileKind.Iface  => "diamond"
                case FileKind.Pack   => "box"
                case FileKind.Source => "circle"
              }

            type Ident = String
            def makeNode(
                idx: Int,
                dep: (Path, PackageName, FileKind, List[PackageName])
            ): (Ident, String) = {
              // C [shape=box, fillcolor=lightgrey, label="Node C"];
              val ident = s"N$idx"
              val decl =
                s"$ident [shape=${shapeOf(dep._3)}, label=\"${dep._2.asString}\"];"
              (ident, decl)
            }
            def makeMissing(idx: Int, pack: PackageName): (Ident, String) = {
              // C [shape=box, fillcolor=lightgrey, label="Node C"];
              val ident = s"N$idx"
              val decl = s"$ident [shape=octogon, label=\"${pack.asString}\"];"
              (ident, decl)
            }

            val knownPacks = sortedDepInfo.map(_._2).toSet
            val allPacks =
              sortedDepInfo.flatMap(dep => dep._2 :: dep._4).distinct.sorted
            val unknownPacks = allPacks.filterNot(knownPacks)
            type NodeMap = Map[PackageName, NonEmptyList[
              (Int, Option[FileKind], String, String)
            ]]
            val depinfoSize = sortedDepInfo.size
            val nodes: NodeMap =
              (sortedDepInfo.zipWithIndex.map { case (dep, idx) =>
                val (ident, nstr) = makeNode(idx, dep)
                (dep._2, (idx, Some(dep._3), ident, nstr))
              } ::: unknownPacks.mapWithIndex { (pn, idx0) =>
                val idx = depinfoSize + idx0
                val (ident, nstr) = makeMissing(idx, pn)
                (pn, (idx, None, ident, nstr))
              })
                .groupByNel(_._1)
                .map { case (k, v) =>
                  (k, v.map(_._2))
                }

            // now NodeMap has everything
            def makeEdge(
                src: PackageName,
                k: FileKind,
                dst: PackageName,
                nm: NodeMap
            ): String = {
              implicit val orderKind: cats.Order[Option[FileKind]] =
                new cats.Order[Option[FileKind]] {
                  def compare(a: Option[FileKind], b: Option[FileKind]) =
                    (a, b) match {
                      case (None, None)                                   => 0
                      case (Some(_), None)                                => -1
                      case (None, Some(_))                                => 1
                      case (Some(FileKind.Iface), Some(FileKind.Iface))   => 0
                      case (Some(FileKind.Iface), Some(_))                => -1
                      case (Some(FileKind.Pack), Some(FileKind.Iface))    => 1
                      case (Some(FileKind.Pack), Some(FileKind.Pack))     => 0
                      case (Some(FileKind.Pack), Some(FileKind.Source))   => -1
                      case (Some(FileKind.Source), Some(FileKind.Source)) => 0
                      case (Some(FileKind.Source), Some(_))               => 1
                    }
                }

              val srcNode = nm(src).find { case (_, sk, _, _) =>
                sk == Some(k)
              }.get
              val dstNode = nm(dst).sortBy(rec => (rec._2, rec._1)).head
              s"${srcNode._3} -> ${dstNode._3};"
            }

            val header = Doc.text("digraph G {")
            val allNodes: List[Doc] = nodes.iterator
              .flatMap { case (_, ns) =>
                ns.map(rec => (rec._1, Doc.text(rec._4))).toList
              }
              .toList
              .sortBy(_._1)
              .map(_._2)
            val nodesDoc = Doc.intercalate(Doc.hardLine, allNodes)
            val edges: List[Doc] =
              sortedDepInfo.flatMap { case (_, pn, k, deps) =>
                deps.map { dep =>
                  Doc.text(makeEdge(pn, k, dep, nodes))
                }
              }
            val edgesDoc = Doc.intercalate(Doc.hardLine, edges)

            val fullDoc =
              header + (Doc.hardLine + nodesDoc + Doc.hardLine + edgesDoc)
                .nested(2) +
                Doc.hardLine + Doc.char('}')

            writeOut(fullDoc, output).as(ExitCode.Success)
        }

      case Output.Basic(doc, out) =>
        writeOut(doc, out).as(ExitCode.Success)

      case Output.Library(lib, path) =>
        writeLibrary(lib, path).as(ExitCode.Success)
      case Output.Many(items) =>
        items.foldM[F, ExitCode](ExitCode.Success) {
          case (ExitCode.Success, item) => item.report(platformIO)
          case (err, _)                 => moduleIOMonad.pure(err)
        }
    }
  }
}
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

  case class JsonOutput[Path](json: Json, output: Option[Path])
      extends Output[Path]

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
  case class Library[Path](lib: proto.Library, output: Path)
      extends Output[Path]

  case class Many[Path](outputs: Chain[Output[Path]]) extends Output[Path]

  def many[Path](outs: Output[Path]*): Output[Path] =
    Many(Chain.fromSeq(outs))
}
