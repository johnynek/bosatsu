package org.bykn.bosatsu

import cats.effect.{IO, Resource}
import org.typelevel.paiges.{Doc, Document}

import java.nio.file.{Path => JPath}

import cats.data.NonEmptyList
import cats.effect.ExitCode

import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._

object PathModule extends MainModule[IO, JPath](IOPlatformIO) { self =>
  type Path = JPath

  def print(str: String): IO[Unit] =
    IO.println(str)

  val parResource: Resource[IO, Par.EC] =
    Resource.make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC => IO[A]): IO[A] =
    parResource.use(fn)

  def report(io: IO[Output]): IO[ExitCode] =
    io.attempt.flatMap {
      case Right(out) => reportOutput(out)
      case Left(err)  => reportException(err).as(ExitCode.Error)
    }

  private def writeOut(doc: Doc, out: Option[JPath]): IO[Unit] =
    out match {
      case None =>
        IO.blocking {
          doc
            .renderStreamTrim(80)
            .iterator
            .foreach(System.out.print)

          System.out.println("")
        }
      case Some(p) =>
        CodeGenWrite.writeDoc(p, doc)
    }

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: JPath
  ): IO[Unit] =
    ProtoConverter.writeInterfaces(interfaces, path)

  def writePackages[A](packages: List[Package.Typed[A]], path: JPath): IO[Unit] =
    ProtoConverter.writePackages(packages, path)

  def reportOutput(out: Output): IO[ExitCode] =
    out match {
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
        print(testReport.doc.render(80)).as(code)
      case Output.EvaluationResult(_, tpe, resDoc) =>
        val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
        val doc =
          resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
        print(doc.render(100)).as(ExitCode.Success)
      case Output.JsonOutput(json, pathOpt) =>
        writeOut(json.toDoc, pathOpt)
          .as(ExitCode.Success)

      case Output.TranspileOut(outs, base) =>
        def path(p: List[String]): JPath =
          p.foldLeft(base)(_.resolve(_))

        outs.toList
          .map { case (p, d) =>
            (p, CodeGenWrite.writeDoc(path(p.toList), d))
          }
          .sortBy(_._1)
          .traverse_ { case (_, w) => w }
          .as(ExitCode.Success)

      case Output.CompileOut(packList, ifout, output) =>
        val ifres = ifout match {
          case None =>
            IO.unit
          case Some(ifacePath) =>
            val ifs = packList.map(Package.interfaceOf(_))
            writeInterfaces(ifs, ifacePath)
        }
        val out = output.fold(IO.unit)(writePackages(packList, _))

        (ifres *> out).as(ExitCode.Success)

      case Output.ShowOutput(packs, ifaces, output) =>
        val pdocs = packs.map { pack =>
          Document[Package.Typed[Any]].document(pack)
        }
        val idocs = ifaces.map { iface =>
          Document[Package.Interface].document(iface)
        }

        val doc = Doc.intercalate(Doc.hardLine, idocs ::: pdocs)
        writeOut(doc, output).as(ExitCode.Success)

      case Output.DepsOutput(depinfo, output, style) =>
        style match {
          case GraphOutput.Json =>
            def toJson(
                dep: (Path, PackageName, FileKind, List[PackageName])
            ): Json =
              Json.JObject(
                ("path", Json.JString(dep._1.toString())) ::
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
              depinfo
                .sortBy { case (path, pn, fk, _) => (path, pn, fk.name) }
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

            val knownPacks = depinfo.map(_._2).toSet
            val allPacks =
              depinfo.flatMap(dep => dep._2 :: dep._4).distinct.sorted
            val unknownPacks = allPacks.filterNot(knownPacks)
            type NodeMap = Map[PackageName, NonEmptyList[
              (Int, Option[FileKind], String, String)
            ]]
            val depinfoSize = depinfo.size
            val nodes: NodeMap =
              (depinfo.zipWithIndex.map { case (dep, idx) =>
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
              depinfo.flatMap { case (_, pn, k, deps) =>
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
    }

  def reportException(ex: Throwable): IO[Unit] =
    mainExceptionToString(ex) match {
      case Some(msg) =>
        IO.consoleForIO.errorln(msg)
      case None =>
        IO.consoleForIO.errorln("unknown error:\n") *>
          IO.blocking(ex.printStackTrace(System.err))
    }

}
