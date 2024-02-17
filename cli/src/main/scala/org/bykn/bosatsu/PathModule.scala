package org.bykn.bosatsu

import cats.effect.IO
import com.monovore.decline.Argument
import org.typelevel.paiges.{Doc, Document}

import java.nio.file.{Path => JPath}

import cats.implicits._
import cats.effect.ExitCode

object PathModule extends MainModule[IO] {
  type Path = JPath

  override def pathArg: Argument[Path] =
    Argument.readPath

  def readPath(path: Path): IO[String] =
    IO(new String(java.nio.file.Files.readAllBytes(path), "utf-8"))

  val resolvePath: Some[(Path, PackageName) => IO[Option[Path]]] =
    Some(
      { (root: Path, pack: PackageName) =>
        val dir = pack.parts.init.foldLeft(root)(_.resolve(_))
        val filePath = dir.resolve(pack.parts.last + ".bosatsu")
        IO {
          // this is a side-effect since file is mutable
          // and talks to the file system
          val file = filePath.toFile
          if (file.exists()) Some(filePath)
          else None
        }
      }
    )

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
    ProtoConverter.readPackages(paths)

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
    ProtoConverter.readInterfaces(paths)

  def writeInterfaces(interfaces: List[Package.Interface], path: Path): IO[Unit] =
    ProtoConverter.writeInterfaces(interfaces, path)

  def writePackages[A](packages: List[Package.Typed[A]], path: Path): IO[Unit] =
    ProtoConverter.writePackages(packages, path)

  def unfoldDir: Option[Path => IO[Option[IO[List[Path]]]]] = Some {
    { (path: Path) =>
      IO {
        val f = path.toFile

        if (f.isDirectory()) {
          Some(IO {
            f.listFiles.iterator.map(_.toPath).toList
          })
        }
        else None
      }
    }
  }

  def hasExtension(str: String): Path => Boolean =
    { (path: Path) => path.toString.endsWith(str) }

  def print(str: => String): IO[Unit] =
    IO(println(str))

  def delay[A](a: => A): IO[A] = IO(a)

  def report(io: IO[Output]): IO[ExitCode] =
    io.attempt.flatMap {
      case Right(out) => reportOutput(out)
      case Left(err) => reportException(err).as(ExitCode.Error)
    }

  def reportOutput(out: Output): IO[ExitCode] =
    out match {
      case Output.TestOutput(resMap, color) =>
        val hasMissing = resMap.exists(_._2.isEmpty)
        val testReport = Test.outputFor(resMap, color)
        val success = !hasMissing && (testReport.fails == 0)
        val code = if (success) ExitCode.Success else ExitCode.Error
        print(testReport.doc.render(80)).as(code)
      case Output.EvaluationResult(_, tpe, resDoc) =>
        val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
        val doc = resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
        print(doc.render(100)).as(ExitCode.Success)
      case Output.JsonOutput(json, pathOpt) =>
        val jdoc = json.toDoc
        (pathOpt match {
          case Some(path) => CodeGenWrite.writeDoc(path, jdoc)
          case None => IO(println(jdoc.renderTrim(80)))
        }).as(ExitCode.Success)

      case Output.TranspileOut(outs, base) =>
        def path(p: List[String]): Path =
          p.foldLeft(base)(_.resolve(_))

        outs.toList.map { case (p, d) =>
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
        output match {
          case None =>
            IO.blocking {
              doc.renderStreamTrim(80)
                .iterator
                .foreach(System.out.print)

              System.out.println("")
            }
            .as(ExitCode.Success)
          case Some(p) =>
            CodeGenWrite.writeDoc(p, doc)
              .as(ExitCode.Success)
        }
    }

  def reportException(ex: Throwable): IO[Unit] =
    mainExceptionToString(ex) match {
      case Some(msg) =>
        IO.consoleForIO.errorln(msg)
      case None =>
        IO.consoleForIO.errorln("unknown error:\n") *>
          IO(ex.printStackTrace(System.err))
    }

  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] = {
    import scala.jdk.CollectionConverters._

    def getP(p: Path): Option[PackageName] = {
      val subPath = p.relativize(packFile)
        .asScala
        .map { part =>
          part.toString.toLowerCase.capitalize
        }
        .mkString("/")

      val dropExtension = """(.*)\.[^.]*$""".r
      val toParse = subPath match {
        case dropExtension(prefix) => prefix
        case _ => subPath
      }
      PackageName.parse(toParse)
    }

    @annotation.tailrec
    def loop(roots: List[Path]): Option[PackageName] =
      roots match {
        case Nil => None
        case h :: _ if packFile.startsWith(h) => getP(h)
        case _ :: t => loop(t)
      }

    if (packFile.toString.isEmpty) None
    else loop(roots)
  }
}
