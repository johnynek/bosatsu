package org.bykn.bosatsu

import cats.effect.IO
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc

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

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: Path
  ): IO[Unit] =
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
        } else None
      }
    }
  }

  def hasExtension(str: String): Path => Boolean = { (path: Path) =>
    path.toString.endsWith(str)
  }

  def print(str: => String): IO[Unit] =
    IO(println(str))

  def delay[A](a: => A): IO[A] = IO(a)

  def report(io: IO[Output]): IO[ExitCode] =
    io.attempt.flatMap {
      case Right(out) => reportOutput(out)
      case Left(err)  => reportException(err).as(ExitCode.Error)
    }

  def reportOutput(out: Output): IO[ExitCode] =
    out match {
      case Output.TestOutput(resMap, color) =>
        val noTests = resMap.collect { case (p, None) => p }.toList
        val results = resMap
          .collect { case (p, Some(t)) => (p, Test.report(t.value, color)) }
          .toList
          .sortBy(_._1)

        val successes = results.iterator.map { case (_, (s, _, _)) => s }.sum
        val failures = results.iterator.map { case (_, (_, f, _)) => f }.sum
        val success = noTests.isEmpty && (failures == 0)
        val suffix =
          if (results.lengthCompare(1) > 0)
            (Doc.hardLine + Doc.hardLine + Test.summary(
              successes,
              failures,
              color
            ))
          else Doc.empty
        val docRes: Doc =
          Doc.intercalate(
            Doc.hardLine + Doc.hardLine,
            results.map { case (p, (_, _, d)) =>
              Doc.text(p.asString) + Doc.char(':') + (Doc.lineOrSpace + d)
                .nested(2)
            }
          ) + suffix

        if (success) print(docRes.render(80)).as(ExitCode.Success)
        else {
          val missingDoc =
            if (noTests.isEmpty) Nil
            else {
              val prefix = Doc.text("packages with missing tests: ")
              val missingDoc = Doc.intercalate(
                Doc.comma + Doc.lineOrSpace,
                noTests.sorted.map { p => Doc.text(p.asString) }
              )
              (prefix + missingDoc.nested(2)) :: Nil
            }

          val fullOut = Doc.intercalate(
            Doc.hardLine + Doc.hardLine + (Doc.char('#') * 80) + Doc.line,
            docRes :: missingDoc
          )

          val failureStr =
            if (failures == 1) "1 test failure"
            else s"$failures test failures"

          val missingCount = noTests.size
          val excepMessage =
            if (missingCount > 0) {
              val packString = if (missingCount == 1) "package" else "packages"
              s"$failureStr and $missingCount $packString with no tests found"
            } else failureStr

          print(
            (fullOut + Doc.hardLine + Doc.hardLine + Doc.text(excepMessage))
              .render(80)
          )
            .as(ExitCode.Error)
        }
      case Output.EvaluationResult(_, tpe, resDoc) =>
        val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
        val doc =
          resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
        print(doc.render(100)).as(ExitCode.Success)
      case Output.JsonOutput(json, pathOpt) =>
        val jdoc = json.toDoc
        (pathOpt match {
          case Some(path) => CodeGenWrite.writeDoc(path, jdoc)
          case None       => IO(println(jdoc.renderTrim(80)))
        }).as(ExitCode.Success)

      case Output.TranspileOut(outs, base) =>
        def path(p: List[String]): Path =
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
    }

  def reportException(ex: Throwable): IO[Unit] =
    ex match {
      case PathModule.MainException.NoInputs(cmd) =>
        val name = cmd.name
        IO.consoleForIO.errorln(s"no inputs given to $name")
      case pe @ PathModule.MainException.ParseErrors(_, _, _) =>
        IO.consoleForIO.errorln(pe.messages.mkString("\n"))
      case pe @ PathModule.MainException.PackageErrors(_, _, _, _) =>
        IO.consoleForIO.errorln(pe.messages.mkString("\n"))
      case err =>
        IO.consoleForIO.errorln("unknown error:\n") *>
          IO(err.printStackTrace(System.err))
    }

  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] = {
    import scala.jdk.CollectionConverters._

    def getP(p: Path): Option[PackageName] = {
      val subPath = p
        .relativize(packFile)
        .asScala
        .map { part =>
          part.toString.toLowerCase.capitalize
        }
        .mkString("/")

      val dropExtension = """(.*)\.[^.]*$""".r
      val toParse = subPath match {
        case dropExtension(prefix) => prefix
        case _                     => subPath
      }
      PackageName.parse(toParse)
    }

    @annotation.tailrec
    def loop(roots: List[Path]): Option[PackageName] =
      roots match {
        case Nil                              => None
        case h :: _ if packFile.startsWith(h) => getP(h)
        case _ :: t                           => loop(t)
      }

    if (packFile.toString.isEmpty) None
    else loop(roots)
  }
}
