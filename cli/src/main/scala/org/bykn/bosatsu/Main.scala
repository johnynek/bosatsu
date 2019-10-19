package org.bykn.bosatsu

import cats.effect.IO
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc
import scala.util.{ Failure, Success, Try }

import cats.implicits._

object PathModule extends MainModule[IO] {
  type Path = java.nio.file.Path

  override def pathArg: Argument[Path] =
    Argument.readPath

  def readPath(path: Path): IO[String] =
    IO(new String(java.nio.file.Files.readAllBytes(path), "utf-8"))

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
    ProtoConverter.readPackages(paths)

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
    ProtoConverter.readInterfaces(paths)

  def writeInterfaces(interfaces: List[Package.Interface], path: Path): IO[Unit] =
    ProtoConverter.writeInterfaces(interfaces, path)

  def writePackages[A](packages: List[Package.Typed[A]], path: Path): IO[Unit] =
    ProtoConverter.writePackages(packages, path)

  def print(str: => String): IO[Unit] =
    IO(println(str))

  def reportOutput(out: Output): IO[Unit] =
    out match {
      case Output.TestOutput(resMap) =>
        val noTests = resMap.collect { case (p, None) => p }.toList
        val results = resMap.collect { case (p, Some(t)) => (p, Test.report(t)) }.toList.sortBy(_._1)

        val failures = results.iterator.map { case (_, (_, f, _)) => f }.sum
        val success = noTests.isEmpty && (failures == 0)
        val docRes: Doc =
          Doc.intercalate(Doc.line,
            results.map { case (p, (_, _, d)) =>
              Doc.text(p.asString) + Doc.char(':') + (Doc.lineOrSpace + d).nested(2)
            })

        if (success) print(docRes.render(80))
        else {
          val missingDoc =
            if (noTests.isEmpty) Nil
            else {
              val prefix = Doc.text("packages with missing tests: ")
              val missingDoc = Doc.intercalate(Doc.lineOrSpace, noTests.sorted.map { p => Doc.text(p.asString) })
              (prefix + missingDoc.nested(2)) :: Nil
            }

          val fullOut = Doc.intercalate(Doc.line + Doc.text("#######") + Doc.line, docRes :: missingDoc)

          val failureStr =
            if (failures == 1) "1 test failure"
            else s"$failures test failures"

          val missingCount = noTests.size
          val excepMessage =
            if (missingCount > 0) {
              val packString = if (missingCount == 1) "package" else "packages"
              s"$failureStr and $missingCount $packString with no tests found"
            }
            else failureStr

          print(fullOut.render(80)) *> IO.raiseError(new Exception(excepMessage))
        }
      case Output.EvaluationResult(res, tpe) =>
        IO.suspend {
          val r = res.value
          print(s"$res: $tpe")
        }
      case Output.JsonOutput(json, path) =>
        CodeGenWrite.writeDoc(path, json.toDoc)

      case Output.CompileOut(packList, ifout, output) =>
        val ifres = ifout match {
          case None =>
            IO.unit
          case Some(ifacePath) =>
            val ifs = packList.map(Package.interfaceOf(_))
            writeInterfaces(ifs, ifacePath)
        }
        val out = writePackages(packList, output)

        (ifres *> out)
    }

  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] = {
    import scala.collection.JavaConverters._

    def getP(p: Path): Option[PackageName] = {
      val subPath = p.relativize(packFile)
        .asScala
        .map { part =>
          part.toString.toLowerCase.capitalize
        }
        .mkString("/")

      PackageName.parse(subPath)
    }

    @annotation.tailrec
    def loop(roots: List[Path]): Option[PackageName] =
      roots match {
        case Nil => None
        case h :: t =>
          getP(h) match {
            case None => loop(t)
            case some => some
          }
      }

    loop(roots)
  }
}

object Main {
  def main(args: Array[String]): Unit =
    PathModule.run(args.toList) match {
      case Right(out) =>
        val run = out.flatMap(PathModule.reportOutput(_))
        Try(run.unsafeRunSync) match {
          case Failure(err) =>
            // TODO use some verbosity flag to modulate this
            //err.printStackTrace
            System.err.println(err.getMessage)
            System.exit(1)
          case Success(()) =>
            System.exit(0)
        }
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }
}
