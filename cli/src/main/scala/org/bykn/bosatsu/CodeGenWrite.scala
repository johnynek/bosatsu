package org.bykn.bosatsu

import cats.data.NonEmptyList
import java.nio.file.Path
import java.io.PrintWriter
import org.typelevel.paiges.Doc
import scala.util.Try

import cats.implicits._
import alleycats.std.map._ // TODO use SortedMap everywhere

object CodeGenWrite {
  @annotation.tailrec
  final def toPath(root: Path, pn: PackageName): Path =
    pn.parts match {
      case NonEmptyList(h, Nil) => root.resolve(h).resolve("Values.java")
      case NonEmptyList(h0, h1 :: tail) =>
        toPath(root.resolve(h0), PackageName(NonEmptyList(h1, tail)))
    }

  def writeDoc(p: Path, d: Doc): Try[Unit] =
    Try {
      Option(p.getParent).foreach(_.toFile.mkdirs)
      val pw = new PrintWriter(p.toFile, "UTF-8")
      val res = Try {
        d.renderStream(80).foreach(pw.print(_))
      }
      pw.close
      res
    }
    .flatten

  def write(root: Path, packages: PackageMap.Inferred, ext: Externals): Try[Unit] = {
    val cg = new CodeGen { }
    packages.toMap.traverse_ { pack =>
      val (d, _) = CodeGen.run(cg.genPackage(pack, ext))
      val path = toPath(root, pack.name)
      writeDoc(path, d)
    }
  }
}
