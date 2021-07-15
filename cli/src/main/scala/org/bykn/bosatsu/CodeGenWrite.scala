package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.effect.IO
import java.nio.file.Path
import java.io.PrintWriter
import org.typelevel.paiges.Doc

object CodeGenWrite {
  @annotation.tailrec
  final def toPath(root: Path, pn: PackageName): Path =
    pn.parts match {
      case NonEmptyList(h, Nil) => root.resolve(h).resolve("Values.java")
      case NonEmptyList(h0, h1 :: tail) =>
        toPath(root.resolve(h0), PackageName(NonEmptyList(h1, tail)))
    }

  def writeDoc(p: Path, d: Doc): IO[Unit] =
    IO {
      Option(p.getParent).foreach(_.toFile.mkdirs)
      val pw = new PrintWriter(p.toFile, "UTF-8")
      try d.renderStream(100).foreach(pw.print(_))
      finally {
        pw.close
      }
    }
}
