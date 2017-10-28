package org.bykn.edgemar

import fastparse.all._
import com.monovore.decline._
import java.nio.file.{Files, Path}
import org.typelevel.paiges.Document

object Foo {
  def times(i: java.lang.Integer): java.lang.Integer =
    java.lang.Integer.valueOf(i.intValue + 42)
}

object Main extends CommandApp(
  name = "edgemar",
  header = "a total language",
  main = {
    val opt = Opts.option[Path]("input", help = "file to execute")
    opt.map { path =>
      val str = new String(Files.readAllBytes(path), "utf-8")
      Statement.parser.parse(str) match {
        case Parsed.Success(stmts, _) =>
          // todo: re-add evaluation
          println(s"# $stmts")
          println(Document[Statement].document(stmts).render(80))
        case Parsed.Failure(exp, idx, extra) =>
          System.err.println(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
          System.exit(1)
      }
    }
  }
)
