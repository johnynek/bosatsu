package org.bykn.edgemar

import fastparse.all._
import com.monovore.decline._
import java.nio.file.{Files, Path}

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
      Parser.expr.parse(str) match {
        case Parsed.Success(exp, _) =>
          Expr.evaluate(exp) match {
            case Right((t, tpe)) =>
              println(s"$t: $tpe")
            case Left(err) =>
              System.err.println(err)
              System.err.println(exp)
              System.exit(1)
          }
        case Parsed.Failure(exp, idx, extra) =>
          System.err.println(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
          System.exit(1)
      }
    }
  }
)
