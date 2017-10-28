package org.bykn.edgemar

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
      Statement.infer(str).foreach {
        case (v, tpe) =>
          println(s"$v: $tpe")
      }
    }
  }
)
