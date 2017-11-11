package org.bykn.edgemar

import com.monovore.decline._
import java.nio.file.{Files, Path}
import fastparse.all._

object Foo {
  def times(i: java.lang.Integer): java.lang.Integer =
    java.lang.Integer.valueOf(i.intValue + 42)
}

trait Fn[A, B] {
  def apply(arg: A): B
}

object Std {
  //fold = ffi scala org.bykn.edgemar.Std.fold List[a] -> b -> (b -> a -> b) -> b
  @annotation.tailrec
  final def fold(list: Any, bv: Any, fn: Any): Any = {
    list match {
      case (0, _) =>
        // Empty
        bv
      case (1, head :: tail :: Nil) =>
        val fnT = fn.asInstanceOf[Fn[Any, Fn[Any, Any]]]
        fold(tail, fnT(bv)(head), fn)
      case _ => sys.error(s"unexpected: $list")
    }
  }
}

object Main extends CommandApp(
  name = "edgemar",
  header = "a total language",
  main = {
    val opt = Opts.option[Path]("input", help = "file to execute")
    opt.map { path =>
      val str = new String(Files.readAllBytes(path), "utf-8")
      Statement.parser.parse(str) match {
        case Parsed.Success(exp, _) =>
          val prog = exp.toProgram
          Evaluation.evaluateProgram(prog) match {
            case None => sys.error("found no main expression")
            case Some(Left(err)) => sys.error(s"TypeError: $err")
            case Some(Right((res, scheme))) =>
              println(s"$res: ${scheme.result}")
          }
        case Parsed.Failure(exp, idx, extra) =>
          sys.error(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
      }
    }
  }
)
