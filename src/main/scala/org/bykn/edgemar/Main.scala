package org.bykn.edgemar

import cats.Eval
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

  def unitValue: AnyRef = (0, Nil)

  def print(i: java.lang.Integer): Any =
    Eval.always { println(i.toString); unitValue }

  def flatMap(act: Any, fn: Any): Any =
    act.asInstanceOf[Eval[Any]].flatMap { v =>
      fn.asInstanceOf[Fn[Any, Eval[Any]]](v)
    }
  def mapAction(act: Any, fn: Any): Any =
    act.asInstanceOf[Eval[Any]].map { v =>
      fn.asInstanceOf[Fn[Any, Any]](v)
    }

  def toAction(a: Any): Any = Eval.now(a)
  def runAction(a: Any): Any = a.asInstanceOf[Eval[Any]].value
}

object Main extends CommandApp(
  name = "edgemar",
  header = "a total language",
  main = {
    val opt = Opts.option[Path]("input", help = "file to execute")
    opt.map { path =>
      val str = new String(Files.readAllBytes(path), "utf-8")
      Package.parser.parse(str) match {
        case Parsed.Success(exp, _) =>
          val prog = exp.body.toProgram
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
