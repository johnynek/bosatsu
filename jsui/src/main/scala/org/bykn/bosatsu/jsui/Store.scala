package org.bykn.bosatsu.jsui

import cats.effect.{IO, Resource}
import org.bykn.bosatsu.{MemoryMain, rankn}
import org.typelevel.paiges.Doc
import org.scalajs.dom.window.localStorage

object Store {
  val memoryMain = new MemoryMain[Either[Throwable, *], String](_.split("/", -1).toList)

  def runCompile(str: String): IO[String] = IO.blocking {
    val start = System.currentTimeMillis()
    println(s"starting compiling: $start")
    val res = memoryMain.runWith(
      files = Map("root/WebDemo" -> str)
    )(List(
      "eval", "--input", "root/WebDemo", "--package_root", "root",
      "--main_file", "root/WebDemo", "--color", "html"
    )) match {
      case Left(err) =>
        memoryMain.mainExceptionToString(err) match {
          case Some(e) => e
          case None => s"unknown error: $err"
        }
      case Right(memoryMain.Output.EvaluationResult(_, tpe, resDoc)) =>
          val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
          val doc = resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
        doc.render(80)
      case Right(other) =>
        s"internal error. got unexpected result: $other"
    }
    val end = System.currentTimeMillis()
    println(s"finished compiling in ${end - start}ms")
    res
  }

  def stateSetter(st: State): IO[Unit] =
    IO {
      localStorage.setItem("state", State.stateToJsonString(st))
    }

  def initialState: IO[State] =
    IO(localStorage.getItem("state")).flatMap { init =>
      if (init == null) IO.pure(State.Init)
      else IO.fromEither(State.stringToState(init))
    }

  val value: Resource[IO, ff4s.Store[IO, State, Action]] =
    for {
    init <- Resource.liftK(initialState)
    store <- ff4s.Store[IO, State, Action](init) { store =>
      {
        case Action.CodeEntered(text) =>
          {
            case State.Init | State.WithText(_) => (State.WithText(text), None)
            case c @ State.Compiling(_) => (c, None)
            case comp @ State.Compiled(_, _, _) => (comp.copy(editorText = text), None)
          }

        case Action.RunCompile =>
          {
            case State.Init => (State.Init, None)
            case c @ State.Compiling(_) => (c, None)
            case ht: State.HasText =>
              val action =
                for {
                  _ <- stateSetter(ht)
                  start <- IO.monotonic
                  output <- runCompile(ht.editorText)
                  end <- IO.monotonic
                  _ <- store.dispatch(Action.CompileCompleted(output, end - start))
                } yield ()

              (State.Compiling(ht), Some(action))
          }
        case Action.CompileCompleted(result, dur) =>
          {
            case State.Compiling(ht) =>
              val next = State.Compiled(ht.editorText, result, dur)
              (next, Some(stateSetter(next)))
            case unexpected =>
              // TODO send some error message
              println(s"unexpected Complete: $result => $unexpected")
              (unexpected, None)
          }
      }  
    }
  } yield store
}