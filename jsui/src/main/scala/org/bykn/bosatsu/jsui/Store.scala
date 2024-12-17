package org.bykn.bosatsu.jsui

import cats.effect.{IO, Resource}
import org.bykn.bosatsu.{MemoryMain, Package, Test, rankn}
import org.bykn.bosatsu.tool.Output
import org.typelevel.paiges.{Doc, Document}
import org.scalajs.dom.window.localStorage

import Action.Cmd

object Store {
  private val splitFn: String => List[String] = { s => s.split("/", -1).toList }

  type HandlerFn = Output[String] => String
  def cmdHandler(cmd: Cmd): (List[String], HandlerFn) =
    cmd match {
      case Cmd.Eval =>
        val args = List(
          "eval",
          "--input",
          "root/WebDemo",
          "--package_root",
          "root",
          "--main_file",
          "root/WebDemo",
          "--color",
          "html"
        )

        val handler: HandlerFn = {
          case Output.EvaluationResult(_, tpe, resDoc) =>
            val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
            val doc =
              resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
            doc.render(80)
          case other =>
            s"internal error. got unexpected result: $other"
        }
        (args, handler)
      case Cmd.Test =>
        val args = List(
          "test",
          "--input",
          "root/WebDemo",
          "--package_root",
          "root",
          "--test_file",
          "root/WebDemo",
          "--color",
          "html"
        )
        val handler: HandlerFn = {
          case Output.TestOutput(resMap, color) =>
            val evaluatedTests = resMap.map { case (p, opt) =>
              (p, opt.map(_.value))
            }
            val testReport = Test.outputFor(evaluatedTests, color)
            testReport.doc.render(80)
          case other =>
            s"internal error. got unexpected result: $other"
        }
        (args, handler)
      case Cmd.Show =>
        val args = List(
          "show",
          "--input",
          "root/WebDemo",
          "--package_root",
          "root",
          "--color",
          "html"
        )
        val handler: HandlerFn = {
          case Output.ShowOutput(packs, ifaces, _) =>
            val pdocs = packs.map { pack =>
              Document[Package.Typed[Any]].document(pack)
            }
            val idocs = ifaces.map { iface =>
              Document[Package.Interface].document(iface)
            }

            val doc = Doc.intercalate(Doc.hardLine, idocs ::: pdocs)
            doc.render(80)
          case other =>
            s"internal error. got unexpected result: $other"
        }
        (args, handler)
    }

  def run(cmd: Cmd, str: String): IO[String] = IO.blocking {
    val start = System.currentTimeMillis()
    println(s"starting $cmd: $start")
    val (args, handler) = cmdHandler(cmd)

    val state = MemoryMain.stateFrom(Map("root/WebDemo" -> str))
    val memoryMain = MemoryMain(state)(splitFn)
    val res = memoryMain.run(args) match {
      case Right(io) =>
        io.attempt.map {
          case Right(good) => handler(good)
          case Left(err) =>
            memoryMain.mainExceptionToString(err) match {
              case Some(e) => e
              case None    => s"unknown error: $err"
            }
          }
      case Left(help) =>
        s"should never get help: $help"
        Right(good) => handler(good)
    }

    val end = System.currentTimeMillis()
    println(s"finished $cmd in ${end - start}ms")
    res
  }
  .flatten

  def stateSetter(st: State): IO[Unit] =
    IO.blocking {
      localStorage.setItem("state", State.stateToJsonString(st))
    }

  def initialState: IO[State] =
    IO.blocking(localStorage.getItem("state")).flatMap { init =>
      if (init == null) IO.pure(State.Init)
      else
        (State.stringToState(init) match {
          case Right(s) => IO.pure(s)
          case Left(err) =>
            IO.println(s"could not deserialize:\n\n$init\n\n$err")
              .as(State.Init)
        })
    }

  val value: Resource[IO, ff4s.Store[IO, State, Action]] =
    for {
      init <- Resource.liftK(initialState)
      store <- ff4s.Store[IO, State, Action](init) { store =>
        {
          case (Action.CodeEntered(text), state) =>
            state match {
              case State.Init | State.WithText(_) =>
                (State.WithText(text), IO.unit)
              case c @ State.Compiling(_) => (c, IO.unit)
              case comp @ State.Compiled(_, _, _) =>
                (comp.copy(editorText = text), IO.unit)
            }

          case (Action.Run(cmd), state) =>
            state match {
              case State.Init             => (State.Init, IO.unit)
              case c @ State.Compiling(_) => (c, IO.unit)
              case ht: State.HasText =>
                val action =
                  for {
                    _ <- stateSetter(ht)
                    start <- IO.monotonic
                    output <- run(cmd, ht.editorText)
                    end <- IO.monotonic
                    _ <- store.dispatch(
                      Action.CmdCompleted(output, end - start, cmd)
                    )
                  } yield ()

                (State.Compiling(ht), action)
            }
          case (Action.CmdCompleted(result, dur, _), state) =>
            state match {
              case State.Compiling(ht) =>
                val next = State.Compiled(ht.editorText, result, dur)
                (next, stateSetter(next))
              case unexpected =>
                // TODO send some error message
                (
                  unexpected,
                  IO.println(s"unexpected Complete: $result => $unexpected")
                )
            }
        }
      }
    } yield store
}
