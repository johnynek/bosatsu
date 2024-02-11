package org.bykn.bosatsu.jsui

import cats.effect.{IO, IOApp, ExitCode}
import colibri.Subject
import org.bykn.bosatsu.{MemoryMain, rankn}
import org.scalajs.dom.ext.KeyCode
import org.typelevel.paiges.Doc
import outwatch.Outwatch
import outwatch.dsl._

object Main extends IOApp {
  val memoryMain = new MemoryMain[Either[Throwable, *], String](_.split("/", -1).toList)


  def evalAndShow(str: String): String = {
    val start = System.currentTimeMillis()
    println(s"starting compiling: $start")
    val res = memoryMain.runWith(
      files = Map("root/WebDemo" -> str)
    )(List(
      "eval", "--input", "root/WebDemo", "--package_root", "root",
      "--main_file", "root/WebDemo", "--color", "none"
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

  def run(args: List[String]): IO[ExitCode] = {

    val editingCode = Subject.behavior("")
    val output = Subject.behavior("")

    // Emitterbuilders can be extracted and reused!
    val onEnter = onKeyDown
        .filter(e => e.keyCode == KeyCode.Enter)
        .preventDefault
    assert(onEnter ne null)

    val aboveCode =
      div(cls := "grid-item", "Bosatsu Code")

    val aboveOut =
      div(cls := "grid-item", "Output")

    val codeBox =
      div(cls := "grid-item",
          button("evaluate",
            onClick(editingCode) -->
            // in a pure system, x.map(f) --> y
            // vs x --> y.contramap(f)
            // would be the same thing, but not here
                output.contramap(evalAndShow)
          ),
          textArea(`type` := "text",
            cls := "codein",
            onInput.value --> editingCode
          ),
      )

    val outBox =
      div(cls := "grid-item", pre(output))
    
    val component =
      div(cls := "grid-container",
        aboveCode,
        aboveOut,
        codeBox,
        outBox)

    for {
      _ <- Outwatch.renderReplace[IO]("#app", component)
    } yield ExitCode.Success
  }
}