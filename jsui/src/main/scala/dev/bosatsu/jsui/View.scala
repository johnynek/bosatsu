package dev.bosatsu.jsui

import ff4s.Dsl
import dev.bosatsu.CompilerBuildInfo
import dev.bosatsu.jsui.State.Compiled
import org.scalajs.dom.HTMLTextAreaElement

trait View { dsl: Dsl[State, Action] =>
  def buildInfo: CompilerBuildInfo

  val view = {
    import html._

    val aboveCode =
      div(cls := "grid-item above-code", "Bosatsu Code")

    val aboveOut =
      div(cls := "grid-item above-out", "Output")

    val codeBox = dsl.useState { state =>
      val text = state match {
        case ht: State.HasText => ht.editorText
        case _                 => ""
      }
      div(
        cls := "grid-item code-col",
        button("evaluate", onClick := (_ => Some(Action.Run(Action.Cmd.Eval)))),
        button("test", onClick := (_ => Some(Action.Run(Action.Cmd.Test)))),
        button("show", onClick := (_ => Some(Action.Run(Action.Cmd.Show)))),
        textArea(
          `type` := "text",
          cls := "codein",
          value := text,
          onInput := { te =>
            Some(
              Action.CodeEntered(
                te.currentTarget.asInstanceOf[HTMLTextAreaElement].value
              )
            )
          }
        )
      )
    }

    val outBox = dsl.useState {
      case Compiled(_, output, dur) =>
        div(
          cls := "grid-item out-col",
          literal(s"<pre>$output</pre>"),
          br(),
          "completed in ",
          dur.toMillis.toString,
          " ms"
        )
      case _ =>
        div(cls := "grid-item out-col")
    }

    val buildInfoLine = {
      val version = s"Bosatsu ${buildInfo.version}"
      buildInfo.gitHeadCommit match {
        case Some(sha) =>
          div(
            cls := "build-info",
            version,
            " at commit ",
            a(
              href := s"https://github.com/johnynek/bosatsu/commit/$sha",
              sha.take(12)
            )
          )
        case None =>
          div(cls := "build-info", version)
      }
    }

    div(
      cls := "grid-container",
      aboveCode,
      aboveOut,
      codeBox,
      outBox,
      buildInfoLine
    )
  }
}
