package org.bykn.bosatsu.jsui

import ff4s.Dsl
import org.bykn.bosatsu.jsui.State.Compiled
import org.scalajs.dom.HTMLTextAreaElement

trait View { dsl: Dsl[State, Action] =>
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

    div(cls := "grid-container", aboveCode, aboveOut, codeBox, outBox)
  }
}
