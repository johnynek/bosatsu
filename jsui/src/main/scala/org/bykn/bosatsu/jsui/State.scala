package org.bykn.bosatsu.jsui

import scala.concurrent.duration.Duration

sealed trait State

object State {
  sealed trait HasText extends State {
    def editorText: String
  }
  case object Init extends State
  case class WithText(
    editorText: String
  ) extends HasText

  case class Compiling(previousState: HasText) extends State

  case class Compiled(
    editorText: String,
    output: String,
    compilationTime: Duration
  ) extends HasText

  def init: State = Init
}