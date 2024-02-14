package org.bykn.bosatsu.jsui

import scala.concurrent.duration.Duration

sealed abstract class Action

object Action {
  case class CodeEntered(text: String) extends Action
  case object RunCompile extends Action
  case class CompileCompleted(result: String, duration: Duration) extends Action
}