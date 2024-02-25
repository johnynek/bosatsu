package org.bykn.bosatsu.jsui

import scala.concurrent.duration.Duration

sealed abstract class Action

object Action {
  sealed abstract class Cmd
  object Cmd {
    case object Eval extends Cmd
    case object Test extends Cmd
    case object Show extends Cmd
  }
  case class CodeEntered(text: String) extends Action
  case class Run(cmd: Cmd) extends Action
  case class CmdCompleted(result: String, duration: Duration, cmd: Cmd)
      extends Action
}
