package dev.bosatsu.tool

sealed abstract class ExitCode(val toInt: Int) derives CanEqual
object ExitCode {
  case object Success extends ExitCode(0)
  case object Error extends ExitCode(1)
}
