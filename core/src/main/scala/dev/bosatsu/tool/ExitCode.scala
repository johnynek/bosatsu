package dev.bosatsu.tool

sealed abstract class ExitCode(val toInt: Int) derives CanEqual
object ExitCode {
  case object Success extends ExitCode(0)
  case object Error extends ExitCode(1)
  final case class Other(override val toInt: Int) extends ExitCode(toInt)

  def fromInt(code: Int): ExitCode =
    code match {
      case 0 => Success
      case 1 => Error
      case n => Other(n)
    }
}
