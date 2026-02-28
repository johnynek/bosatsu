package dev.bosatsu.smt

final case class SmtScript(commands: Vector[SmtCommand]) {
  def ++(other: SmtScript): SmtScript =
    SmtScript(commands ++ other.commands)

  def append(cmd: SmtCommand): SmtScript =
    SmtScript(commands :+ cmd)
}
object SmtScript {
  val empty: SmtScript = SmtScript(Vector.empty)
}
