package dev.bosatsu

final case class CompileOptions(
    optimize: Boolean,
    mode: CompileOptions.Mode = CompileOptions.Mode.Emit
)

object CompileOptions {
  enum Mode derives CanEqual {
    case Emit, TypeCheckOnly
  }

  val Default: CompileOptions =
    CompileOptions(optimize = true, mode = Mode.Emit)
  val NoOptimize: CompileOptions =
    CompileOptions(optimize = false, mode = Mode.Emit)
  val TypeCheckOnly: CompileOptions =
    CompileOptions(optimize = false, mode = Mode.TypeCheckOnly)
}
