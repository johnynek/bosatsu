package dev.bosatsu

final case class CompileOptions(optimize: Boolean)

object CompileOptions {
  val Default: CompileOptions = CompileOptions(optimize = true)
  val NoOptimize: CompileOptions = CompileOptions(optimize = false)
}
