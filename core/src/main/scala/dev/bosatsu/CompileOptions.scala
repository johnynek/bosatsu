package dev.bosatsu

final case class CompileOptions(
    typedPasses: Set[CompileOptions.TypedPass],
    mode: CompileOptions.Mode = CompileOptions.Mode.Emit
) {
  def optimize: Boolean =
    typedPasses.nonEmpty

  def enables(pass: CompileOptions.TypedPass): Boolean =
    typedPasses(pass)

  def enabledTypedPasses: List[CompileOptions.TypedPass] =
    CompileOptions.TypedPass.ordered.filter(typedPasses)
}

object CompileOptions {
  enum Mode derives CanEqual {
    case Emit, TypeCheckOnly
  }

  enum TypedPass(val cliName: String) derives CanEqual {
    case LoopRecurLowering extends TypedPass("loop-recur-lowering")
    case Normalize extends TypedPass("normalize")
    case DiscardUnused extends TypedPass("discard-unused")
  }
  object TypedPass {
    val ordered: List[TypedPass] = values.toList
    val defaultSet: Set[TypedPass] = ordered.toSet

    def fromCliName(name: String): Option[TypedPass] =
      ordered.find(_.cliName == name)
  }

  def fromDisabledTypedPasses(
      disabled: Set[TypedPass],
      mode: Mode = Mode.Emit
  ): CompileOptions =
    CompileOptions(TypedPass.defaultSet -- disabled, mode)

  val Default: CompileOptions =
    CompileOptions(TypedPass.defaultSet, Mode.Emit)
  val NoOptimize: CompileOptions =
    CompileOptions(Set.empty, Mode.Emit)
  val TypeCheckOnly: CompileOptions =
    CompileOptions(Set.empty, Mode.TypeCheckOnly)
}
