package dev.bosatsu.cache

import dev.bosatsu.{
  CompileOptions,
  Package,
  TypedExprLoopRecurLowering,
  TypedExprNormalization
}
import scala.collection.immutable.SortedMap

trait InferPhases {
  def id: String

  def dependencyInterface(pack: Package.Typed[Any]): Package.Interface

  def finishPackage(
      pack: Package.Inferred,
      depIfaces: SortedMap[dev.bosatsu.PackageName, Package.Interface],
      compileOptions: CompileOptions
  ): Package.Inferred
}

object InferPhases {
  val default: InferPhases =
    new InferPhases {
      val id: String = "tool-default-v1"

      def dependencyInterface(pack: Package.Typed[Any]): Package.Interface =
        Package.interfaceOf(pack)

      def finishPackage(
          pack: Package.Inferred,
          _depIfaces: SortedMap[dev.bosatsu.PackageName, Package.Interface],
          compileOptions: CompileOptions
      ): Package.Inferred = {
        val lowered =
          if (compileOptions.enables(CompileOptions.TypedPass.LoopRecurLowering)) {
            val loweredProgram =
              TypedExprLoopRecurLowering.lowerProgram(pack.program._1)
            pack.copy(program = (loweredProgram, pack.program._2))
          } else {
            pack
          }

        val normalized =
          if (compileOptions.enables(CompileOptions.TypedPass.Normalize)) {
            val loweredProgram = lowered.program._1
            lowered.copy(program =
              (
                TypedExprNormalization.normalizeProgram(
                  lowered.name,
                  loweredProgram.types,
                  loweredProgram
                ),
                lowered.program._2
              )
            )
          } else {
            lowered
          }

        if (compileOptions.enables(CompileOptions.TypedPass.DiscardUnused))
          Package.discardUnused(normalized)
        else
          normalized
      }
    }
}
