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

  def dependencyInterface(pack: Package.Inferred): Package.Interface

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

      def dependencyInterface(pack: Package.Inferred): Package.Interface =
        Package.interfaceOf(pack)

      def finishPackage(
          pack: Package.Inferred,
          _depIfaces: SortedMap[dev.bosatsu.PackageName, Package.Interface],
          compileOptions: CompileOptions
      ): Package.Inferred =
        if (compileOptions.optimize) {
          val loweredProgram =
            TypedExprLoopRecurLowering.lowerProgram(pack.program._1)
          val normalized =
            pack.copy(program =
              (
                TypedExprNormalization.normalizeProgram(
                  pack.name,
                  pack.program._1.types,
                  loweredProgram
                ),
                pack.program._2
              )
            )
          Package.discardUnused(normalized)
        } else {
          pack
        }
    }
}
