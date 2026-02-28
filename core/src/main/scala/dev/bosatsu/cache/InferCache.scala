package dev.bosatsu.cache

import cats.Applicative
import dev.bosatsu.{CompileOptions, Package, PackageName}

trait InferCache[F[_]] {
  type Key

  def generateKey(
      pack: Package.Parsed,
      depInterfaces: List[(PackageName, Package.Interface)],
      compileOptions: CompileOptions,
      compilerIdentity: String,
      phaseIdentity: String
  ): F[Key]

  def get(key: Key): F[Option[Package.Inferred]]
  def put(key: Key, value: Package.Inferred): F[Unit]
}

object InferCache {
  def noop[F[_]: Applicative]: InferCache[F] { type Key = Unit } =
    new InferCache[F] {
      type Key = Unit

      def generateKey(
          pack: Package.Parsed,
          depInterfaces: List[(PackageName, Package.Interface)],
          compileOptions: CompileOptions,
          compilerIdentity: String,
          phaseIdentity: String
      ): F[Key] =
        Applicative[F].pure(())

      def get(key: Key): F[Option[Package.Inferred]] =
        Applicative[F].pure(None)

      def put(key: Key, value: Package.Inferred): F[Unit] =
        Applicative[F].unit
    }
}
