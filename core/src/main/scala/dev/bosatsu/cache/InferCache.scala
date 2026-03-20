package dev.bosatsu.cache

import cats.Applicative
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.{CompileOptions, Package, PackageName}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.immutable.SortedMap

trait InferCache[F[_]] {
  type Key
  type DepHash

  def generateKey(
      packageName: PackageName,
      sourceHash: HashValue[Algo.Blake3],
      depInterfaceHashes: SortedMap[PackageName, DepHash],
      compileOptions: CompileOptions,
      compilerIdentity: String,
      phaseIdentity: String
  ): F[Key]

  def get(key: Key): F[Option[Package.Compiled]]
  def put(key: Key, value: Package.Compiled): F[Unit]
  def dependencyHash(interface: Package.Interface): F[DepHash]

  def statsSnapshot: Option[String] = None
}

object InferCache {
  def statsEnabled: Boolean =
    Option(System.getenv("BOSATSU_CACHE_STATS")).exists { raw =>
      val v = raw.trim
      v == "1" || v.equalsIgnoreCase("true") || v.equalsIgnoreCase("yes")
    }

  def noop[F[_]: Applicative]: InferCache[F] { type Key = Unit; type DepHash = Unit } =
    new InferCache[F] {
      type Key = Unit
      type DepHash = Unit
      private val statsEnabled = InferCache.statsEnabled
      private val noneF: F[Option[Package.Compiled]] =
        Applicative[F].pure(None)
      private val generateKeyCalls = new AtomicLong(0L)
      private val getCalls = new AtomicLong(0L)
      private val putCalls = new AtomicLong(0L)
      private val dependencyHashCalls = new AtomicLong(0L)

      def generateKey(
          packageName: PackageName,
          sourceHash: HashValue[Algo.Blake3],
          depInterfaceHashes: SortedMap[PackageName, DepHash],
          compileOptions: CompileOptions,
          compilerIdentity: String,
          phaseIdentity: String
      ): F[Key] = {
        val _ =
          (
            packageName,
            sourceHash,
            depInterfaceHashes,
            compileOptions,
            compilerIdentity,
            phaseIdentity
          )
        if (statsEnabled) {
          generateKeyCalls.incrementAndGet()
          ()
        }
        Applicative[F].unit
      }

      def get(key: Key): F[Option[Package.Compiled]] = {
        if (statsEnabled) {
          getCalls.incrementAndGet()
          ()
        }
        noneF
      }

      def put(key: Key, value: Package.Compiled): F[Unit] = {
        if (statsEnabled) {
          putCalls.incrementAndGet()
          ()
        }
        Applicative[F].unit
      }

      def dependencyHash(interface: Package.Interface): F[DepHash] = {
        if (statsEnabled) {
          dependencyHashCalls.incrementAndGet()
          ()
        }
        Applicative[F].unit
      }

      override def statsSnapshot: Option[String] =
        if (!statsEnabled) None
        else {
          val generateKeyCallsV = generateKeyCalls.get()
          val getCallsV = getCalls.get()
          val putCallsV = putCalls.get()
          val dependencyHashCallsV = dependencyHashCalls.get()
          Some(
            s"[compile-cache noop] generateKeyCalls=$generateKeyCallsV getCalls=$getCallsV putCalls=$putCallsV dependencyHashCalls=$dependencyHashCallsV"
          )
        }
    }
}
