package dev.bosatsu.cache

import cats.Applicative
import java.util.concurrent.atomic.AtomicLong

trait InferCache[F[_], K, V] {
  type Key

  def generateKey(input: K): F[Key]

  def get(key: Key): F[Option[V]]
  def put(key: Key, value: V): F[Unit]

  def statsSnapshot: Option[String] = None
}

object InferCache {
  def statsEnabled: Boolean =
    Option(System.getenv("BOSATSU_CACHE_STATS")).exists { raw =>
      val v = raw.trim
      v == "1" || v.equalsIgnoreCase("true") || v.equalsIgnoreCase("yes")
    }

  def noop[F[_]: Applicative, K, V]: InferCache[F, K, V] { type Key = Unit } =
    new InferCache[F, K, V] {
      type Key = Unit
      private val statsEnabled = InferCache.statsEnabled
      private val noneF: F[Option[V]] =
        Applicative[F].pure(None)
      private val generateKeyCalls = new AtomicLong(0L)
      private val getCalls = new AtomicLong(0L)
      private val putCalls = new AtomicLong(0L)

      def generateKey(input: K): F[Key] = {
        val _ = input
        if (statsEnabled) {
          generateKeyCalls.incrementAndGet()
          ()
        }
        Applicative[F].unit
      }

      def get(key: Key): F[Option[V]] = {
        if (statsEnabled) {
          getCalls.incrementAndGet()
          ()
        }
        noneF
      }

      def put(key: Key, value: V): F[Unit] = {
        if (statsEnabled) {
          putCalls.incrementAndGet()
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
          Some(
            s"[compile-cache noop] generateKeyCalls=$generateKeyCallsV getCalls=$getCallsV putCalls=$putCallsV"
          )
        }
    }
}
