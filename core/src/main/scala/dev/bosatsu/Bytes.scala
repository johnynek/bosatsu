package dev.bosatsu

import dev.bosatsu.hashing.{Algo, HashValue}
import java.nio.charset.StandardCharsets
import java.util.Arrays
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import scala.util.Try

/** Immutable bytes backed by an unboxed array. */
opaque type Bytes = Array[Byte]

object Bytes {
  val empty: Bytes = Array.emptyByteArray

  /** Copy caller-owned mutable storage. */
  def fromArray(bytes: Array[Byte]): Bytes = bytes.clone()

  /** Transfer ownership of a fresh array. The caller must never mutate it again. */
  private[bosatsu] def unsafeFromArray(bytes: Array[Byte]): Bytes = bytes

  def fromUTF8String(text: String): Bytes = text.getBytes(StandardCharsets.UTF_8)

  def fromMessage(message: GeneratedMessage): Bytes = message.toByteArray

  extension (bytes: Bytes) {
    def copyToArray: Array[Byte] = bytes.clone()

    def toList: List[Byte] = bytes.iterator.toList

    def sameBytes(other: Bytes): Boolean = Arrays.equals(bytes, other)

    private[bosatsu] def contentHashCode: Int = Arrays.hashCode(bytes)

    def hash[A](using algo: Algo[A]): HashValue[A] = algo.hashBytes(bytes)

    def toMessage[A <: GeneratedMessage](companion: GeneratedMessageCompanion[A]): Try[A] =
      Try(companion.parseFrom(bytes))

    /** For platform APIs that only read the array; never mutate or expose it. */
    private[bosatsu] def unsafeReadOnlyView: Array[Byte] = bytes
  }
}
