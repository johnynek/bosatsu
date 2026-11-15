package dev.bosatsu

import java.nio.charset.StandardCharsets
import scalapb.GeneratedMessage

/** Immutable serialized bytes, backed by an unboxed array. */
opaque type Bytes = Array[Byte]

object Bytes {
  val empty: Bytes = Array.emptyByteArray

  def fromUTF8String(value: String): Bytes =
    value.getBytes(StandardCharsets.UTF_8)

  def fromMessage(message: GeneratedMessage): Bytes = message.toByteArray

  /** Copy caller-owned mutable storage. */
  def copyFromArray(value: Array[Byte]): Bytes = value.clone()

  /** Take ownership of fresh storage. The caller must not retain a mutable alias. */
  private[bosatsu] def unsafeFromArray(value: Array[Byte]): Bytes = value

  extension (bytes: Bytes) {
    def copyToArray: Array[Byte] = bytes.clone()

    def sameBytes(other: Bytes): Boolean = java.util.Arrays.equals(bytes, other)

    def contentHashCode: Int = java.util.Arrays.hashCode(bytes)

    /** Zero-copy view for read-only foreign APIs. The caller must neither
      * mutate this array nor pass it to an API that may mutate it.
      */
    private[bosatsu] def unsafeReadOnlyView: Array[Byte] = bytes
  }
}
