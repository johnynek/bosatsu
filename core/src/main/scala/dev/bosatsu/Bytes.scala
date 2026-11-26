package dev.bosatsu

import cats.{Hash, Order, Show}
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, StandardCharsets}
import java.util.Arrays
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

  // Share Eq between Hash and Order so implicit Eq lookup is unambiguous.
  given bytesInstances: (Hash[Bytes] & Order[Bytes]) =
    new Hash[Bytes] with Order[Bytes] {
      override def eqv(left: Bytes, right: Bytes): Boolean = left.sameBytes(right)

      def hash(bytes: Bytes): Int = bytes.contentHashCode

      /** Lexicographic order of unsigned bytes, with shorter prefixes first. */
      def compare(left: Bytes, right: Bytes): Int = {
        val limit = math.min(left.length, right.length)
        var index = 0
        while (index < limit) {
          val diff = (left(index) & 0xff) - (right(index) & 0xff)
          if (diff != 0) return diff
          index += 1
        }
        left.length - right.length
      }
    }

  /** Render binary content as lowercase hexadecimal. */
  given Show[Bytes] = Show.show { bytes =>
    val digits = "0123456789abcdef"
    val result = new StringBuilder(bytes.length * 2)
    var index = 0
    while (index < bytes.length) {
      val byte = bytes(index) & 0xff
      result.append(digits.charAt(byte >>> 4))
      result.append(digits.charAt(byte & 0x0f))
      index += 1
    }
    result.toString
  }

  extension (bytes: Bytes) {
    def length: Int = bytes.length

    /** Read one byte without copying. Reads cannot change the storage. */
    def apply(index: Int): Byte = bytes(index)

    /** Decode strict UTF-8: malformed input is an error, never replaced. */
    def decodeUtf8: Either[CharacterCodingException, String] =
      decodeUtf8Slice(0, bytes.length)

    /** Decode `length` bytes starting at `offset` as strict UTF-8. */
    def decodeUtf8Slice(
        offset: Int,
        length: Int
    ): Either[CharacterCodingException, String] =
      try {
        val buffer = ByteBuffer.wrap(bytes, offset, length)
        Right(StandardCharsets.UTF_8.newDecoder().decode(buffer).toString)
      } catch { case error: CharacterCodingException => Left(error) }

    def copyToArray: Array[Byte] = bytes.clone()

    def sameBytes(other: Bytes): Boolean = Arrays.equals(bytes, other)

    def contentHashCode: Int = Arrays.hashCode(bytes)

    /** Zero-copy view for read-only foreign APIs. The caller must neither
      * mutate this array nor pass it to an API that may mutate it.
      */
    private[bosatsu] def unsafeReadOnlyView: Array[Byte] = bytes
  }
}
