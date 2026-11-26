package dev.bosatsu

import scala.scalajs.js.typedarray.{
  Int8Array,
  Uint8Array,
  byteArray2Int8Array,
  int8Array2ByteArray
}

/** Copies between Bytes and JavaScript byte arrays such as Node Buffers.
  *
  * Scala.js stores an Array[Byte] in an Int8Array, so whole-array copies are
  * typed-array copies, never per-byte loops through js.Dynamic.
  */
private[bosatsu] object JsBytes {

  /** Copy a Uint8Array, such as a Node Buffer, into fresh Bytes.
    *
    * A Buffer may be a view into a larger shared pool, so this copies exactly
    * its `byteOffset` and `length` window.
    */
  def fromUint8Array(view: Uint8Array): Bytes =
    Bytes.unsafeFromArray(
      int8Array2ByteArray(new Int8Array(view.buffer, view.byteOffset, view.length))
    )

  /** Copy Bytes into a fresh Uint8Array that the caller may keep or mutate. */
  def toUint8Array(bytes: Bytes): Uint8Array = {
    val signed = byteArray2Int8Array(bytes.unsafeReadOnlyView)
    new Uint8Array(signed.buffer, signed.byteOffset, signed.length)
  }

  /** Copy `length` bytes starting at `offset` into a fresh Uint8Array.
    *
    * A whole-array range uses the typed-array copy. A narrower range copies
    * only its own bytes, so a small slice of large storage stays cheap.
    */
  def toUint8Array(bytes: Bytes, offset: Int, length: Int): Uint8Array =
    if (offset == 0 && length == bytes.length) toUint8Array(bytes)
    else {
      val out = new Uint8Array(length)
      var index = 0
      while (index < length) {
        out(index) = (bytes(offset + index) & 0xff).toShort
        index += 1
      }
      out
    }
}
