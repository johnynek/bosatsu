package org.bykn.bosatsu.hashing

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("js-sha256", JSImport.Namespace)
object JsSha256 extends js.Object {
  def sha256(input: js.Any): String = js.native
}

object Sha256Hash {
  def sha256HashHex(bytes: Array[Byte]): String = {
    // Convert Array[Byte] to Uint8Array
    val uint8 = new Uint8Array(bytes.length)
    for (i <- bytes.indices) {
      // Mask with 0xFF to properly handle sign extension of bytes
      uint8(i) = (bytes(i) & 0xff).toShort
    }
    // Compute and return the hexadecimal SHA-256 hash
    JsSha256.sha256(uint8)
  }
}
