package dev.bosatsu.hashing

import java.security.MessageDigest

object Sha256Hash {

  // Precomputed hexadecimal characters
  private val hexArray = "0123456789abcdef".toCharArray

  def sha256HashHex(bytes: Array[Byte]): String = {
    // Create a SHA-256 MessageDigest instance
    val digest = MessageDigest.getInstance("SHA-256")

    // Compute the hash of the input bytes
    val hashBytes = digest.digest(bytes)

    // Each byte produces two hex characters
    val resultChars = new Array[Char](hashBytes.length * 2)

    var i = 0
    while (i < hashBytes.length) {
      val v = hashBytes(i) & 0xff
      resultChars(i * 2) = hexArray(v >>> 4) // high nibble
      resultChars(i * 2 + 1) = hexArray(v & 0x0f) // low nibble
      i += 1
    }
    new String(resultChars)
  }
}
