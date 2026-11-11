package dev.bosatsu.hashing

/** A portable, synchronous SHA-256 implementation shared by the JVM and JS builds. */
object Sha256Hash {
  private val roundConstants = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  )

  private val initialState = Array(
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
  )

  private val hexDigits = "0123456789abcdef".toCharArray

  private def rotateRight(value: Int, bits: Int): Int =
    (value >>> bits) | (value << (32 - bits))

  /** Mutable state is deliberately package-private and never returned from the public API. */
  private[hashing] final class Hasher private[hashing] (
      private val state: Array[Int],
      private val buffer: Array[Byte],
      private var bufferedBytes: Int,
      private var byteCount: Long
  ) {
    private val schedule = new Array[Int](64)

    private[hashing] def this() =
      this(initialState.clone(), new Array[Byte](64), 0, 0L)

    def update(bytes: Array[Byte], offset: Int, len: Int): Unit = {
      if (offset < 0 || len < 0 || offset > bytes.length - len)
        throw new IndexOutOfBoundsException(
          s"invalid SHA-256 input slice: offset=$offset length=$len arrayLength=${bytes.length}"
        )

      byteCount += len.toLong
      var inputOffset = offset
      var remaining = len

      if (bufferedBytes > 0 && remaining > 0) {
        val copied = math.min(64 - bufferedBytes, remaining)
        copyBytes(bytes, inputOffset, buffer, bufferedBytes, copied)
        bufferedBytes += copied
        inputOffset += copied
        remaining -= copied
        if (bufferedBytes == 64) {
          compress(buffer, 0)
          bufferedBytes = 0
        }
      }

      while (remaining >= 64) {
        compress(bytes, inputOffset)
        inputOffset += 64
        remaining -= 64
      }

      if (remaining > 0) {
        copyBytes(bytes, inputOffset, buffer, 0, remaining)
        bufferedBytes = remaining
      }
    }

    private def copyBytes(
        from: Array[Byte],
        fromOffset: Int,
        to: Array[Byte],
        toOffset: Int,
        length: Int
    ): Unit = {
      var i = 0
      while (i < length) {
        to(toOffset + i) = from(fromOffset + i)
        i += 1
      }
    }

    private def compress(input: Array[Byte], offset: Int): Unit = {
      var i = 0
      while (i < 16) {
        val byteOffset = offset + (i * 4)
        schedule(i) =
          ((input(byteOffset) & 0xff) << 24) |
            ((input(byteOffset + 1) & 0xff) << 16) |
            ((input(byteOffset + 2) & 0xff) << 8) |
            (input(byteOffset + 3) & 0xff)
        i += 1
      }

      while (i < 64) {
        val x = schedule(i - 15)
        val y = schedule(i - 2)
        val small0 = rotateRight(x, 7) ^ rotateRight(x, 18) ^ (x >>> 3)
        val small1 = rotateRight(y, 17) ^ rotateRight(y, 19) ^ (y >>> 10)
        schedule(i) = schedule(i - 16) + small0 + schedule(i - 7) + small1
        i += 1
      }

      var a = state(0)
      var b = state(1)
      var c = state(2)
      var d = state(3)
      var e = state(4)
      var f = state(5)
      var g = state(6)
      var h = state(7)
      i = 0
      while (i < 64) {
        val large1 = rotateRight(e, 6) ^ rotateRight(e, 11) ^ rotateRight(e, 25)
        val choose = (e & f) ^ (~e & g)
        val temp1 = h + large1 + choose + roundConstants(i) + schedule(i)
        val large0 = rotateRight(a, 2) ^ rotateRight(a, 13) ^ rotateRight(a, 22)
        val majority = (a & b) ^ (a & c) ^ (b & c)
        val temp2 = large0 + majority

        h = g
        g = f
        f = e
        e = d + temp1
        d = c
        c = b
        b = a
        a = temp1 + temp2
        i += 1
      }

      state(0) += a
      state(1) += b
      state(2) += c
      state(3) += d
      state(4) += e
      state(5) += f
      state(6) += g
      state(7) += h
    }

    private[hashing] def finishHex(): String = {
      val copy = new Hasher(state.clone(), buffer.clone(), bufferedBytes, byteCount)
      copy.padAndRender()
    }

    private def padAndRender(): String = {
      val bitCount = byteCount << 3
      buffer(bufferedBytes) = 0x80.toByte
      bufferedBytes += 1

      if (bufferedBytes > 56) {
        while (bufferedBytes < 64) {
          buffer(bufferedBytes) = 0.toByte
          bufferedBytes += 1
        }
        compress(buffer, 0)
        bufferedBytes = 0
      }

      while (bufferedBytes < 56) {
        buffer(bufferedBytes) = 0.toByte
        bufferedBytes += 1
      }
      var i = 0
      while (i < 8) {
        buffer(63 - i) = (bitCount >>> (i * 8)).toByte
        i += 1
      }
      compress(buffer, 0)

      val result = new Array[Char](64)
      i = 0
      while (i < state.length) {
        var byteInWord = 0
        while (byteInWord < 4) {
          val byte = (state(i) >>> (24 - (byteInWord * 8))) & 0xff
          result((i * 4 + byteInWord) * 2) = hexDigits(byte >>> 4)
          result((i * 4 + byteInWord) * 2 + 1) = hexDigits(byte & 0x0f)
          byteInWord += 1
        }
        i += 1
      }
      new String(result)
    }
  }

  def sha256HashHex(bytes: Array[Byte]): String =
    Algo.sha256Algo.hashBytes(bytes).hex
}
