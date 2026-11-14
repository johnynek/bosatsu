package dev.bosatsu.hashing

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class AlgoTest extends munit.ScalaCheckSuite {
  private val sha256 = Algo.sha256Algo
  private val byteGen = Gen.choose(-128, 127).map(_.toByte)
  private val bytesGen = Gen.choose(0, 2048).flatMap { size =>
    Gen.listOfN(size, byteGen).map(_.toArray)
  }
  private val chunkSizesGen = Gen.listOf(Gen.choose(0, 128))

  private def bytes(value: String): Array[Byte] =
    value.iterator.map(_.toByte).toArray

  private def incrementallyHash(
      value: Array[Byte],
      chunkSizes: List[Int]
  ): HashValue[Algo.Sha256] = {
    val padded = Array.fill[Byte](value.length + 2)(0x55.toByte)
    Array.copy(value, 0, padded, 1, value.length)

    var hasher = sha256.newHasher()
    hasher = sha256.hashBytes(hasher, padded, 0, 0)

    var consumed = 0
    chunkSizes.foreach { chunkSize =>
      val size = math.min(chunkSize, value.length - consumed)
      hasher = sha256.hashBytes(hasher, padded, consumed + 1, size)
      consumed += size
    }
    if (consumed < value.length)
      hasher = sha256.hashBytes(
        hasher,
        padded,
        consumed + 1,
        value.length - consumed
      )

    hasher = sha256.hashBytes(hasher, padded, value.length + 1, 0)
    sha256.finishHash(hasher)
  }

  test("SHA-256 matches standard known vectors") {
    assertEquals(
      sha256.hashBytes(Array.emptyByteArray).hex,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(
      sha256.hashBytes(bytes("abc")).hex,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    assertEquals(
      sha256.hashBytes(
        bytes(
          "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
        )
      ).hex,
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    )
    assertEquals(
      sha256.hashBytes(Array.fill[Byte](1000000)('a'.toByte)).hex,
      "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
    )
  }

  test("BLAKE3 existing vectors remain unchanged") {
    assertEquals(
      Algo.blake3Algo.hashBytes(Array.emptyByteArray).hex,
      "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"
    )
    assertEquals(
      Algo.blake3Algo.hashBytes(bytes("abc")).hex,
      "6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85"
    )
  }

  property("SHA-256 one-shot and incremental hashing agree") {
    forAll(bytesGen, chunkSizesGen) { (value, chunkSizes) =>
      assertEquals(incrementallyHash(value, chunkSizes), sha256.hashBytes(value))
    }
  }

  property("SHA-256 and BLAKE3 identifiers round-trip through the shared parser") {
    forAll(bytesGen) { value =>
      val shaIdent = sha256.hashBytes(value).toIdent(using sha256)
      val blake3Ident = Algo.blake3Algo.hashBytes(value).toIdent(using Algo.blake3Algo)

      assertEquals(
        Algo.parseIdent.parseAll(shaIdent).map(_.toIdent),
        Right(shaIdent)
      )
      assertEquals(
        Algo.parseIdent.parseAll(blake3Ident).map(_.toIdent),
        Right(blake3Ident)
      )
    }
  }

  test("the shared identifier parser rejects unsupported and malformed digests") {
    val validSha = sha256.hashBytes(Array.emptyByteArray).toIdent(using sha256)
    val validBlake3 = Algo.blake3Algo
      .hashBytes(Array.emptyByteArray)
      .toIdent(using Algo.blake3Algo)

    assert(
      Algo.parseIdent
        .parseAll("sha-256:" + validSha.drop("sha256:".length))
        .isLeft
    )
    assert(Algo.parseIdent.parseAll("sha256:" + ("0" * 63)).isLeft)
    assert(Algo.parseIdent.parseAll("sha256:" + ("g" * 64)).isLeft)
    assert(Algo.parseIdent.parseAll(validSha + "x").isLeft)
    assert(Algo.parseIdent.parseAll("blake3:" + ("0" * 63)).isLeft)
    assert(Algo.parseIdent.parseAll("blake3:" + ("g" * 64)).isLeft)
    assert(Algo.parseIdent.parseAll(validBlake3 + "x").isLeft)
  }

  test("bare SHA-256 fields reuse the shared fixed-width hex parser") {
    val hash = sha256.hashBytes(bytes("abc"))
    assertEquals(Algo.parseHashHex(sha256).parseAll(hash.hex), Right(hash))
    assert(Algo.parseHashHex(sha256).parseAll(hash.hex + "0").isLeft)
    assert(Algo.parseHashHex(sha256).parseAll("g" * 64).isLeft)
  }

  test("new SHA-256 hashers keep independent state") {
    var first = sha256.newHasher()
    var second = sha256.newHasher()
    val firstBytes = bytes("first")
    val secondBytes = bytes("second")

    first = sha256.hashBytes(first, firstBytes, 0, 2)
    second = sha256.hashBytes(second, secondBytes, 0, secondBytes.length)
    first = sha256.hashBytes(first, firstBytes, 2, firstBytes.length - 2)

    assertEquals(sha256.finishHash(first), sha256.hashBytes(firstBytes))
    assertEquals(sha256.finishHash(second), sha256.hashBytes(secondBytes))
  }
}
