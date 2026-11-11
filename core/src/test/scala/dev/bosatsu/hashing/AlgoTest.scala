package dev.bosatsu.hashing

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class AlgoTest extends munit.ScalaCheckSuite {
  private def ascii(value: String): Array[Byte] =
    value.toCharArray.map(_.toByte)

  private def hashChunks[A](
      algo: Algo[A],
      bytes: Array[Byte],
      chunkSizes: List[Int]
  ): HashValue[A] = {
    var hasher = algo.newHasher()
    val emptyPrefix = Array[Byte](1, 2)
    hasher = algo.hashBytes(hasher, emptyPrefix, emptyPrefix.length, 0)

    var offset = 0
    chunkSizes.foreach { size =>
      val chunkLength = math.min(size, bytes.length - offset)
      val padded =
        Array[Byte](11, 12) ++
          bytes.slice(offset, offset + chunkLength) ++
          Array[Byte](13)
      hasher = algo.hashBytes(hasher, padded, 2, chunkLength)
      offset += chunkLength
    }
    if (offset < bytes.length) {
      val padded = Array[Byte](11, 12) ++ bytes.drop(offset) ++ Array[Byte](13)
      hasher = algo.hashBytes(hasher, padded, 2, bytes.length - offset)
    }

    val emptySuffix = Array[Byte](14, 15)
    hasher = algo.hashBytes(hasher, emptySuffix, emptySuffix.length, 0)
    algo.finishHash(hasher)
  }

  test("SHA-256 known vectors cover empty, short, multi-block, and padding boundaries") {
    val algo = Algo.sha256Algo
    assertEquals(
      algo.hashBytes(Array.emptyByteArray).hex,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(
      algo.hashBytes(ascii("abc")).hex,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    assertEquals(
      algo.hashBytes(
        ascii("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
      ).hex,
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    )

    val boundaryVectors = List(
      55 -> "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318",
      56 -> "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a",
      63 -> "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34",
      64 -> "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb",
      65 -> "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0"
    )
    boundaryVectors.foreach { case (length, expected) =>
      assertEquals(algo.hashBytes(Array.fill[Byte](length)('a'.toByte)).hex, expected)
    }
  }

  test("SHA-256 compatibility helper delegates to Algo") {
    val bytes = ascii("bosatsu")
    assertEquals(
      Sha256Hash.sha256HashHex(bytes),
      Algo.sha256Algo.hashBytes(bytes).hex
    )
  }

  test("BLAKE3 known vectors cover empty, short, and multi-block inputs") {
    val algo = Algo.blake3Algo
    assertEquals(
      algo.hashBytes(Array.emptyByteArray).hex,
      "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"
    )
    assertEquals(
      algo.hashBytes(Array[Byte](0)).hex,
      "2d3adedff11b61f14c886e35afa036736dcd87a74d27b5c1510225d0f592e213"
    )
    assertEquals(
      algo.hashBytes(Array[Byte](0, 1, 2)).hex,
      "e1be4d7a8ab5560aa4199eea339849ba8e293d55ca0a81006726d184519e647f"
    )
    val repeatedSequence = Array.tabulate[Byte](1024)(i => (i % 251).toByte)
    assertEquals(
      algo.hashBytes(repeatedSequence).hex,
      "42214739f095a406f3fc83deb889744ac00df831c10daa55189b5d121c855af7"
    )
  }

  property("incremental hashing is independent of chunks and backing-array offsets") {
    val bytes = Gen.listOf(Arbitrary.arbitrary[Byte])
    val chunkSizes = Gen.listOf(Gen.choose(0, 80))

    forAll(bytes, chunkSizes) { (byteList, sizes) =>
      val input = byteList.toArray
      val sha256 = Algo.sha256Algo
      val blake3 = Algo.blake3Algo
      hashChunks(sha256, input, sizes).hex == sha256.hashBytes(input).hex &&
      hashChunks(blake3, input, sizes).hex == blake3.hashBytes(input).hex
    }
  }

  property("both supported hash identifiers round trip through the full parser") {
    val bytes = Gen.listOf(Arbitrary.arbitrary[Byte])
    forAll(bytes) { byteList =>
      val input = byteList.toArray
      val hashes = List(
        Algo.sha256Algo.hashBytes(input).toIdent(using Algo.sha256Algo),
        Algo.blake3Algo.hashBytes(input).toIdent(using Algo.blake3Algo)
      )
      hashes.forall { ident =>
        Algo.parseIdent.parseAll(ident).exists { parsed =>
          parsed.value.toIdent(using parsed.algo) == ident
        }
      }
    }
  }

  test("hash identifier parser rejects unknown algorithms and malformed input") {
    val hex = "0" * 64
    val invalid = List(
      s"sha-256:$hex",
      s"SHA256:$hex",
      s"sha256:A${hex.drop(1)}",
      s"sha256:${hex.drop(1)}",
      s"sha256:${hex.dropRight(1)}g",
      s"blake3:$hex trailing"
    )
    invalid.foreach(ident => assert(Algo.parseIdent.parseAll(ident).isLeft, ident))
    assert(Algo.parseIdent.parseAll(s"blake3:$hex").isRight)
    assert(Algo.parseIdent.parseAll(s"sha256:$hex").isRight)
  }
}
