package dev.bosatsu.hashing

import cats.Hash
import dev.bosatsu.Bytes
import org.scalacheck.Prop.forAll

class HashedTest extends munit.ScalaCheckSuite {
  property(
    "Hash[Hashed] compares digests and argument contents, not references"
  ) {
    forAll { (left: List[Byte], right: List[Byte]) =>
      val leftBytes = Bytes.copyFromArray(left.toArray)
      val digest = Algo.hash[Algo.Blake3](leftBytes)
      val hashed = Hashed(digest, leftBytes)
      val copy = Hashed(digest, Bytes.copyFromArray(left.toArray))
      val other = Hashed(digest, Bytes.copyFromArray(right.toArray))
      val otherDigest = Hashed(
        Algo.hash[Algo.Blake3](Bytes.copyFromArray((0.toByte :: left).toArray)),
        leftBytes
      )
      val hash = Hash[Hashed[Algo.Blake3, Bytes]]
      assert(hash.eqv(hashed, copy))
      assertEquals(hash.hash(hashed), hash.hash(copy))
      assertEquals(hash.hash(hashed), hash.hash(other))
      assertEquals(hash.eqv(hashed, other), left == right)
      assert(!hash.eqv(hashed, otherDigest))
    }
  }
}
