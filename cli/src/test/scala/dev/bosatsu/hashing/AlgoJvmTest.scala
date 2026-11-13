package dev.bosatsu.hashing

import java.security.MessageDigest
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class AlgoJvmTest extends munit.ScalaCheckSuite {
  private val bytesGen = Gen.choose(0, 4096).flatMap { size =>
    Gen.listOfN(size, Gen.choose(-128, 127).map(_.toByte)).map(_.toArray)
  }

  private def javaHex(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    digest.iterator
      .map { byte =>
        val hex = Integer.toHexString(byte & 0xff)
        if (hex.length == 1) "0" + hex else hex
      }
      .mkString
  }

  property("portable SHA-256 matches java.security across generated inputs") {
    forAll(bytesGen) { bytes =>
      assertEquals(Algo.sha256Algo.hashBytes(bytes).hex, javaHex(bytes))
    }
  }

  test("portable SHA-256 matches java.security around padding and block boundaries") {
    val lengths = List(0, 1, 55, 56, 63, 64, 65, 119, 120, 127, 128, 129, 255, 256)

    lengths.foreach { size =>
      val bytes = Array.tabulate(size)(idx => (idx * 31 + size).toByte)
      assertEquals(Algo.sha256Algo.hashBytes(bytes).hex, javaHex(bytes))
    }
  }
}
