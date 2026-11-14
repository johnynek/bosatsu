package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import dev.bosatsu.hashing.Algo
import org.scalacheck.Prop.forAll
import java.nio.charset.StandardCharsets

class BytesTest extends munit.ScalaCheckSuite {
  property("array constructors and copies cannot mutate the stored bytes") {
    forAll { (values: List[Byte]) =>
      val source = values.toArray
      val bytes = Bytes.fromArray(source)
      val copy = bytes.copyToArray
      if (source.nonEmpty) {
        source(0) = (source(0) ^ 1).toByte
        copy(0) = (copy(0) ^ 1).toByte
      }
      assertEquals(bytes.toList, values)
      assert(bytes.sameBytes(Bytes.fromArray(values.toArray)))
      assertEquals(bytes.hash[Algo.Sha256], Algo.hashBytes[Algo.Sha256](values.toArray))
      assertEquals(bytes.hash[Algo.Blake3], Algo.hashBytes[Algo.Blake3](values.toArray))
    }
  }

  test("trusted array ownership transfer keeps the same read-only backing storage") {
    val array = Array[Byte](1, 2, 3)
    val bytes = Bytes.unsafeFromArray(array)
    assert(bytes.unsafeReadOnlyView eq array)
    assert(!bytes.sameBytes(Bytes.empty))
    assert(Bytes.fromArray(Array[Byte](-1)).toMessage(proto.Library).isFailure)
  }

  property("UTF-8 strings and protobuf messages round trip") {
    forAll { (raw: String) =>
      val text = new String(raw.getBytes(StandardCharsets.UTF_8), StandardCharsets.UTF_8)
      val encoded = Bytes.fromUTF8String(text)
      assertEquals(encoded.toList, text.getBytes(StandardCharsets.UTF_8).toList)
      val message = proto.Library(name = text)
      val bytes = Bytes.fromMessage(message)
      assertEquals(bytes.toMessage(proto.Library).get, message)
      assertEquals(bytes.toList, message.toByteArray.toList)
    }
  }
}
