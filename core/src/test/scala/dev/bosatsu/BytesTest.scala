package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.{Eq, Hash, Order, Show}
import cats.instances.int._
import cats.instances.list._
import java.nio.charset.StandardCharsets
import org.scalacheck.Prop.forAll

class BytesTest extends munit.ScalaCheckSuite {
  property(
    "Hash, Order and Eq compare contents across independent allocations"
  ) {
    forAll { (left: List[Byte], right: List[Byte]) =>
      val a = Bytes.copyFromArray(left.toArray)
      val b = Bytes.copyFromArray(right.toArray)
      val copy = Bytes.copyFromArray(left.toArray)
      val hash = Hash[Bytes]
      val order = Order[Bytes]
      assert(hash.eqv(a, copy))
      assertEquals(hash.hash(a), hash.hash(copy))
      assertEquals(hash.hash(a), a.contentHashCode)
      assertEquals(hash.eqv(a, b), left == right)
      assertEquals(Eq[Bytes].eqv(a, b), left == right)
      assertEquals(order.eqv(a, b), hash.eqv(a, b))
      val expected = Order[List[Int]].compare(
        left.map(_ & 0xff),
        right.map(_ & 0xff)
      )
      assertEquals(math.signum(order.compare(a, b)), math.signum(expected))
      assertEquals(math.signum(order.compare(b, a)), -math.signum(expected))
      assert(order.lteqv(a, Bytes.copyFromArray((left ::: right).toArray)))
    }
  }

  property("byte ordering is transitive") {
    forAll { (first: List[Byte], second: List[Byte], third: List[Byte]) =>
      val a = Bytes.copyFromArray(first.toArray)
      val b = Bytes.copyFromArray(second.toArray)
      val c = Bytes.copyFromArray(third.toArray)
      val order = Order[Bytes]
      if (order.lteqv(a, b) && order.lteqv(b, c)) assert(order.lteqv(a, c))
    }
  }

  property("Show renders every byte as two lowercase hexadecimal digits") {
    forAll { (values: List[Byte]) =>
      val bytes = Bytes.copyFromArray(values.toArray)
      val expected = values.map(byte => f"${byte & 0xff}%02x").mkString
      assertEquals(Show[Bytes].show(bytes), expected)
    }
  }

  property("array copies isolate Bytes from mutations in both directions") {
    forAll { (values: List[Byte]) =>
      val input = values.toArray
      val bytes = Bytes.copyFromArray(input)
      val copy = bytes.copyToArray
      input.indices.foreach(index =>
        input(index) = (input(index) ^ 0xff).toByte
      )
      copy.indices.foreach(index => copy(index) = (copy(index) ^ 0xff).toByte)
      assertEquals(bytes.unsafeReadOnlyView.toList, values)
      assert(bytes.unsafeReadOnlyView eq bytes.unsafeReadOnlyView)
    }
  }

  property("length and apply read contents without copying") {
    forAll { (values: List[Byte]) =>
      val bytes = Bytes.copyFromArray(values.toArray)
      assertEquals(bytes.length, values.length)
      assertEquals(values.indices.map(bytes(_)).toList, values)
    }
  }

  property("decodeUtf8 decodes whole values and slices of valid UTF-8") {
    forAll { (prefix: String, text: String) =>
      val prefixBytes = prefix.getBytes(StandardCharsets.UTF_8)
      val textBytes = text.getBytes(StandardCharsets.UTF_8)
      val all = prefixBytes ++ textBytes
      val bytes = Bytes.copyFromArray(all)
      assertEquals(
        bytes.decodeUtf8.toOption,
        Some(new String(all, StandardCharsets.UTF_8))
      )
      assertEquals(
        bytes.decodeUtf8Slice(prefixBytes.length, textBytes.length).toOption,
        Some(new String(textBytes, StandardCharsets.UTF_8))
      )
    }
  }

  test("decodeUtf8 reports malformed input instead of replacing it") {
    val malformed =
      Bytes.copyFromArray(Array(0x61.toByte, 0xc3.toByte, 0x28.toByte))
    assert(malformed.decodeUtf8.isLeft)
    assertEquals(malformed.decodeUtf8Slice(0, 1).toOption, Some("a"))
    assert(Bytes.copyFromArray(Array(0x80.toByte)).decodeUtf8.isLeft)
  }

  property("UTF-8 and protobuf constructors preserve serialization") {
    forAll { (text: String) =>
      val bytes = Bytes.fromUTF8String(text)
      assertEquals(
        bytes.unsafeReadOnlyView.toList,
        text.getBytes(StandardCharsets.UTF_8).toList
      )
      val message = proto.Library(name = text)
      assertEquals(
        proto.Library.parseFrom(Bytes.fromMessage(message).unsafeReadOnlyView),
        message
      )
    }
  }
}
