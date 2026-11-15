package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import java.nio.charset.StandardCharsets
import org.scalacheck.Prop.forAll

class BytesTest extends munit.ScalaCheckSuite {
  property("array copies isolate Bytes from mutations in both directions") {
    forAll { (values: List[Byte]) =>
      val input = values.toArray
      val bytes = Bytes.copyFromArray(input)
      val copy = bytes.copyToArray
      input.indices.foreach(index => input(index) = (input(index) ^ 0xff).toByte)
      copy.indices.foreach(index => copy(index) = (copy(index) ^ 0xff).toByte)
      assertEquals(bytes.unsafeReadOnlyView.toList, values)
      assert(bytes.unsafeReadOnlyView eq bytes.unsafeReadOnlyView)
    }
  }

  property("UTF-8 and protobuf constructors preserve serialization") {
    forAll { (text: String) =>
      val bytes = Bytes.fromUTF8String(text)
      assertEquals(bytes.unsafeReadOnlyView.toList, text.getBytes(StandardCharsets.UTF_8).toList)
      val message = proto.Library(name = text)
      assertEquals(proto.Library.parseFrom(Bytes.fromMessage(message).unsafeReadOnlyView), message)
    }
  }
}
