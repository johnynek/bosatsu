package dev.bosatsu

import org.scalacheck.Prop.forAll
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

class JsBytesTest extends munit.ScalaCheckSuite {
  private val nodeBuffer = js.Dynamic.global.Buffer

  private def buffer(values: List[Byte]): Uint8Array =
    nodeBuffer.from(js.Array(values.map(_ & 0xff)*)).asInstanceOf[Uint8Array]

  private def unsigned(view: Uint8Array): List[Int] =
    List.tabulate(view.length)(view(_).toInt)

  private def contents(bytes: Bytes): List[Byte] =
    List.tabulate(bytes.length)(bytes(_))

  property("fromUint8Array copies exactly a Node Buffer's window") {
    forAll { (prefix: List[Byte], values: List[Byte], suffix: List[Byte]) =>
      val whole = buffer(prefix ::: values ::: suffix)
      val window = whole.subarray(prefix.length, prefix.length + values.length)
      val bytes = JsBytes.fromUint8Array(window)
      assertEquals(contents(bytes), values)
      if (values.nonEmpty) {
        window(0) = ((window(0) + 1) & 0xff).toShort
        assertEquals(contents(bytes), values)
      }
    }
  }

  property("toUint8Array copies unsigned bytes into storage the caller owns") {
    forAll { (values: List[Byte]) =>
      val bytes = Bytes.copyFromArray(values.toArray)
      val view = JsBytes.toUint8Array(bytes)
      assertEquals(unsigned(view), values.map(_ & 0xff))
      if (values.nonEmpty) {
        view(0) = ((view(0) + 1) & 0xff).toShort
        assertEquals(contents(bytes), values)
      }
    }
  }

  property("toUint8Array with a range copies only that slice") {
    forAll { (prefix: List[Byte], values: List[Byte], suffix: List[Byte]) =>
      val bytes = Bytes.copyFromArray((prefix ::: values ::: suffix).toArray)
      val view = JsBytes.toUint8Array(bytes, prefix.length, values.length)
      assertEquals(view.length, values.length)
      assertEquals(unsigned(view), values.map(_ & 0xff))
    }
  }
}
