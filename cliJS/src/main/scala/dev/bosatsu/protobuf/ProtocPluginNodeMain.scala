package dev.bosatsu.protobuf

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.typedarray.Uint8Array

/** Node.js protoc plugin entrypoint for bosatsu-node.
  */
object ProtocPluginNodeMain {

  def runFromStdio(): Unit = {
    val fs = global.require("fs")
    val requestBytes = nodeBufferToBytes(fs.readFileSync(0))
    val responseBytes = CodeGenerator.generateResponseBytes(requestBytes)
    val _ = fs.writeFileSync(1, bytesToUint8Array(responseBytes))
    ()
  }

  private def nodeBufferToBytes(buffer: js.Dynamic): Array[Byte] = {
    val length = buffer.length.asInstanceOf[Int]
    val out = new Array[Byte](length)
    var idx = 0
    while (idx < length) {
      out(idx) = (buffer(idx).asInstanceOf[Int] & 0xff).toByte
      idx += 1
    }
    out
  }

  private def bytesToUint8Array(bytes: Array[Byte]): Uint8Array = {
    val out = new Uint8Array(bytes.length)
    var idx = 0
    while (idx < bytes.length) {
      out(idx) = (bytes(idx) & 0xff).toShort
      idx += 1
    }
    out
  }
}
