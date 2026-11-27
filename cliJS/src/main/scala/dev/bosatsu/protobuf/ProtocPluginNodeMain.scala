package dev.bosatsu.protobuf

import dev.bosatsu.JsBytes
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.typedarray.Uint8Array

/** Node.js protoc plugin entrypoint for bosatsu-node.
  */
object ProtocPluginNodeMain {

  def runFromStdio(): Unit = {
    val fs = global.require("fs")
    val requestBytes =
      JsBytes.fromUint8Array(fs.readFileSync(0).asInstanceOf[Uint8Array])
    val responseBytes = CodeGenerator.generateResponseBytes(requestBytes)
    val _ = fs.writeFileSync(1, JsBytes.toUint8Array(responseBytes))
    ()
  }
}
