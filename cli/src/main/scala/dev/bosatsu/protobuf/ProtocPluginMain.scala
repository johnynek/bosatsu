package dev.bosatsu.protobuf

import com.google.protobuf.compiler.plugin.{
  CodeGeneratorRequest,
  CodeGeneratorResponse
}
import dev.bosatsu.Bytes

object ProtocPluginMain {

  def main(args: Array[String]): Unit =
    runFromStdio()

  def runFromStdio(): Unit = {
    val requestBytes = Bytes.unsafeFromArray(System.in.readAllBytes())
    val responseBytes = CodeGenerator.generateResponseBytes(requestBytes)
    System.out.write(responseBytes.unsafeReadOnlyView)
    System.out.flush()
  }

  private[protobuf] def generateResponse(
      request: CodeGeneratorRequest
  ): CodeGeneratorResponse =
    CodeGenerator.generateResponse(request)
}
