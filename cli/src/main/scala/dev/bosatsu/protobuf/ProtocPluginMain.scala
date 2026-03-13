package dev.bosatsu.protobuf

import com.google.protobuf.compiler.plugin.{
  CodeGeneratorRequest,
  CodeGeneratorResponse
}

object ProtocPluginMain {

  def main(args: Array[String]): Unit = {
    runFromStdio()
  }

  def runFromStdio(): Unit = {
    val responseBytes = CodeGenerator.generateResponseBytes(System.in.readAllBytes())
    System.out.write(responseBytes)
    System.out.flush()
  }

  private[protobuf] def generateResponse(
      request: CodeGeneratorRequest
  ): CodeGeneratorResponse =
    CodeGenerator.generateResponse(request)
}
