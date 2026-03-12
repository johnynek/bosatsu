package dev.bosatsu.protobuf

import com.google.protobuf.compiler.plugin.{
  CodeGeneratorRequest,
  CodeGeneratorResponse
}
import scala.util.control.NonFatal

object ProtocPluginMain {

  def main(args: Array[String]): Unit = {
    runFromStdio()
  }

  def runFromStdio(): Unit = {
    val response =
      try {
        val request = CodeGeneratorRequest.parseFrom(System.in)
        generateResponse(request)
      } catch {
        case NonFatal(err) =>
          val msg = Option(err.getMessage).getOrElse(err.toString)
          CodeGeneratorResponse.defaultInstance.withError(msg)
      }

    response.writeTo(System.out)
    System.out.flush()
  }

  private[protobuf] def generateResponse(
      request: CodeGeneratorRequest
  ): CodeGeneratorResponse =
    CodeGenerator.generateResponse(request)
}
