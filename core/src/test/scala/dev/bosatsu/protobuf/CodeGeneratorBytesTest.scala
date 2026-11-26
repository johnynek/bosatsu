package dev.bosatsu.protobuf

import com.google.protobuf.compiler.plugin.CodeGeneratorResponse
import dev.bosatsu.Bytes

class CodeGeneratorBytesTest extends munit.FunSuite {
  private def respond(request: Bytes): CodeGeneratorResponse =
    CodeGeneratorResponse.parseFrom(
      CodeGenerator.generateResponseBytes(request).unsafeReadOnlyView
    )

  test("generateResponseBytes answers an empty request with an empty response") {
    val response = respond(Bytes.empty)
    assertEquals(response.error, None)
    assertEquals(response.file, Nil)
  }

  test("generateResponseBytes reports a malformed request in the response") {
    val response = respond(Bytes.copyFromArray(Array(0x78.toByte)))
    assert(response.error.exists(_.nonEmpty), response)
  }
}
