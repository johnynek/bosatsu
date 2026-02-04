package dev.bosatsu.service

import dev.bosatsu.Par

class ServiceTest extends munit.FunSuite {

  // Create implicit EC for tests
  given ec: Par.EC = Par.ecFromExecutionContext(
    using scala.concurrent.ExecutionContext.global
  )

  test("ServiceBuilder.compileHandlers - simple function") {
    val source = """
package TestService

def hello(name: String) -> String:
  name
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isRight, s"Expected Right but got: $result")

    val handlers = result.toOption.get
    assertEquals(handlers.size, 1)
    assertEquals(handlers.head.name, "hello")
    assertEquals(handlers.head.params.map(_.name.asString), List("name"))
  }

  test("ServiceBuilder.compileHandlers - function with different params") {
    val source = """
package TestService

def identity(x: Int) -> Int:
  x
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isRight, s"Expected Right but got: $result")

    val handlers = result.toOption.get
    assertEquals(handlers.size, 1)
    val handler = handlers.head
    assertEquals(handler.name, "identity")
    assertEquals(handler.params.size, 1)
    assertEquals(handler.params.head.name.asString, "x")
  }

  test("ServiceBuilder.compileHandlers - parse error") {
    val source = """
package TestService

def broken(
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isLeft, "Expected Left for parse error")
  }

  test("ServiceBuilder.compileHandlers - type error") {
    val source = """
package TestService

def broken(x: String) -> Int:
  x  # String is not Int
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isLeft, "Expected Left for type error")
  }

  test("ServiceBuilder.compileHandlers - no functions") {
    val source = """
package TestService

x = 42
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isLeft, "Expected Left when no functions found")
    assert(result.left.toOption.get.contains("No handlers"),
      s"Expected 'No handlers' error but got: ${result.left.toOption.get}")
  }

  test("ServiceBuilder.build - generates JavaScript for standalone") {
    val source = """
package TestService

def greet(name: String) -> String:
  name
"""
    val result = ServiceBuilder.build(source, "test.bosatsu", BuildTarget.Standalone)
    assert(result.isRight, s"Expected Right but got: $result")

    val buildResult = result.toOption.get
    assert(buildResult.handlers.nonEmpty)
    assertEquals(buildResult.handlers, List("greet"))
    assert(buildResult.jsCode.nonEmpty)
    assertEquals(buildResult.target, BuildTarget.Standalone)
  }

  test("ServiceBuilder.build - generates JavaScript for Vercel") {
    val source = """
package TestService

def process(x: Int) -> Int:
  x.add(1)
"""
    val result = ServiceBuilder.build(source, "test.bosatsu", BuildTarget.Vercel)
    assert(result.isRight, s"Expected Right but got: $result")

    val buildResult = result.toOption.get
    assertEquals(buildResult.target, BuildTarget.Vercel)
    assert(buildResult.jsCode.contains("Vercel serverless handler"))
  }

  test("ServiceBuilder.build - generates JavaScript for AWS Lambda") {
    val source = """
package TestService

def compute(n: Int) -> Int:
  n.times(2)
"""
    val result = ServiceBuilder.build(source, "test.bosatsu", BuildTarget.AwsLambda)
    assert(result.isRight, s"Expected Right but got: $result")

    val buildResult = result.toOption.get
    assertEquals(buildResult.target, BuildTarget.AwsLambda)
    assert(buildResult.jsCode.contains("AWS Lambda handler"))
  }

  test("ServiceJson.renderAnalyses - produces valid JSON") {
    val analysis = ServiceAnalysis(
      handlerName = "test_handler",
      sourceFile = "test.bosatsu",
      operations = List(
        ServiceOperation(
          interface = "Database",
          method = "query",
          kind = OperationKind.Read,
          batchable = true,
          batchMethod = Some("batchQuery")
        )
      ),
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 1,
      batchedQueries = 0,
      queriesSaved = 0
    )

    val json = ServiceJson.renderAnalyses(List(analysis))
    assert(json.contains("test_handler"))
    assert(json.contains("Database"))
    assert(json.contains("query"))
    assert(json.contains("read"))
  }

  test("ServiceAnalysis.batchingEfficiency - calculates correctly") {
    val analysis = ServiceAnalysis(
      handlerName = "handler",
      sourceFile = "test.bosatsu",
      operations = Nil,
      batchGroups = Nil,
      canBatch = true,
      totalQueries = 10,
      batchedQueries = 5,
      queriesSaved = 5
    )

    assertEquals(analysis.batchingEfficiency, "50%")
  }

  test("ServiceAnalysis.batchingEfficiency - handles zero queries") {
    val analysis = ServiceAnalysis(
      handlerName = "handler",
      sourceFile = "test.bosatsu",
      operations = Nil,
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 0,
      batchedQueries = 0,
      queriesSaved = 0
    )

    assertEquals(analysis.batchingEfficiency, "0%")
  }

  test("CompiledHandler - jsCode contains function") {
    val source = """
package TestService

def double_it(x: Int) -> Int:
  x
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isRight, s"Expected Right but got: $result")

    val handlers = result.toOption.get
    val handler = handlers.find(_.name == "double_it")
    assert(handler.isDefined)
    assert(handler.get.jsCode.contains("double_it"), s"Expected 'double_it' in JS code but got: ${handler.get.jsCode}")
  }

  test("ServiceOperation - equality") {
    val op1 = ServiceOperation("DB", "get", OperationKind.Read, true, Some("getMany"))
    val op2 = ServiceOperation("DB", "get", OperationKind.Read, true, Some("getMany"))
    val op3 = ServiceOperation("DB", "set", OperationKind.Write, false, None)

    assertEquals(op1, op2)
    assertNotEquals(op1, op3)
  }

  test("BuildTarget - all variants") {
    val standalone: BuildTarget = BuildTarget.Standalone
    val vercel: BuildTarget = BuildTarget.Vercel
    val lambda: BuildTarget = BuildTarget.AwsLambda

    assertNotEquals(standalone, vercel)
    assertNotEquals(vercel, lambda)
    assertNotEquals(standalone, lambda)
  }

  test("OperationKind - all variants") {
    val read: OperationKind = OperationKind.Read
    val write: OperationKind = OperationKind.Write
    val unknown: OperationKind = OperationKind.Unknown

    assertNotEquals(read, write)
    assertNotEquals(write, unknown)
    assertNotEquals(read, unknown)
  }
}
