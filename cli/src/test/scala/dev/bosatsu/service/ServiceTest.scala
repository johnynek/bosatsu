package dev.bosatsu.service

import dev.bosatsu.Par
import org.http4s.circe.CirceEntityCodec._

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

  test("BatchGroup - equality") {
    val bg1 = BatchGroup("DB", "get", "getMany", Nil, 3)
    val bg2 = BatchGroup("DB", "get", "getMany", Nil, 3)
    val bg3 = BatchGroup("DB", "set", "setMany", Nil, 2)

    assertEquals(bg1, bg2)
    assertNotEquals(bg1, bg3)
  }

  test("BatchConfig - default values") {
    val config = BatchConfig.default
    assert(config.batchableMethods.contains("get"))
    assert(config.readMethods.contains("get"))
    assert(config.writeMethods.contains("set"))
  }

  test("HandlerParam - equality") {
    import dev.bosatsu.Identifier
    val name1 = Identifier.Name("x")
    val param1 = HandlerParam(name1, false, Set.empty)
    val param2 = HandlerParam(name1, false, Set.empty)
    val param3 = HandlerParam(name1, true, Set("get"))

    assertEquals(param1, param2)
    assertNotEquals(param1, param3)
  }

  test("HandlerResult - equality") {
    val hr1 = HandlerResult("{\"x\": 1}", Some("{\"trace\": []}"))
    val hr2 = HandlerResult("{\"x\": 1}", Some("{\"trace\": []}"))
    val hr3 = HandlerResult("{\"x\": 2}", None)

    assertEquals(hr1, hr2)
    assertNotEquals(hr1, hr3)
  }

  test("ServiceResponse.Success") {
    val resp: ServiceResponse = ServiceResponse.Success("handler1", "{\"result\": 42}", None)
    resp match {
      case ServiceResponse.Success(h, r, p) =>
        assertEquals(h, "handler1")
        assertEquals(r, "{\"result\": 42}")
        assertEquals(p, None)
      case _ => fail("Expected Success")
    }
  }

  test("ServiceResponse.Error") {
    val resp: ServiceResponse = ServiceResponse.Error("handler1", "Something went wrong", 500)
    resp match {
      case ServiceResponse.Error(h, e, c) =>
        assertEquals(h, "handler1")
        assertEquals(e, "Something went wrong")
        assertEquals(c, 500)
      case _ => fail("Expected Error")
    }
  }

  test("ServiceJson.renderAnalysis - single analysis") {
    val analysis = ServiceAnalysis(
      handlerName = "single_handler",
      sourceFile = "single.bosatsu",
      operations = Nil,
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 0,
      batchedQueries = 0,
      queriesSaved = 0
    )

    val json = ServiceJson.renderAnalysis(analysis)
    assert(json.contains("single_handler"))
    assert(json.contains("single.bosatsu"))
  }

  test("ServiceJson.renderResponse - success") {
    val resp = ServiceResponse.Success("test", "{\"value\": 1}", None)
    val json = ServiceJson.renderResponse(resp)
    assert(json.contains("\"success\":true"))
    assert(json.contains("\"handler\":\"test\""))
  }

  test("ServiceJson.renderResponse - error") {
    val resp = ServiceResponse.Error("test", "failed", 400)
    val json = ServiceJson.renderResponse(resp)
    assert(json.contains("\"success\":false"))
    assert(json.contains("\"error\":\"failed\""))
    assert(json.contains("\"code\":400"))
  }

  test("ServiceJson.renderBuildResult") {
    val result = BuildResult(List("handler1", "handler2"), "const x = 1;", BuildTarget.Standalone)
    val json = ServiceJson.renderBuildResult(result)
    assert(json.contains("handler1"))
    assert(json.contains("handler2"))
    assert(json.contains("\"target\" : \"standalone\""))
  }

  test("ServiceJson - OperationKind roundtrip") {
    import io.circe.parser.decode
    import ServiceJson.given

    val encoded = io.circe.Encoder[OperationKind].apply(OperationKind.Read)
    assertEquals(encoded.asString, Some("read"))

    val decoded = decode[OperationKind]("\"write\"")
    assertEquals(decoded, Right(OperationKind.Write))

    val unknown = decode[OperationKind]("\"unknown\"")
    assertEquals(unknown, Right(OperationKind.Unknown))
  }

  test("ServiceJson - invalid OperationKind") {
    import io.circe.parser.decode
    import ServiceJson.given

    val invalid = decode[OperationKind]("\"invalid\"")
    assert(invalid.isLeft)
  }

  test("ServiceJson - ServiceOperation roundtrip") {
    import io.circe.parser.decode
    import ServiceJson.given

    val op = ServiceOperation("DB", "get", OperationKind.Read, true, Some("getMany"))
    val json = io.circe.Encoder[ServiceOperation].apply(op)
    val decoded = decode[ServiceOperation](json.noSpaces)

    assertEquals(decoded, Right(op))
  }

  test("BuildResult - equality") {
    val br1 = BuildResult(List("h1"), "code", BuildTarget.Vercel)
    val br2 = BuildResult(List("h1"), "code", BuildTarget.Vercel)
    val br3 = BuildResult(List("h2"), "code", BuildTarget.AwsLambda)

    assertEquals(br1, br2)
    assertNotEquals(br1, br3)
  }

  test("CompiledHandler - equality") {
    import dev.bosatsu.Identifier
    val analysis = ServiceAnalysis("h", "f.bosatsu", Nil, Nil, false, 0, 0, 0)
    val param = HandlerParam(Identifier.Name("x"), false, Set.empty)
    val ch1 = CompiledHandler("handler", List(param), "code", analysis)
    val ch2 = CompiledHandler("handler", List(param), "code", analysis)
    val ch3 = CompiledHandler("other", Nil, "code2", analysis)

    assertEquals(ch1, ch2)
    assertNotEquals(ch1, ch3)
  }

  // ==========================================================================
  // ServiceAnalyzer tests
  // ==========================================================================

  test("ServiceAnalyzer.analyzeHandler - empty expression produces analysis") {
    val source = """
package TestService

def simple(x: Int) -> Int:
  x
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isRight)
    val handlers = result.toOption.get
    assertEquals(handlers.size, 1)
    assertEquals(handlers.head.analysis.totalQueries, 0)
    assertEquals(handlers.head.analysis.canBatch, false)
  }

  test("ServiceAnalyzer.analyzeHandler - with let binding") {
    val source = """
package TestLet

def with_let(x: Int) -> Int:
  y = x.add(1)
  z = y.times(2)
  z
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isRight, s"Expected Right but got: $result")
    val handlers = result.toOption.get
    assertEquals(handlers.size, 1)
    // The analysis should still work even with let bindings
    assertEquals(handlers.head.analysis.handlerName, "with_let")
  }

  test("ServiceAnalyzer.analyzeHandler - with match expression") {
    val source = """
package TestMatch

def with_match(x: Int) -> Int:
  match x.cmp_Int(0):
    case GT: x
    case EQ: 0
    case LT: 0.sub(x)
"""
    val result = ServiceBuilder.compileHandlers(source, "test.bosatsu")
    assert(result.isRight, s"Expected Right but got: $result")
    val handlers = result.toOption.get
    assertEquals(handlers.size, 1)
    assertEquals(handlers.head.analysis.handlerName, "with_match")
  }

  test("ServiceAnalyzer - BatchGroup construction") {
    // Test that BatchGroup is correctly constructed
    val ops = List(
      ServiceOperation("DB", "get", OperationKind.Read, true, Some("getMany")),
      ServiceOperation("DB", "get", OperationKind.Read, true, Some("getMany")),
      ServiceOperation("DB", "get", OperationKind.Read, true, Some("getMany"))
    )
    val group = BatchGroup("DB", "get", "getMany", ops, 2)
    assertEquals(group.queriesSaved, 2)
    assertEquals(group.operations.size, 3)
  }

  test("OperationKind - classifyMethod via BatchConfig") {
    val config = BatchConfig.default
    // Verify read methods in default config
    assert(config.readMethods.contains("get"))
    assert(config.readMethods.contains("fetch"))
    assert(config.readMethods.contains("find"))
    // Verify write methods
    assert(config.writeMethods.contains("set"))
    assert(config.writeMethods.contains("put"))
    assert(config.writeMethods.contains("delete"))
  }

  test("BatchConfig - custom methods") {
    val config = BatchConfig(
      batchableMethods = Map("lookup" -> "lookupMany"),
      readMethods = Set("lookup", "search"),
      writeMethods = Set("store", "remove")
    )

    assert(config.batchableMethods.contains("lookup"))
    assert(config.readMethods.contains("search"))
    assert(config.writeMethods.contains("remove"))
  }

  // ==========================================================================
  // ServiceJson additional tests
  // ==========================================================================
  import ServiceJson.given

  test("ServiceJson - BuildTarget encode") {
    import ServiceJson.given
    val targets = List(BuildTarget.Standalone, BuildTarget.Vercel, BuildTarget.AwsLambda)
    val expected = List("standalone", "vercel", "aws-lambda")
    for ((target, exp) <- targets.zip(expected)) {
      val json = summon[io.circe.Encoder[BuildTarget]].apply(target)
      assertEquals(json.asString, Some(exp))
    }
  }

  test("ServiceJson - ServiceAnalysis encode") {
    import ServiceJson.given
    val analysis = ServiceAnalysis(
      handlerName = "complex_handler",
      sourceFile = "complex.bosatsu",
      operations = List(
        ServiceOperation("Cache", "get", OperationKind.Read, true, Some("getMany")),
        ServiceOperation("DB", "insert", OperationKind.Write, false, None)
      ),
      batchGroups = List(
        BatchGroup("Cache", "get", "getMany", Nil, 2)
      ),
      canBatch = true,
      totalQueries = 10,
      batchedQueries = 5,
      queriesSaved = 5
    )

    val json = summon[io.circe.Encoder[ServiceAnalysis]].apply(analysis)
    assertEquals(json.hcursor.downField("handlerName").as[String], Right("complex_handler"))
    assertEquals(json.hcursor.downField("totalQueries").as[Int], Right(10))
    assertEquals(json.hcursor.downField("canBatch").as[Boolean], Right(true))
  }

  test("ServiceJson - BatchGroup encode") {
    import ServiceJson.given
    val bg = BatchGroup("DB", "get", "getMany", Nil, 5)
    val json = summon[io.circe.Encoder[BatchGroup]].apply(bg)
    assertEquals(json.hcursor.downField("interface").as[String], Right("DB"))
    assertEquals(json.hcursor.downField("queriesSaved").as[Int], Right(5))
  }

  test("ServiceJson - HandlerResult encode") {
    import ServiceJson.given
    import io.circe.syntax._
    val result = HandlerResult("""{"value": 42}""", Some("""{"trace": []}"""))
    val json = summon[io.circe.Encoder[HandlerResult]].apply(result)
    assert(json.noSpaces.contains("42"), "HandlerResult should contain the value")
  }

  test("ServiceJson - ServiceResponse Success encode") {
    import ServiceJson.given
    val resp = ServiceResponse.Success("myHandler", """{"value": 42}""", Some("""{"trace": []}"""))
    val json = summon[io.circe.Encoder[ServiceResponse]].apply(resp)
    assertEquals(json.hcursor.downField("success").as[Boolean], Right(true))
    assertEquals(json.hcursor.downField("handler").as[String], Right("myHandler"))
  }

  test("ServiceJson - ServiceResponse Error encode") {
    import ServiceJson.given
    val resp = ServiceResponse.Error("handler", "Something went wrong", 400)
    val json = summon[io.circe.Encoder[ServiceResponse]].apply(resp)
    assertEquals(json.hcursor.downField("success").as[Boolean], Right(false))
    assertEquals(json.hcursor.downField("error").as[String], Right("Something went wrong"))
    assertEquals(json.hcursor.downField("code").as[Int], Right(400))
  }

  // ==========================================================================
  // McpServer integration tests
  // ==========================================================================

  import cats.effect.unsafe.implicits.global
  import io.circe.Json
  import io.circe.syntax._

  test("McpServer.jsonRpcResponse - with id") {
    val response = McpServer.jsonRpcResponse(Some(Json.fromInt(1)), Json.obj("success" -> true.asJson))
    assertEquals(response.hcursor.downField("jsonrpc").as[String], Right("2.0"))
    assertEquals(response.hcursor.downField("id").as[Int], Right(1))
    assertEquals(response.hcursor.downField("result").downField("success").as[Boolean], Right(true))
  }

  test("McpServer.jsonRpcResponse - with null id") {
    val response = McpServer.jsonRpcResponse(None, Json.obj())
    assertEquals(response.hcursor.downField("id").focus, Some(Json.Null))
  }

  test("McpServer.jsonRpcError - format") {
    val error = McpServer.jsonRpcError(Some(Json.fromInt(42)), -32601, "Method not found")
    assertEquals(error.hcursor.downField("jsonrpc").as[String], Right("2.0"))
    assertEquals(error.hcursor.downField("id").as[Int], Right(42))
    assertEquals(error.hcursor.downField("error").downField("code").as[Int], Right(-32601))
    assertEquals(error.hcursor.downField("error").downField("message").as[String], Right("Method not found"))
  }

  test("McpServer.processRequest - initialize") {
    val handlers = List.empty[CompiledHandler]
    val request = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 1.asJson,
      "method" -> "initialize".asJson,
      "params" -> Json.obj()
    )

    val response = McpServer.processRequest("TestServer", handlers, request).unsafeRunSync()
    assertEquals(response.hcursor.downField("result").downField("serverInfo").downField("name").as[String], Right("TestServer"))
    assertEquals(response.hcursor.downField("result").downField("protocolVersion").as[String], Right("2024-11-05"))
  }

  test("McpServer.processRequest - tools/list") {
    val analysis = ServiceAnalysis("myHandler", "test.bosatsu", Nil, Nil, false, 0, 0, 0)
    val handlers = List(CompiledHandler("myHandler", Nil, "code", analysis))
    val request = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 2.asJson,
      "method" -> "tools/list".asJson
    )

    val response = McpServer.processRequest("TestServer", handlers, request).unsafeRunSync()
    val tools = response.hcursor.downField("result").downField("tools").as[List[Json]]
    assert(tools.isRight)
    assertEquals(tools.toOption.get.size, 1)
    assertEquals(tools.toOption.get.head.hcursor.downField("name").as[String], Right("myHandler"))
  }

  test("McpServer.processRequest - tools/call success") {
    val analysis = ServiceAnalysis("handler1", "test.bosatsu", Nil, Nil, false, 0, 0, 0)
    val handlers = List(CompiledHandler("handler1", Nil, "code", analysis))
    val request = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 3.asJson,
      "method" -> "tools/call".asJson,
      "params" -> Json.obj(
        "name" -> "handler1".asJson,
        "arguments" -> Json.obj("x" -> 42.asJson)
      )
    )

    val response = McpServer.processRequest("TestServer", handlers, request).unsafeRunSync()
    val content = response.hcursor.downField("result").downField("content").as[List[Json]]
    assert(content.isRight)
    val text = content.toOption.get.head.hcursor.downField("text").as[String]
    assert(text.isRight)
    assert(text.toOption.get.contains("Executed handler1"))
  }

  test("McpServer.processRequest - tools/call not found") {
    val handlers = List.empty[CompiledHandler]
    val request = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 4.asJson,
      "method" -> "tools/call".asJson,
      "params" -> Json.obj("name" -> "nonexistent".asJson)
    )

    val response = McpServer.processRequest("TestServer", handlers, request).unsafeRunSync()
    assertEquals(response.hcursor.downField("error").downField("code").as[Int], Right(-32601))
    assert(response.hcursor.downField("error").downField("message").as[String].toOption.get.contains("not found"))
  }

  test("McpServer.processRequest - shutdown") {
    val request = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 5.asJson,
      "method" -> "shutdown".asJson
    )

    val response = McpServer.processRequest("TestServer", List.empty, request).unsafeRunSync()
    assert(response.hcursor.downField("result").focus.isDefined)
  }

  test("McpServer.processRequest - notifications/initialized returns null") {
    val request = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "method" -> "notifications/initialized".asJson
    )

    val response = McpServer.processRequest("TestServer", List.empty, request).unsafeRunSync()
    assert(response.isNull)
  }

  test("McpServer.processRequest - unknown method") {
    val request = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 6.asJson,
      "method" -> "unknown/method".asJson
    )

    val response = McpServer.processRequest("TestServer", List.empty, request).unsafeRunSync()
    assertEquals(response.hcursor.downField("error").downField("code").as[Int], Right(-32601))
    assert(response.hcursor.downField("error").downField("message").as[String].toOption.get.contains("Method not found"))
  }

  // ==========================================================================
  // HttpServer integration tests
  // ==========================================================================

  import org.http4s._
  import org.http4s.implicits._

  test("HttpServer.apiRoutes - GET / returns server info") {
    val analysis = ServiceAnalysis("h1", "f.bosatsu", Nil, Nil, false, 0, 0, 0)
    val handlers = List(CompiledHandler("h1", Nil, "code", analysis))
    val routes = HttpServer.apiRoutes(handlers)

    val request = Request[cats.effect.IO](Method.GET, uri"/")
    val response = routes.orNotFound.run(request).unsafeRunSync()

    assertEquals(response.status, Status.Ok)
    val body = response.as[Json].unsafeRunSync()
    assertEquals(body.hcursor.downField("name").as[String], Right("Bosatsu Service"))
    assertEquals(body.hcursor.downField("handlers").as[List[String]], Right(List("h1")))
  }

  test("HttpServer.apiRoutes - GET /handlers returns handler list") {
    val analysis = ServiceAnalysis("handler1", "f.bosatsu", Nil, Nil, true, 5, 3, 2)
    val handlers = List(CompiledHandler("handler1", Nil, "code", analysis))
    val routes = HttpServer.apiRoutes(handlers)

    val request = Request[cats.effect.IO](Method.GET, uri"/handlers")
    val response = routes.orNotFound.run(request).unsafeRunSync()

    assertEquals(response.status, Status.Ok)
    val body = response.as[Json].unsafeRunSync()
    val handlerList = body.as[List[Json]]
    assert(handlerList.isRight)
    assertEquals(handlerList.toOption.get.size, 1)
    assertEquals(handlerList.toOption.get.head.hcursor.downField("name").as[String], Right("handler1"))
    assertEquals(handlerList.toOption.get.head.hcursor.downField("canBatch").as[Boolean], Right(true))
  }

  test("HttpServer.apiRoutes - GET /handlers/:name/analysis returns analysis") {
    val analysis = ServiceAnalysis("myHandler", "test.bosatsu", Nil, Nil, false, 10, 5, 5)
    val handlers = List(CompiledHandler("myHandler", Nil, "code", analysis))
    val routes = HttpServer.apiRoutes(handlers)

    val request = Request[cats.effect.IO](Method.GET, uri"/handlers/myHandler/analysis")
    val response = routes.orNotFound.run(request).unsafeRunSync()

    assertEquals(response.status, Status.Ok)
    val body = response.as[Json].unsafeRunSync()
    assertEquals(body.hcursor.downField("handlerName").as[String], Right("myHandler"))
    assertEquals(body.hcursor.downField("totalQueries").as[Int], Right(10))
  }

  test("HttpServer.apiRoutes - GET /handlers/:name/analysis not found") {
    val handlers = List.empty[CompiledHandler]
    val routes = HttpServer.apiRoutes(handlers)

    val request = Request[cats.effect.IO](Method.GET, uri"/handlers/nonexistent/analysis")
    val response = routes.orNotFound.run(request).unsafeRunSync()

    assertEquals(response.status, Status.NotFound)
  }

  test("HttpServer.apiRoutes - POST /handlers/:name executes handler") {
    val analysis = ServiceAnalysis("exec", "test.bosatsu", Nil, Nil, false, 0, 0, 0)
    val handlers = List(CompiledHandler("exec", Nil, "code", analysis))
    val routes = HttpServer.apiRoutes(handlers)

    val body = Json.obj("input" -> "test".asJson)
    val request = Request[cats.effect.IO](Method.POST, uri"/handlers/exec")
      .withEntity(body)

    val response = routes.orNotFound.run(request).unsafeRunSync()

    assertEquals(response.status, Status.Ok)
    val responseBody = response.as[Json].unsafeRunSync()
    assertEquals(responseBody.hcursor.downField("success").as[Boolean], Right(true))
    assertEquals(responseBody.hcursor.downField("handler").as[String], Right("exec"))
  }

  test("HttpServer.apiRoutes - POST /handlers/:name not found") {
    val handlers = List.empty[CompiledHandler]
    val routes = HttpServer.apiRoutes(handlers)

    val body = Json.obj("input" -> "test".asJson)
    val request = Request[cats.effect.IO](Method.POST, uri"/handlers/missing")
      .withEntity(body)

    val response = routes.orNotFound.run(request).unsafeRunSync()

    assertEquals(response.status, Status.NotFound)
  }

  test("HttpServer.httpApp - routes under /api prefix") {
    val analysis = ServiceAnalysis("h", "f.bosatsu", Nil, Nil, false, 0, 0, 0)
    val handlers = List(CompiledHandler("h", Nil, "code", analysis))
    val app = HttpServer.httpApp(handlers)

    val request = Request[cats.effect.IO](Method.GET, uri"/api/")
    val response = app.run(request).unsafeRunSync()

    assertEquals(response.status, Status.Ok)
  }

  test("HttpServer.httpApp - non-api routes return not found") {
    val handlers = List.empty[CompiledHandler]
    val app = HttpServer.httpApp(handlers)

    val request = Request[cats.effect.IO](Method.GET, uri"/other")
    val response = app.run(request).unsafeRunSync()

    assertEquals(response.status, Status.NotFound)
  }

  // ==========================================================================
  // ServiceBuilder additional tests
  // ==========================================================================

  test("ServiceBuilder.build - produces valid build result") {
    val source = """
package BuildService

def myHandler(x: Int) -> Int:
  x
"""
    val result = ServiceBuilder.build(source, "build.bosatsu", BuildTarget.Standalone)
    assert(result.isRight, s"Expected Right but got: $result")

    val buildResult = result.toOption.get
    assert(buildResult.handlers.nonEmpty, "Expected at least one handler")
    assert(buildResult.jsCode.nonEmpty, "Expected non-empty JS code")
    assertEquals(buildResult.target, BuildTarget.Standalone)
  }

  test("ServiceBuilder.build - with Vercel target") {
    val source = """
package VercelService

def handler(name: String) -> String:
  name
"""
    val result = ServiceBuilder.build(source, "vercel.bosatsu", BuildTarget.Vercel)
    assert(result.isRight)
    assertEquals(result.toOption.get.target, BuildTarget.Vercel)
  }

  test("ServiceBuilder.build - with AwsLambda target") {
    val source = """
package LambdaService

def handler(arg: Int) -> Int:
  arg.add(1)
"""
    val result = ServiceBuilder.build(source, "lambda.bosatsu", BuildTarget.AwsLambda)
    assert(result.isRight)
    assertEquals(result.toOption.get.target, BuildTarget.AwsLambda)
  }

  test("ServiceBuilder.compileHandlers - function with complex body") {
    val source = """
package Complex

def compute(x: Int, y: Int) -> Int:
  z = x.add(y)
  w = z.times(2)
  w
"""
    val result = ServiceBuilder.compileHandlers(source, "complex.bosatsu")
    assert(result.isRight)

    val handlers = result.toOption.get
    assertEquals(handlers.size, 1)
    assertEquals(handlers.head.name, "compute")
    assertEquals(handlers.head.params.size, 2)
  }

  // ==========================================================================
  // ServiceCli tests (testing action creation)
  // ==========================================================================

  test("ServiceCli.AnalyzeAction - construction") {
    import fs2.io.file.Path
    val action = ServiceCli.AnalyzeAction(Path("test.bosatsu"), Some("handler"), true)
    assertEquals(action.outputJson, true)
    assertEquals(action.functionName, Some("handler"))
  }

  test("ServiceCli.ValidateAction - construction") {
    import fs2.io.file.Path
    val action = ServiceCli.ValidateAction(Path("test.bosatsu"))
    // Just verify construction works
    assert(action.sourceFile.toString.contains("test.bosatsu"))
  }

  test("ServiceCli.BuildAction - construction") {
    import fs2.io.file.Path
    val action = ServiceCli.BuildAction(Path("test.bosatsu"), Path("dist"), BuildTarget.Vercel)
    assertEquals(action.target, BuildTarget.Vercel)
  }

  test("ServiceCli.ServeAction - construction") {
    import fs2.io.file.Path
    val action = ServiceCli.ServeAction(Path("test.bosatsu"), 8080, None, None)
    assertEquals(action.port, 8080)
  }

  test("ServiceCli.McpAction - construction") {
    import fs2.io.file.Path
    val action = ServiceCli.McpAction(Path("test.bosatsu"), None, Some("MyServer"))
    assertEquals(action.name, Some("MyServer"))
  }

  test("ServiceCli.parse - analyze command") {
    val args = List("analyze", "test.bosatsu", "--json")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    assert(result.toOption.get.isInstanceOf[ServiceCli.AnalyzeAction])
  }

  test("ServiceCli.parse - validate command") {
    val args = List("validate", "test.bosatsu")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    assert(result.toOption.get.isInstanceOf[ServiceCli.ValidateAction])
  }

  test("ServiceCli.parse - build command") {
    val args = List("build", "test.bosatsu", "--output", "out", "--target", "vercel")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    val action = result.toOption.get.asInstanceOf[ServiceCli.BuildAction]
    assertEquals(action.target, BuildTarget.Vercel)
  }

  test("ServiceCli.parse - serve command") {
    val args = List("serve", "test.bosatsu", "--port", "9000")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    val action = result.toOption.get.asInstanceOf[ServiceCli.ServeAction]
    assertEquals(action.port, 9000)
  }

  test("ServiceCli.parse - mcp command") {
    val args = List("mcp", "test.bosatsu", "--name", "TestServer")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    val action = result.toOption.get.asInstanceOf[ServiceCli.McpAction]
    assertEquals(action.name, Some("TestServer"))
  }

  test("ServiceCli.parse - invalid command") {
    val args = List("invalid", "test.bosatsu")
    val result = ServiceCli.parse(args)
    assert(result.isLeft)
  }

  test("ServiceCli.parse - analyze with function filter") {
    val args = List("analyze", "test.bosatsu", "--function", "myHandler")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    val action = result.toOption.get.asInstanceOf[ServiceCli.AnalyzeAction]
    assertEquals(action.functionName, Some("myHandler"))
  }

  test("ServiceCli.parse - build with aws-lambda target") {
    val args = List("build", "test.bosatsu", "--target", "aws-lambda")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    val action = result.toOption.get.asInstanceOf[ServiceCli.BuildAction]
    assertEquals(action.target, BuildTarget.AwsLambda)
  }

  test("ServiceCli.parse - build with standalone target (default)") {
    val args = List("build", "test.bosatsu", "--target", "standalone")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    val action = result.toOption.get.asInstanceOf[ServiceCli.BuildAction]
    assertEquals(action.target, BuildTarget.Standalone)
  }

  test("ServiceCli.parse - build with unknown target defaults to standalone") {
    val args = List("build", "test.bosatsu", "--target", "unknown")
    val result = ServiceCli.parse(args)
    assert(result.isRight)
    val action = result.toOption.get.asInstanceOf[ServiceCli.BuildAction]
    assertEquals(action.target, BuildTarget.Standalone)
  }

  // ==========================================================================
  // ServiceProtocol additional tests
  // ==========================================================================

  test("ServiceAnalysis - batchingEfficiency with different ratios") {
    val analysis100 = ServiceAnalysis("h", "f", Nil, Nil, true, 10, 10, 10)
    assertEquals(analysis100.batchingEfficiency, "100%")

    val analysis25 = ServiceAnalysis("h", "f", Nil, Nil, true, 100, 25, 25)
    assertEquals(analysis25.batchingEfficiency, "25%")
  }

  test("BuildTarget - all variants are distinct") {
    val targets = Set(BuildTarget.Standalone, BuildTarget.Vercel, BuildTarget.AwsLambda)
    assertEquals(targets.size, 3)
  }

  test("OperationKind - all variants are distinct") {
    val kinds = Set(OperationKind.Read, OperationKind.Write, OperationKind.Unknown)
    assertEquals(kinds.size, 3)
  }
}
