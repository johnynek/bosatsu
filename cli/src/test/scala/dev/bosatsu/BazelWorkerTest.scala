package dev.bosatsu

import cats.effect.{Deferred, IO}
import cats.syntax.all._
import com.fasterxml.jackson.databind.ObjectMapper
import dev.bosatsu.tool.ExitCode
import java.io.{ByteArrayOutputStream, InputStream, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import java.util.Base64
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.util.Random

class BazelWorkerTest extends FileCommandFixtures {
  private val jsonMapper = new ObjectMapper()

  private final class ControlledInputStream extends InputStream {
    private val bytes = new LinkedBlockingQueue[Integer]()
    @volatile var closed = false

    def offer(value: String): Unit =
      value.getBytes(UTF_8).foreach(byte => bytes.add(Integer.valueOf(byte & 0xff)))

    def finish(): Unit = {
      bytes.add(Integer.valueOf(-1)): Unit
    }

    override def read(): Int = bytes.take().intValue()

    override def close(): Unit = closed = true
  }

  private final class FlushedLineOutputStream extends OutputStream {
    private val current = new ByteArrayOutputStream
    private val complete = scala.collection.mutable.Queue.empty[String]
    private val flushed = new LinkedBlockingQueue[String]()
    @volatile var closed = false

    override def write(value: Int): Unit = synchronized {
      if (value == '\n') {
        complete.enqueue(current.toString(UTF_8))
        current.reset()
      } else current.write(value)
    }

    override def flush(): Unit = synchronized {
      while (complete.nonEmpty) flushed.add(complete.dequeue()): Unit
    }

    def nextLine(): String =
      Option(flushed.poll(10, TimeUnit.SECONDS)).getOrElse {
        throw new AssertionError("persistent worker did not flush a response")
      }

    def hasExtraLine: Boolean = flushed.poll() != null

    override def close(): Unit = closed = true
  }

  private final class TrackingPrintStream(out: ByteArrayOutputStream)
      extends PrintStream(out, false, UTF_8) {
    @volatile var closed = false

    override def close(): Unit = {
      closed = true
      super.close()
    }
  }

  private def writeBytes(root: Path, relative: String, bytes: Array[Byte]): IO[Unit] =
    IO.blocking {
      val path = root.resolve(relative)
      Files.createDirectories(path.getParent)
      Files.write(path, bytes)
      ()
    }

  private def parameterInvocation(
      runner: FileCommandRunner,
      root: Path,
      content: String
  ): IO[FileCommandRunner.Result] =
    write(root, "worker.params", content) *>
      BazelWorker.readParameterFile(root, "@worker.params").flatMap { invocation =>
        runner.runWorker(root, invocation.commandArguments)
      }

  private def encoded(
      values: List[String],
      delimiter: String,
      finalTerminator: Boolean
  ): String = {
    val joined = values.mkString(delimiter)
    if (values.lastOption.contains("")) joined + delimiter
    else if (finalTerminator) joined + delimiter
    else joined
  }

  private def bytes(root: Path, relative: String): IO[List[Byte]] =
    IO.blocking(Files.readAllBytes(root.resolve(relative)).toList)

  private def compareOutput(
      direct: Path,
      worker: Path,
      relative: String
  ): IO[Unit] =
    (bytes(direct, relative), bytes(worker, relative)).tupled.map {
      case (expected, actual) => assertEquals(actual, expected, relative)
    }

  private def runWorkerTo(
      runner: FileCommandRunner,
      root: Path,
      arguments: List[String]
  ): IO[FileCommandRunner.Result] = {
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream
    val outStream = new PrintStream(out, false, UTF_8)
    val errStream = new PrintStream(err, false, UTF_8)
    runner.runWorkerTo(root, arguments, outStream, errStream).map { code =>
      FileCommandRunner.Result(code, out.toString(UTF_8), err.toString(UTF_8))
    }
  }

  test("worker request envelopes use defaults, aliases, nulls, and input-order duplicates") {
    val defaults = BazelWorker.decodeRequest("{}").toOption
    assertEquals(
      defaults,
      Some(BazelWorker.ActionRequest(Nil, Nil, 0, 0, ""))
    )

    val decoded = BazelWorker.decodeRequest(
      """{"arguments":["headers","path with spaces","雪"],"inputs":[{"path":"雪 file","digest":"AQID","ignored":{"nested":true}}],"request_id":-2147483648,"requestId":"2147483647","request_id":null,"verbosity":"-4","cancel":false,"sandbox_dir":"work","sandboxDir":null,"unknown":[1,2]}"""
    )
    assertEquals(
      decoded,
      Right(
        BazelWorker.ActionRequest(
          List("headers", "path with spaces", "雪"),
          List(BazelWorker.Input("雪 file", Vector[Byte](1, 2, 3))),
          Int.MaxValue,
          -4,
          "work"
        )
      )
    )

    assertEquals(
      BazelWorker.decodeRequest("""{"requestId":7,"cancel":true,"arguments":["headers"]}"""),
      Right(BazelWorker.CancelRequest(7))
    )
    assertEquals(
      BazelWorker.decodeRequest("""{"arguments":null,"inputs":null,"cancel":null}"""),
      Right(BazelWorker.ActionRequest(Nil, Nil, 0, 0, ""))
    )
    assertEquals(
      BazelWorker.decodeRequest("""{"requestId":1e2,"verbosity":1.0}"""),
      Right(BazelWorker.ActionRequest(Nil, Nil, 100, 1, ""))
    )
    assertEquals(
      BazelWorker.decodeRequest("""{"request_id":"1.00e+2"}"""),
      Right(BazelWorker.ActionRequest(Nil, Nil, 100, 0, ""))
    )
  }

  test("worker response encoding round trips generated values as one compact JSON line") {
    val random = new Random(2484L)
    val codePoints = Array(0, 8, 9, 10, 13, 31, 34, 39, 92, 127, 0x00e9, 0x96ea, 0x1f642)
    def generatedString(maxSize: Int): String = {
      val size = random.nextInt(maxSize + 1)
      val builder = new java.lang.StringBuilder
      var index = 0
      while (index < size) {
        builder.appendCodePoint(codePoints(random.nextInt(codePoints.length)))
        index += 1
      }
      builder.toString
    }

    (0 until 100).foreach { _ =>
      val args = List.fill(random.nextInt(8))(generatedString(30))
      val inputs = List(BazelWorker.Input(generatedString(24), Vector.empty))
      val requestId = random.nextInt()
      val verbosity = random.nextInt()
      val request = Json.JObject(
        List(
          "arguments" -> Json.JArray(args.map(Json.JString(_)).toVector),
          "inputs" -> Json.JArray(
            Vector(
              Json.JObject(
                List("path" -> Json.JString(inputs.head.path), "unknown" -> Json.JObject(Nil))
              )
            )
          ),
          "requestId" -> Json.JNumberStr(requestId.toString),
          "request_id" -> Json.JNull,
          "verbosity" -> Json.JString(verbosity.toString),
          "cancel" -> Json.JNull,
          "sandboxDir" -> Json.JNull,
          "unknown" -> Json.JObject(List("nested" -> Json.JArray(Vector(Json.JBool.True))))
        )
      ).render
      BazelWorker.decodeRequest(request) match {
        case Right(action: BazelWorker.ActionRequest) =>
          assertEquals(action, BazelWorker.ActionRequest(args, inputs, requestId, verbosity, ""))
        case other                                    => fail(s"expected action request, got $other")
      }

      val response = BazelWorker.Response(
        random.nextInt(),
        random.nextInt(),
        generatedString(400)
      )
      val encoded = BazelWorker.encodeResponse(response)
      assert(!encoded.contains('\n'))
      assert(!encoded.contains('\r'))
      val parsed = jsonMapper.readTree(encoded)
      assertEquals(parsed.get("requestId").intValue(), response.requestId)
      assertEquals(parsed.get("exitCode").intValue(), response.exitCode)
      assertEquals(parsed.get("output").textValue(), response.output)
      assertEquals(parsed.get("wasCancelled").booleanValue(), false)
    }

    val longDiagnostic = ("bad \"input\" \\ line\n" * 5000) + "雪"
    val encoded = BazelWorker.encodeResponse(BazelWorker.Response(0, 1, longDiagnostic))
    assert(!encoded.contains('\n'))
    assert(!encoded.contains('\r'))
    assertEquals(jsonMapper.readTree(encoded).get("output").textValue(), longDiagnostic)
    assert(encoded.contains("\"wasCancelled\":false"))
  }

  test("worker request decoding accepts standard and URL-safe base64 digests") {
    val random = new Random(0L)
    val vectors = List(
      Array.emptyByteArray,
      Array[Byte](1),
      Array[Byte](0xfb.toByte, 0xff.toByte, 0xef.toByte),
      Array.fill[Byte](32)(0)
    ) ::: List.fill(50)(Array.fill(random.nextInt(64))(random.nextInt(256).toByte))
    val encoders = List(
      Base64.getEncoder,
      Base64.getEncoder.withoutPadding(),
      Base64.getUrlEncoder,
      Base64.getUrlEncoder.withoutPadding()
    )

    vectors.foreach { bytes =>
      encoders.foreach { encoder =>
        val encoded = encoder.encodeToString(bytes)
        val json = s"""{"inputs":[{"digest":"$encoded"}]}"""
        assertEquals(
          BazelWorker.decodeRequest(json),
          Right(BazelWorker.ActionRequest(Nil, List(BazelWorker.Input("", bytes.toVector)), 0, 0, ""))
        )
      }
    }

    assertEquals(
      BazelWorker.decodeRequest("""{"inputs":[{}, {"digest":""}, {"path":null,"digest":null}]}"""),
      Right(
        BazelWorker.ActionRequest(
          Nil,
          List(
            BazelWorker.Input("", Vector.empty),
            BazelWorker.Input("", Vector.empty),
            BazelWorker.Input("", Vector.empty)
          ),
          0,
          0,
          ""
        )
      )
    )
  }

  test("worker request decoding rejects malformed JSON and invalid known fields") {
    val invalid = List(
      "[]",
      "{",
      "{} {}",
      """{"arguments":"headers"}""",
      """{"arguments":[null]}""",
      """{"inputs":[null]}""",
      """{"inputs":[{"path":1}]}""",
      """{"inputs":[{"digest":1}]}""",
      """{"inputs":[{"digest":"A"}]}""",
      """{"inputs":[{"digest":"+-_"}]}""",
      """{"requestId":1.5}""",
      """{"requestId":2147483648}""",
      """{"request_id":"-2147483649"}""",
      """{"verbosity":true}""",
      """{"cancel":"false"}""",
      """{"sandboxDir":[]}"""
    )
    invalid.foreach { json =>
      val decoded = BazelWorker.decodeRequest(json)
      assert(decoded.isLeft, s"expected request to fail: $json, got $decoded")
    }
  }

  test("the parameter decoder preserves argument lines across LF and CRLF") {
    withRunner { (root, _) =>
      val samples = List(
        "",
        " ",
        " with spaces ",
        "雪",
        "'quoted'",
        "\\slash",
        "@nested.params"
      )
      val generated = for {
        operation <- BazelWorker.Operation.registered
        argument <- samples
      } yield List(operation.commandName, argument)
      val vectors = List(
        List("headers"),
        List("headers", "", "with spaces", "雪", "'quoted'", "\\slash", "@nested.params"),
        List("headers", "", ""),
        List("emit-c", "--out", "")
      ) ::: generated
      val lineEndings = List("\n", "\r\n")
      for {
        _ <- vectors.zipWithIndex.toList.traverse_ { case (values, index) =>
          lineEndings.zipWithIndex.traverse_ { case (delimiter, endingIndex) =>
            List(false, true).traverse_ { finalTerminator =>
              val name = s"params-$index-$endingIndex-${if (finalTerminator) 1 else 0}"
              val text = encoded(values, delimiter, finalTerminator)
              for {
                _ <- write(root, name, text)
                decoded <- BazelWorker.readParameterFile(root, s"@$name")
                _ = assertEquals(decoded.commandArguments, values)
              } yield ()
            }
          }
        }
        _ <- write(root, "mixed.params", "headers\r\nfirst\n\r\nlast\rinside")
        mixed <- BazelWorker.readParameterFile(root, "@mixed.params")
        _ = assertEquals(
          mixed.commandArguments,
          List("headers", "first", "", "last\rinside")
        )
      } yield ()
    }
  }

  test("empty, malformed, unreadable, and invalid parameter-file arguments fail") {
    withRunner { (root, _) =>
      val invalid = List("", "@", "params", "@@")
      for {
        _ <- invalid.traverse_ { argument =>
          BazelWorker.readParameterFile(root, argument).attempt.map { result =>
            assert(result.isLeft, s"expected $argument to fail")
          }
        }
        _ <- write(root, "empty.params", "")
        empty <- BazelWorker.readParameterFile(root, "@empty.params").attempt
        _ = assert(empty.swap.toOption.exists(_.getMessage.contains("is empty")))
        _ <- write(root, "empty-operation.params", "\nargument")
        emptyOperation <- BazelWorker
          .readParameterFile(root, "@empty-operation.params")
          .attempt
        _ = assert(
          emptyOperation.swap.toOption.exists(_.getMessage.contains("empty worker operation"))
        )
        _ <- writeBytes(root, "invalid-utf8.params", Array(0xc3.toByte, 0x28.toByte))
        badUtf8 <- BazelWorker.readParameterFile(root, "@invalid-utf8.params").attempt
        _ = assert(badUtf8.swap.toOption.exists(_.getMessage.contains("not valid UTF-8")))
        missing <- BazelWorker.readParameterFile(root, "@missing.params").attempt
        _ = assert(missing.swap.toOption.exists(_.getMessage.contains("cannot read parameter file")))
      } yield ()
    }
  }

  test("only the four registered build operations are classified for workers") {
    assertEquals(
      BazelWorker.Operation.registered.map(_.commandName),
      List("headers", "compile-packages", "validate-graph", "emit-c")
    )
    assertEquals(
      BazelWorker.Operation.registered.flatMap(op =>
        BazelWorker.Operation.fromCommandName(op.commandName)
      ),
      BazelWorker.Operation.registered
    )
    assertEquals(BazelWorker.Operation.fromCommandName("check"), None)
  }

  test("worker and standalone operations produce the same outputs and diagnostics") {
    withRunner { (base, runner) =>
      val direct = base.resolve("direct")
      val worker = base.resolve("worker")
      val headerSource = "src/space path/雪 API.bosatsu"
      val rootNode =
        """{"schema_version":1,"kind":"group","label":"//:root","members":[{"package":"Example/Root","source":"src/Root.bosatsu"}],"deps":[]}"""
      val headerSourceText =
        "package Example/Api\nexport value\nvalue = 1\n"
      val packageSource =
        "package Example/Unit\nexport value\nvalue = 2\n"
      val suiteSource =
        "package Example/Suite\nchecks = Assertion(True, \"worker parity\")\n"
      for {
        _ <- IO.blocking {
          Files.createDirectories(direct)
          Files.createDirectories(worker)
        }
        _ <- write(direct, headerSource, headerSourceText)
        _ <- write(worker, headerSource, headerSourceText)
        headerArgs = List("headers", "--out", "gen/headers.json", "--src", headerSource)
        headerDirect <- runner.run(direct, headerArgs)
        headerWorker <- parameterInvocation(runner, worker, encoded(headerArgs, "\n", true))
        _ = assertEquals(headerWorker, headerDirect)
        _ <- compareOutput(direct, worker, "gen/headers.json")

        _ <- write(direct, "src/Package.bosatsu", packageSource)
        _ <- write(worker, "src/Package.bosatsu", packageSource)
        compileArgs = List(
          "compile-packages",
          "--packages-out",
          "gen/unit.pkg",
          "--sig-out",
          "gen/unit.sig",
          "--src",
          "src/Package.bosatsu"
        )
        compileDirect <- runner.run(direct, compileArgs)
        compileWorker <- parameterInvocation(runner, worker, encoded(compileArgs, "\r\n", true))
        _ = assertEquals(compileWorker, compileDirect)
        _ <- compareOutput(direct, worker, "gen/unit.pkg")
        _ <- compareOutput(direct, worker, "gen/unit.sig")

        _ <- write(direct, "graph/root.json", rootNode)
        _ <- write(worker, "graph/root.json", rootNode)
        graphArgs = List(
          "validate-graph",
          "--root",
          "graph/root.json",
          "--out",
          "gen/validated"
        )
        graphDirect <- runner.run(direct, graphArgs)
        graphWorker <- parameterInvocation(runner, worker, encoded(graphArgs, "\n", false))
        _ = assertEquals(graphWorker, graphDirect)
        _ <- compareOutput(direct, worker, "gen/validated")

        _ <- write(direct, "src/Suite.bosatsu", suiteSource)
        _ <- write(worker, "src/Suite.bosatsu", suiteSource)
        suiteCompile = List(
          "compile-packages",
          "--packages-out",
          "suite.pkg",
          "--sig-out",
          "suite.sig",
          "--src",
          "src/Suite.bosatsu"
        )
        _ <- runner.run(direct, suiteCompile).map(success)
        _ <- runner.run(worker, suiteCompile).map(success)
        runtime =
          """{"schema_version":1,"kind":"local","packages":["suite.pkg"],"externals":[]}"""
        _ <- write(direct, "runtime.json", runtime)
        _ <- write(worker, "runtime.json", runtime)
        emitArgs = List(
          "emit-c",
          "--runtime-manifest",
          "runtime.json",
          "--test-packages",
          "suite.pkg",
          "--out",
          "gen/tests.c"
        )
        emitDirect <- runner.run(direct, emitArgs)
        emitWorker <- parameterInvocation(runner, worker, encoded(emitArgs, "\r\n", true))
        _ = assertEquals(emitWorker, emitDirect)
        _ <- compareOutput(direct, worker, "gen/tests.c")

        missingHeaders = List("headers", "--out", "bad/headers.json", "--src", "missing.bosatsu")
        badHeadersDirect <- runner.run(worker, missingHeaders)
        badHeadersWorker <- parameterInvocation(runner, worker, encoded(missingHeaders, "\n", true))
        _ = assertEquals(badHeadersWorker, badHeadersDirect)
        _ = failure(badHeadersDirect)
        _ = assert(!Files.exists(direct.resolve("bad/headers.json")))
        _ = assert(!Files.exists(worker.resolve("bad/headers.json")))

        _ <- write(direct, "Invalid.bosatsu", "package Example/Invalid\nexport value\nvalue = missing_name\n")
        _ <- write(worker, "Invalid.bosatsu", "package Example/Invalid\nexport value\nvalue = missing_name\n")
        invalidCompile = List(
          "compile-packages",
          "--packages-out",
          "bad/invalid.pkg",
          "--sig-out",
          "bad/invalid.sig",
          "--src",
          "Invalid.bosatsu"
        )
        invalidCompileDirect <- runner.run(worker, invalidCompile)
        invalidCompileWorker <- parameterInvocation(runner, worker, encoded(invalidCompile, "\r\n", true))
        _ = assertEquals(invalidCompileWorker, invalidCompileDirect)
        _ = failure(invalidCompileDirect)

        missingGraph = List("validate-graph", "--root", "missing.json", "--out", "bad/validated")
        missingGraphDirect <- runner.run(worker, missingGraph)
        missingGraphWorker <- parameterInvocation(runner, worker, encoded(missingGraph, "\n", true))
        _ = assertEquals(missingGraphWorker, missingGraphDirect)
        _ = failure(missingGraphDirect)

        missingEmit = List(
          "emit-c",
          "--runtime-manifest",
          "missing-runtime.json",
          "--test-packages",
          "suite.pkg",
          "--out",
          "bad/tests.c"
        )
        missingEmitDirect <- runner.run(worker, missingEmit)
        missingEmitWorker <- parameterInvocation(runner, worker, encoded(missingEmit, "\n", true))
        _ = assertEquals(missingEmitWorker, missingEmitDirect)
        _ = failure(missingEmitDirect)
        _ = assert(!Files.exists(worker.resolve("bad/tests.c")))

        literalAt = List("headers", "--out", "@literal-output.json", "--src", headerSource)
        literalAtResult <- parameterInvocation(runner, worker, encoded(literalAt, "\n", true))
        _ = success(literalAtResult)
        _ = assert(Files.isRegularFile(worker.resolve("@literal-output.json")))
        _ = assert(!listPaths(worker).exists(_.contains(".bosatsu-stage-")))
      } yield ()
    }
  }

  test("worker requests reject legacy operations and parser-owned cache options") {
    withRunner { (root, runner) =>
      val unsupported = List(
        "fetch",
        "publish",
        "eval",
        "test",
        "transpile",
        "protoc-plugin",
        "check",
        "assemble"
      )
      val operations = BazelWorker.Operation.registered.map(_.commandName)
      for {
        _ <- unsupported.traverse_ { command =>
          runner.runWorker(root, List(command)).map { result =>
            failure(result)
            assert(result.stderr.contains("unsupported bazel worker operation"))
          }
        }
        _ <- operations.traverse_ { operation =>
          List(
            "--cache_dir" -> "--cache_dir",
            "--cache_dir=cache" -> "--cache_dir",
            "--not-a-worker-option" -> "--not-a-worker-option"
          ).traverse_ { case (option, expectedDiagnostic) =>
              runner.runWorker(root, List(operation, option)).map { result =>
                failure(result)
                assert(
                  result.stderr.contains(expectedDiagnostic),
                  result.stderr
                )
              }
          }
        }
        _ = assert(!Files.exists(root.resolve("cache")))
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
      } yield ()
    }
  }

  test("a failed worker publication rolls back and the same runner can retry") {
    withRunner { (root, runner) =>
      val source = "package Example/Retry\nexport value\nvalue = 42\n"
      val args = List(
        "compile-packages",
        "--packages-out",
        "out/package.pkg",
        "--sig-out",
        "out/signature.sig",
        "--src",
        "Retry.bosatsu"
      )
      for {
        _ <- write(root, "Retry.bosatsu", source)
        _ <- write(root, "out/signature.sig", "previous signature")
        _ <- IO.blocking(Files.createDirectory(root.resolve("out/package.pkg")))
        failed <- parameterInvocation(runner, root, encoded(args, "\n", true))
        _ = failure(failed)
        _ = assert(failed.stderr.contains("expected a regular output file"), failed.stderr)
        oldSignature <- read(root, "out/signature.sig")
        _ = assertEquals(oldSignature, "previous signature")
        _ = assert(Files.isDirectory(root.resolve("out/package.pkg")))
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
        _ <- IO.blocking(Files.delete(root.resolve("out/package.pkg")))
        retried <- parameterInvocation(runner, root, encoded(args, "\n", true))
        _ = success(retried)
        _ = assert(Files.isRegularFile(root.resolve("out/package.pkg")))
        _ = assert(Files.isRegularFile(root.resolve("out/signature.sig")))
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
      } yield ()
    }
  }

  test("worker publication retains identical bytes and follows output symlinks") {
    withRunner { (root, runner) =>
      val args = List("headers", "--out", "alias.json", "--src", "Source.bosatsu")
      for {
        _ <- write(root, "Source.bosatsu", "package Example/Source\nexport value\nvalue = 1\n")
        _ <- IO.blocking(Files.createSymbolicLink(root.resolve("alias.json"), Path.of("generated/headers.json")))
        first <- parameterInvocation(runner, root, encoded(args, "\n", true))
        _ = success(first)
        target = root.resolve("generated/headers.json")
        firstBytes <- bytes(root, "generated/headers.json")
        _ <- IO.blocking(Files.setLastModifiedTime(target, FileTime.fromMillis(100000L)))
        timestamp = Files.getLastModifiedTime(target)
        second <- parameterInvocation(runner, root, encoded(args, "\r\n", true))
        _ = success(second)
        secondBytes <- bytes(root, "generated/headers.json")
        _ = assertEquals(secondBytes, firstBytes)
        _ = assertEquals(Files.getLastModifiedTime(target), timestamp)
        _ = assert(Files.isSymbolicLink(root.resolve("alias.json")))
        _ = assertEquals(Files.readSymbolicLink(root.resolve("alias.json")), Path.of("generated/headers.json"))
      } yield ()
    }
  }

  test("the streaming worker route rejects an absent or unregistered operation") {
    withRunner { (root, runner) =>
      for {
        empty <- runWorkerTo(runner, root, Nil)
        _ = assertEquals(empty.exitCode, ExitCode.Error)
        _ = assertEquals(empty.stderr, "expected a bazel worker operation\n")
        unsupported <- runWorkerTo(runner, root, List("fetch"))
        _ = assertEquals(unsupported.exitCode, ExitCode.Error)
        _ = assertEquals(unsupported.stderr, "unsupported bazel worker operation: fetch\n")
      } yield ()
    }
  }

  test("persistent requests finish and flush before the next input frame") {
    withRunner { (root, runner) =>
      val input = new ControlledInputStream
      val output = new FlushedLineOutputStream
      val errorBytes = new ByteArrayOutputStream
      val diagnostics = new TrackingPrintStream(errorBytes)
      val source = "package Example/Persistent\nexport value\nvalue = 42\n"
      val first =
        """{"arguments":["headers","--src","Source.bosatsu","--out","first.json"],"requestId":7,"inputs":[{"path":"Source.bosatsu","digest":""}],"unknown":true}"""
      val bad = """{"arguments":["fetch"],"requestId":8}"""
      val sandbox =
        """{"arguments":["headers","--src","Source.bosatsu","--out","sandbox.json"],"requestId":9,"sandboxDir":"elsewhere"}"""
      val cancel = """{"requestId":10,"cancel":true}"""
      val last =
        """{"arguments":["headers","--src","Source.bosatsu","--out","last.json"]}"""

      for {
        _ <- write(root, "Source.bosatsu", source)
        ready <- Deferred[IO, Unit]
        fiber <- (ready.complete(()) *> BazelWorker
          .runPersistent(root, input, output, diagnostics, runner)).start
        _ <- ready.get
        _ <- IO.blocking(input.offer(first + "\r\n"))
        firstLine <- IO.blocking(output.nextLine())
        firstResponse = jsonMapper.readTree(firstLine)
        _ = assertEquals(firstResponse.get("requestId").intValue(), 7)
        _ = assertEquals(firstResponse.get("exitCode").intValue(), 0)
        _ = assert(Files.isRegularFile(root.resolve("first.json")))
        _ = assert(!input.closed)
        _ <- IO.blocking(input.offer(bad + "\n"))
        badLine <- IO.blocking(output.nextLine())
        badResponse = jsonMapper.readTree(badLine)
        _ = assertEquals(badResponse.get("requestId").intValue(), 8)
        _ = assertEquals(badResponse.get("exitCode").intValue(), 1)
        _ = assert(badResponse.get("output").asText().contains("unsupported bazel worker operation: fetch"))
        _ <- IO.blocking(input.offer(sandbox + "\n"))
        sandboxLine <- IO.blocking(output.nextLine())
        sandboxResponse = jsonMapper.readTree(sandboxLine)
        _ = assertEquals(sandboxResponse.get("requestId").intValue(), 9)
        _ = assertEquals(sandboxResponse.get("exitCode").intValue(), 1)
        _ = assert(sandboxResponse.get("output").asText().contains("unsupported nonempty sandboxDir"))
        _ = assert(!Files.exists(root.resolve("sandbox.json")))
        _ <- IO.blocking(input.offer(cancel + "\n"))
        _ <- IO.blocking(input.offer(last + "\n"))
        lastLine <- IO.blocking(output.nextLine())
        lastResponse = jsonMapper.readTree(lastLine)
        _ = assertEquals(lastResponse.get("requestId").intValue(), 0)
        _ = assertEquals(lastResponse.get("exitCode").intValue(), 0)
        _ = assert(Files.isRegularFile(root.resolve("last.json")))
        _ <- IO.blocking(input.finish())
        result <- fiber.joinWithNever
        _ = assertEquals(result, ExitCode.Success)
        _ = assert(!output.hasExtraLine)
        _ = assertEquals(errorBytes.size(), 0)
        _ = assert(!input.closed)
        _ = assert(!output.closed)
        _ = assert(!diagnostics.closed)
        _ <- IO.blocking {
          input.close()
          output.close()
          diagnostics.close()
        }
      } yield ()
    }
  }

}
