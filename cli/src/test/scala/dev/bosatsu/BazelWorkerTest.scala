package dev.bosatsu

import cats.effect.IO
import cats.syntax.all._
import dev.bosatsu.tool.ExitCode
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

class BazelWorkerTest extends FileCommandFixtures {
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

  test("the PathModule worker route rejects malformed startup and parameter files") {
    withRunner { (root, _) =>
      val emptyFile = root.resolve("empty params 雪")
      val emptyOperation = root.resolve("empty-operation.params")
      val invalidUtf8 = root.resolve("invalid-utf8.params")
      for {
        _ <- IO.blocking(Files.writeString(emptyFile, ""))
        _ <- IO.blocking(Files.writeString(emptyOperation, "\nheaders"))
        _ <- writeBytes(root, "invalid-utf8.params", Array(0xc3.toByte, 0x28.toByte))
        results <- List(
          List("tool", "bazel-worker"),
          List("tool", "bazel-worker", "params"),
          List("tool", "bazel-worker", "@"),
          List("tool", "bazel-worker", "@absent.params"),
          List("tool", "bazel-worker", "@one.params", "@two.params"),
          List("tool", "bazel-worker", "--persistent_worker"),
          List("tool", "bazel-worker", s"@$emptyFile"),
          List("tool", "bazel-worker", s"@$emptyOperation"),
          List("tool", "bazel-worker", s"@$invalidUtf8")
        ).traverse(runCli)
        _ = results.foreach { case (exitCode, stderr) =>
          assertEquals(exitCode, ExitCode.Error)
          assert(stderr.nonEmpty)
        }
        _ = assert(!listPaths(root).exists(path =>
          path.endsWith(".json") || path.endsWith(".pkg") || path.endsWith(".sig")
        ))
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

  test("PathModule sends an accepted one-shot operation through the file runner") {
    withRunner { (root, _) =>
      val source = root.resolve("source 雪.bosatsu").toAbsolutePath
      val output = root.resolve("headers 雪.json").toAbsolutePath
      val params = root.resolve("worker.params").toAbsolutePath
      val sourceText = "package Example/Worker\nexport value\nvalue = 1\n"
      for {
        _ <- IO.blocking(Files.writeString(source, sourceText))
        _ <- IO.blocking(Files.writeString(params,
          s"headers\n--src\n$source\n--out\n$output\n"))
        result <- runCliCaptured(List("tool", "bazel-worker", s"@$params"))
        _ = assertEquals(result.exitCode, ExitCode.Success, result.stderr)
        _ = assertEquals(result.stdout, "")
        _ = assertEquals(result.stderr, "")
        written <- IO.blocking(Files.readString(output))
        _ = assert(written.contains("Example/Worker"), written)
      } yield ()
    }
  }

  private def runCli(arguments: List[String]): IO[(ExitCode, String)] =
    IO.defer {
      val buffer = new ByteArrayOutputStream
      val stream = new PrintStream(buffer, false, UTF_8)
      val previous = System.err
      System.setErr(stream)
      PathModule
        .runCli(arguments)
        .guarantee(IO(System.setErr(previous)))
        .map(code => (code, buffer.toString(UTF_8)))
    }

  private def runCliCaptured(arguments: List[String]): IO[FileCommandRunner.Result] =
    IO.defer {
      val out = new ByteArrayOutputStream
      val err = new ByteArrayOutputStream
      val outStream = new PrintStream(out, false, UTF_8)
      val errStream = new PrintStream(err, false, UTF_8)
      val previousOut = System.out
      val previousErr = System.err
      System.setOut(outStream)
      System.setErr(errStream)
      PathModule
        .runCli(arguments)
        .guarantee(IO {
          System.setOut(previousOut)
          System.setErr(previousErr)
        })
        .map(code => FileCommandRunner.Result(code, out.toString(UTF_8), err.toString(UTF_8)))
    }
}
