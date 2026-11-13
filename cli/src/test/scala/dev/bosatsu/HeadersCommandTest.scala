package dev.bosatsu

import cats.effect.IO
import cats.syntax.all._
import java.nio.file.{Files, Path}

class HeadersCommandTest extends FileCommandFixtures {
  private val output = "generated/headers.json"

  private def scan(
      runner: FileCommandRunner,
      root: Path,
      paths: String*
  ): IO[FileCommandRunner.Result] =
    runner.run(
      root,
      List("headers", "--out", output) ++ paths.flatMap(p => List("--src", p))
    )

  private def parsed(text: String): Json =
    Json.parserFile.parseAll(text).fold(e => fail(e.toString), identity)

  private val apiSource =
    """package Example/Api
      |
      |from Example/Zed import value as zed
      |from Example/Types import (Token, make as token)
      |export make_token
      |exposes Example/Types
      |
      |make_token = token(zed)
      |""".stripMargin

  test(
    "versioned JSON retains package, original and local import names, and exposures"
  ) {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Api.bosatsu", apiSource)
        result <- scan(runner, root, "Api.bosatsu")
        _ = success(result)
        _ = assertEquals(result.stdout, "")
        _ = assertEquals(result.stderr, "")
        json <- read(root, output)
        expected = parsed(
          """{
            |  "schema_version": 1,
            |  "packages": [{
            |    "path": "Api.bosatsu",
            |    "package": "Example/Api",
            |    "imports": [
            |      {"package": "Example/Types", "names": [
            |        {"original": "Token", "local": "Token"},
            |        {"original": "make", "local": "token"}
            |      ]},
            |      {"package": "Example/Zed", "names": [
            |        {"original": "value", "local": "zed"}
            |      ]}
            |    ],
            |    "exposes": ["Example/Types"]
            |  }]
            |}""".stripMargin
        )
        _ = assert(Json.eqJson.eqv(parsed(json), expected), json)
      } yield ()
    }
  }

  test(
    "source order, body edits, comments and duplicate src arguments preserve exact bytes"
  ) {
    withRunner { (root, runner) =>
      val a = "src/A.bosatsu"
      val b = "src/B.bosatsu"
      val firstBody = "private_value = 42"
      val prefix = "package Example/B\n\nexposes ()\n\n"
      for {
        _ <- write(root, a, apiSource)
        _ <- write(root, b, prefix + firstBody)
        fst <- scan(runner, root, b, a)
        _ = success(fst)
        before <- read(root, output)
        _ <- write(
          root,
          b,
          prefix + "# parser need not understand the body\nprivate_value = ("
        )
        snd <- scan(runner, root, a, b, a)
        _ = success(snd)
        after <- read(root, output)
        _ = assertEquals(after, before)
        _ = assert(after.indexOf("Example/Api") < after.indexOf("Example/B"))
        _ = assert(!after.contains(firstBody))
        _ = assert(!after.contains(root.toString))
      } yield ()
    }
  }

  test("combine split import clauses and deduplicate exposure names") {
    withRunner { (root, runner) =>
      val src = "Module.bosatsu"
      val one = """package Example/Module
                  |
                  |from Other import value as x
                  |from Other import value as y
                  |exposes (Zed, Other, Other)
                  |
                  |test = incomplete
                  |""".stripMargin
      val two = """package Example/Module
                  |
                  |from Other import (value as y, value as x)
                  |exposes (Other, Zed)
                  |
                  |other = 1
                  |""".stripMargin
      for {
        _ <- write(root, src, one)
        result1 <- scan(runner, root, src)
        _ = success(result1)
        before <- read(root, output)
        _ <- write(root, src, two)
        result2 <- scan(runner, root, src)
        _ = success(result2)
        after <- read(root, output)
        _ = assertEquals(after, before)
      } yield ()
    }
  }

  test(
    "paths are JSON escaped; same package in distinct source files stays distinguishable"
  ) {
    withRunner { (root, runner) =>
      val a =
        if (java.io.File.separatorChar == '/') "src/quoted \"path\".bosatsu"
        else "src/space in path.bosatsu"
      val b = "generated/雪.bosatsu"
      val src = "package Example/Duplicate\n\nvalue = 0\n"
      for {
        _ <- write(root, a, src)
        _ <- write(root, b, src)
        result <- scan(runner, root, a, b)
        _ = success(result)
        text <- read(root, output)
        rows = parsed(text) match {
          case obj: Json.JObject =>
            obj.getOrNull("packages") match {
              case Json.JArray(items) => items
              case other => fail(s"expected package entries, got $other")
            }
          case other => fail(s"expected header index, got $other")
        }
        names = rows.collect { case o: Json.JObject =>
          (o.getOrNull("path"), o.getOrNull("package"))
        }
        _ = assertEquals(
          names,
          Vector(
            (
              Json.JString(Path.of(b).toString),
              Json.JString("Example/Duplicate")
            ),
            (
              Json.JString(Path.of(a).toString),
              Json.JString("Example/Duplicate")
            )
          )
        )
      } yield ()
    }
  }

  test(
    "report every unreadable file and malformed header without replacing an existing output"
  ) {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, output, "last successful index")
        _ <- write(root, "A.bosatsu", "package invalid\n")
        _ <- write(
          root,
          "B.bosatsu",
          "package Example/B\nfrom Example/A import (\n"
        )
        result <- scan(
          runner,
          root,
          "missing.bosatsu",
          "B.bosatsu",
          "A.bosatsu"
        )
        _ = failure(result)
        _ = assert(result.stderr.contains("A.bosatsu:1:"), result.stderr)
        _ = assert(result.stderr.contains("B.bosatsu:"), result.stderr)
        _ = assert(
          result.stderr.contains("file not found: missing.bosatsu"),
          result.stderr
        )
        _ = assert(
          result.stderr.indexOf("A.bosatsu") < result.stderr.indexOf(
            "B.bosatsu"
          )
        )
        retained <- read(root, output)
        _ = assertEquals(retained, "last successful index")
      } yield ()
    }
  }

  test("a misplaced import cannot disappear from a generated dependency graph") {
    withRunner { (root, runner) =>
      val outOfOrder =
        """package Example/API
          |export run
          |
          |from Example/Impl import work
          |
          |run = work
          |""".stripMargin
      for {
        _ <- write(root, "Api.bosatsu", outOfOrder)
        result <- scan(runner, root, "Api.bosatsu")
        _ = failure(result)
        _ = assert(result.stderr.contains("Api.bosatsu:"), result.stderr)
        _ = assert(!Files.exists(root.resolve(output)))
      } yield ()
    }
  }

  test("incomplete headers fail without replacing the last successful index") {
    withRunner { (root, runner) =>
      val src = "Api.bosatsu"
      val badHeaders = List(
        "from",
        "# section break\n\nfrom",
        "export run\nfrom Example/Ok import valok\nfrom Example/Broken import (\nrun = valok",
        "exposes ()\nexport",
        "exposes ()\npackage"
      )
      for {
        _ <- write(root, src, apiSource)
        good <- scan(runner, root, src)
        _ = success(good)
        previous <- read(root, output)
        _ <- badHeaders.traverse_ { header =>
          for {
            _ <- write(root, src, s"package Example/Api\n$header\n")
            invalid <- scan(runner, root, src)
            _ = failure(invalid)
            _ = assert(invalid.stderr.contains("Api.bosatsu:"), invalid.stderr)
            retained <- read(root, output)
            _ = assertEquals(retained, previous)
          } yield ()
        }
      } yield ()
    }
  }

  test("header keywords and their prefixes can start legal body bindings") {
    withRunner { (root, runner) =>
      List(
        "fromage = 1",
        "exported = 1",
        "package = 1",
        "export: Int = 1",
        "from = 1"
      )
        .traverse_ { binding =>
          val source = s"package Example/Api\nexposes ()\n# body\n$binding\n"
          assert(Package.parser.parseAll(source).isRight, source)
          write(root, "Api.bosatsu", source) *>
            scan(runner, root, "Api.bosatsu").map(success)
        }
    }
  }

  test("both --src and --out are required") {
    withRunner { (root, runner) =>
      for {
        noSrc <- runner.run(root, List("headers", "--out", output))
        _ = failure(noSrc)
        _ = assert(noSrc.stderr.contains("--src"), noSrc.stderr)
        noOut <- runner.run(root, List("headers", "--src", "A.bosatsu"))
        _ = failure(noOut)
        _ = assert(noOut.stderr.contains("--out"), noOut.stderr)
        _ = assert(!Files.exists(root.resolve(output)))
      } yield ()
    }
  }
}
