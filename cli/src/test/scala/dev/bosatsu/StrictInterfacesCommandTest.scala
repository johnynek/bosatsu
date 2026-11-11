package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.effect.IO
import cats.syntax.all._
import java.nio.file.{Files, Path}

class StrictInterfacesCommandTest extends FileCommandFixtures {
  private val leaf =
    "package Leaf\nexport Leaf, leaf\nstruct Leaf(n: Int)\nleaf = Leaf(1)\n"
  private val y =
    """package Y
      |from Leaf import Leaf, leaf
      |export Token(), token
      |exposes Leaf
      |struct Token(value: Leaf)
      |token = Token(leaf)
      |""".stripMargin
  private val privateSource = "package Private\nexport enabled\nenabled = True\n"
  private val sibling = "package Sibling\nexport number\nnumber = 7\n"
  private val x =
    """package X
      |from Private import enabled
      |from Y import token
      |export shared
      |exposes Y
      |shared = if enabled: token
      |         else: token
      |""".stripMargin

  private def args(out: String, srcs: List[String], deps: List[String]): List[String] =
    List("compile-packages", "--packages-out", s"$out.pkg", "--sig-out", s"$out.sig") :::
      srcs.flatMap(s => List("--src", s"$s.bosatsu")) :::
      deps.flatMap(p => List("--direct-interface", p))

  private def build(
      runner: FileCommandRunner, root: Path, out: String, sources: List[String], deps: List[String]
  ): IO[Unit] =
    runner.run(root, args(out, sources, deps)).map(success)

  private def fixture(runner: FileCommandRunner, root: Path): IO[Unit] =
    for {
      _ <- write(root, "Leaf.bosatsu", leaf)
      _ <- write(root, "Y.bosatsu", y)
      _ <- write(root, "Private.bosatsu", privateSource)
      _ <- write(root, "Sibling.bosatsu", sibling)
      _ <- write(root, "X.bosatsu", x)
      _ <- build(runner, root, "leaf", List("Leaf"), Nil)
      _ <- build(runner, root, "y", List("Y"), List("leaf.sig"))
      _ <- build(runner, root, "private", List("Private"), Nil)
      _ <- build(runner, root, "wide", List("X", "Sibling"), List("y.sig", "leaf.sig", "private.sig"))
    } yield ()

  private def labels(root: Path, entries: List[(String, String)]): IO[Unit] = {
    val rows = entries.map { case (path, label) =>
      Json.JObject(List("path" -> Json.JString(path), "label" -> Json.JString(label)))
    }
    val manifest = Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"), "interfaces" -> Json.JArray(rows.toVector)
    ))
    write(root, "labels.json", manifest.render)
  }

  private val strict = List("--error-unused-interfaces")
  private val labeled = List("--interface-labels", "labels.json")

  test("only selected roots of a wide direct interface contribute exposure dependencies") {
    withRunner { (root, runner) =>
      def run(deps: String*) =
        runner.run(root, args("client", List("Client"), deps.toList) ::: strict)
      for {
        _ <- fixture(runner, root)
        _ <- write(root, "Client.bosatsu", "package Client\nfrom Sibling import number\nmain = number\n")
        fromSibling <- run("wide.sig")
        _ = success(fromSibling)
        unused <- run("wide.sig", "y.sig")
        _ = failure(unused)
        _ = assert(unused.stderr.contains("unused direct interfaces: y.sig"), unused.stderr)
        _ <- write(root, "Client.bosatsu", "package Client\nfrom X import shared\nmain = [shared]\n")
        missing <- run("wide.sig")
        _ = failure(missing)
        _ = assert(missing.stderr.contains("Client -> X -> Y"), missing.stderr)
        nested <- run("wide.sig", "y.sig")
        _ = failure(nested)
        _ = assert(nested.stderr.contains("Client -> X -> Y -> Leaf"), nested.stderr)
        exact <- run("leaf.sig", "y.sig", "wide.sig", "wide.sig")
        _ = success(exact)
        privateEdge <- run("wide.sig", "leaf.sig", "private.sig", "y.sig")
        _ = failure(privateEdge)
        _ = assert(privateEdge.stderr.contains("unused direct interfaces: private.sig"), privateEdge.stderr)
      } yield ()
    }
  }

  test("unused files retain label attribution, even when labels repeat or files export no roots") {
    withRunner { (root, runner) =>
      val unicode = "generated/empty snow 雪.sig"
      val deps = List(unicode, "api.sig", "also-empty.sig")
      val argv = args("app", List("Client"), deps) ::: strict ::: labeled
      for {
        _ <- write(root, "Sibling.bosatsu", sibling)
        _ <- build(runner, root, "api", List("Sibling"), Nil)
        _ <- write(root, "Client.bosatsu", "package Client\nfrom Sibling import number\nmain = number\n")
        _ <- IO.blocking(Files.createDirectories(root.resolve("generated")))
        _ <- List(unicode, "also-empty.sig").traverse_ { path =>
          IOPlatformIO.writeInterfaceBundle(proto.Interfaces(), root.resolve(path))
        }
        _ <- labels(root, List("api.sig" -> "//group:api", unicode -> "//copy:both", "also-empty.sig" -> "//copy:both"))
        rejected <- runner.run(root, argv)
        _ = failure(rejected)
        _ = assert(rejected.stderr.contains("also-empty.sig (//copy:both)"), rejected.stderr)
        _ = assert(rejected.stderr.contains(s"${Path.of(unicode)} (//copy:both)"), rejected.stderr)
        _ = assert(!rejected.stderr.contains("//group:api"), rejected.stderr)
        permissive <- runner.run(root, args("app", List("Client"), deps) ::: labeled)
        _ = success(permissive)
      } yield ()
    }
  }

  test("declaring labels changes diagnostics and preserves both successful output files byte-for-byte") {
    withRunner { (root, runner) =>
      val srcs = List("Client")
      val paths = List("wide.sig", "y.sig", "leaf.sig")
      val argv = args("result", srcs, paths) ::: strict ::: labeled
      for {
        _ <- fixture(runner, root)
        _ <- write(root, "Client.bosatsu", "package Client\nfrom X import shared\nmain = [shared]\n")
        _ <- labels(root, paths.map(p => p -> s"//first:$p"))
        first <- runner.run(root, argv)
        _ = success(first)
        impl = Files.readAllBytes(root.resolve("result.pkg")).toList
        api = Files.readAllBytes(root.resolve("result.sig")).toList
        _ <- labels(root, paths.reverse.map(p => p -> s"@second//:$p"))
        second <- runner.run(root, args("result", srcs, paths.reverse) ::: labeled ::: strict)
        _ = success(second)
        _ = assertEquals(Files.readAllBytes(root.resolve("result.pkg")).toList, impl)
        _ = assertEquals(Files.readAllBytes(root.resolve("result.sig")).toList, api)
        _ <- labels(root, List("wide.sig" -> "//app:facade", "y.sig" -> "//types:owner"))
        missing <- runner.run(root, args("result", srcs, paths.init) ::: strict ::: labeled)
        _ = failure(missing)
        _ = assert(missing.stderr.contains("Client -> X -> Y -> Leaf"), missing.stderr)
        _ = assert(missing.stderr.contains("wide.sig (//app:facade)"), missing.stderr)
        _ = assert(missing.stderr.contains("y.sig (//types:owner)"), missing.stderr)
        _ = assertEquals(Files.readAllBytes(root.resolve("result.pkg")).toList, impl)
        _ = assertEquals(Files.readAllBytes(root.resolve("result.sig")).toList, api)
      } yield ()
    }
  }

  test("a group-local owner satisfies an external exposure and cannot consume its own signature") {
    withRunner { (root, runner) =>
      for {
        _ <- fixture(runner, root)
        _ <- write(root, "Client.bosatsu", "package Client\nfrom X import shared\nmain = [shared]\n")
        srcs = List("Client", "Y")
        exact <- runner.run(root, args("mixed", srcs, List("wide.sig", "leaf.sig")) ::: strict)
        _ = success(exact)
        shadowed <- runner.run(root, args("mixed", srcs, List("wide.sig", "leaf.sig", "y.sig")) ::: strict)
        _ = failure(shadowed)
        _ = assert(shadowed.stderr.contains("unused direct interfaces: y.sig"), shadowed.stderr)
      } yield ()
    }
  }

  test("an unrelated member can use a second file, and legacy exposed-only files count as used") {
    withRunner { (root, runner) =>
      for {
        _ <- fixture(runner, root)
        // Produce unindexed signatures through the legacy writer.
        legacy <- runner.run(root, List("check", "--input", "X.bosatsu", "--interface", "private.sig",
          "--interface", "y.sig", "--interface", "leaf.sig", "--interface_out", "old.sig"))
        _ = success(legacy)
        _ <- write(root, "Client.bosatsu", "package Client\nfrom X import shared\nmain = [shared]\n")
        _ <- write(root, "Other.bosatsu", "package Other\nfrom Private import enabled\nmain = enabled\n")
        argv = args("group", List("Client", "Other"), List("old.sig", "y.sig", "leaf.sig", "private.sig"))
        result <- runner.run(root, argv ::: strict)
        _ = success(result)
      } yield ()
    }
  }

  test("an implicit Predef cannot justify a declared file and ambiguity names both providers") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Client.bosatsu", "package Client\nmain = 1\n")
        predef <- IO.fromTry(PackageSignatures.encode(List(PackageSignatures.predef)))
        _ <- IOPlatformIO.writeInterfaceBundle(predef, root.resolve("predef.sig"))
        extra <- runner.run(root, args("client", List("Client"), List("predef.sig")) ::: strict)
        _ = failure(extra)
        _ = assert(extra.stderr.contains("unused direct interfaces: predef.sig"), extra.stderr)
        _ <- write(root, "Sibling.bosatsu", sibling)
        _ <- build(runner, root, "one", List("Sibling"), Nil)
        _ <- IO.blocking(Files.copy(root.resolve("one.sig"), root.resolve("two.sig")))
        _ <- write(root, "Client.bosatsu", "package Client\nfrom Sibling import number\nmain = number\n")
        _ <- labels(root, List("two.sig" -> "@second//:dep", "one.sig" -> "@first//:dep"))
        ambiguous <- runner.run(root, args("client", List("Client"), List("two.sig", "one.sig")) ::: strict ::: labeled)
        _ = failure(ambiguous)
        _ = assert(ambiguous.stderr.contains("Client -> Sibling"), ambiguous.stderr)
        _ = assert(ambiguous.stderr.contains("one.sig (@first//:dep), two.sig (@second//:dep)"), ambiguous.stderr)
      } yield ()
    }
  }

  test("two privately built signatures become ambiguous when both wrappers expose Shared/Api") {
    withRunner { (root, runner) =>
      def provider(field: String, value: String) =
        s"package Shared/Api\nexport Token(), token\nstruct Token(value: $field)\ntoken = Token($value)\n"
      def facade(name: String) =
        s"package $name\nfrom Shared/Api import token\nexport value\nexposes Shared/Api\nvalue = token\n"
      val both = List("left.sig", "right.sig", "one.sig", "two.sig")
      val app = "package App\nfrom Left import value as x\nfrom Right import value as y\nmain = (x, y)\n"
      for {
        _ <- write(root, "One.bosatsu", provider("Int", "1"))
        _ <- write(root, "Two.bosatsu", provider("Bool", "True"))
        _ <- build(runner, root, "one", List("One"), Nil)
        _ <- build(runner, root, "two", List("Two"), Nil)
        _ <- write(root, "Left.bosatsu", facade("Left"))
        _ <- write(root, "Right.bosatsu", facade("Right"))
        _ <- build(runner, root, "left", List("Left"), List("one.sig"))
        _ <- build(runner, root, "right", List("Right"), List("two.sig"))
        _ <- write(root, "App.bosatsu", "package App\nfrom Left import value\nmain = [value]\n")
        ok <- runner.run(root, args("app", List("App"), List("one.sig", "left.sig")) ::: strict)
        _ = success(ok)
        outputs = List("app.pkg", "app.sig").map(p => Files.readAllBytes(root.resolve(p)).toList)
        _ <- write(root, "App.bosatsu", app)
        _ <- labels(root, List("left.sig" -> "//:left", "right.sig" -> "//:right",
          "one.sig" -> "@shared_v1//:lib", "two.sig" -> "@shared_v2//:lib"))
        _ <- List(both, both.reverse).traverse_ { paths =>
          runner.run(root, args("app", List("App"), paths) ::: labeled ::: strict).map { result =>
            failure(result)
            assert(result.stderr.contains("ambiguous interface App -> "), result.stderr)
            assert(result.stderr.contains(" -> Shared/Api"), result.stderr)
            assert(result.stderr.contains("one.sig (@shared_v1//:lib), two.sig (@shared_v2//:lib)"), result.stderr)
            assertEquals(List("app.pkg", "app.sig").map(p => Files.readAllBytes(root.resolve(p)).toList), outputs)
          }
        }
      } yield ()
    }
  }

  test("duplicate sources cannot make a required direct interface look unused") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Sibling.bosatsu", sibling)
        _ <- build(runner, root, "api", List("Sibling"), Nil)
        _ <- write(root, "A.bosatsu", "package App\nfrom Sibling import number\nmain = number\n")
        _ <- write(root, "B.bosatsu", "package App\nmain = 42\n")
        result <- runner.run(root, args("build", List("A", "B"), List("api.sig")) ::: strict)
        _ = failure(result)
        _ = assert(result.stderr.contains("package App duplicated in A.bosatsu, B.bosatsu"), result.stderr)
        _ = assert(!result.stderr.contains("unused direct interfaces"), result.stderr)
      } yield ()
    }
  }

  test("a reused runner recomputes strict deps when the same signature path loses an exposure") {
    withRunner { (root, runner) =>
      val deps = List("wide.sig", "y.sig", "leaf.sig")
      val argv = args("result", List("Client"), deps) ::: strict
      for {
        _ <- fixture(runner, root)
        _ <- write(root, "Client.bosatsu", "package Client\nfrom X import shared\nmain = [shared]\n")
        valid <- runner.run(root, argv)
        _ = success(valid)
        before = Files.readAllBytes(root.resolve("result.pkg")).toList
        _ <- write(root, "X.bosatsu", "package X\nexport shared\nshared = 1\n")
        _ <- build(runner, root, "wide", List("X", "Sibling"), Nil)
        warm <- runner.run(root, argv)
        _ = failure(warm)
        _ = assert(warm.stderr.contains("unused direct interfaces: leaf.sig, y.sig"), warm.stderr)
        cold <- FileCommandRunner.resource.use(_.run(root, argv))
        _ = assertEquals(warm, cold)
        _ = assertEquals(Files.readAllBytes(root.resolve("result.pkg")).toList, before)
        recovered <- runner.run(root, args("result", List("Client"), List("wide.sig")) ::: strict)
        _ = success(recovered)
      } yield ()
    }
  }

  test("a supplied mapping must exactly describe the command line declarations") {
    withRunner { (root, runner) =>
      val argv = args("client", List("Client"), List("g.sig")) ::: labeled
      for {
        _ <- write(root, "Client.bosatsu", "package Client\nmain = 1\n")
        _ <- IOPlatformIO.writeInterfaceBundle(proto.Interfaces(), root.resolve("g.sig"))
        _ <- List(
          (List.empty[(String, String)], "missing labels for: g.sig"),
          (List("undeclared.sig" -> "//g:unknown"), "undeclared interface undeclared.sig"),
          (List("g.sig" -> "//a:g", "g.sig" -> "//b:g"), "duplicate entry for interface g.sig"),
          (List("g.sig" -> ""), "expected a nonempty string")
        ).traverse_ { case (entries, reason) =>
          for {
            _ <- labels(root, entries)
            result <- runner.run(root, argv)
            _ = failure(result)
            _ = assert(result.stderr.contains("labels.json") && result.stderr.contains(reason), result.stderr)
          } yield ()
        }
        _ <- labels(root, List("g.sig" -> "//real:g"))
        recovered <- runner.run(root, argv)
        _ = success(recovered)
      } yield ()
    }
  }
}
