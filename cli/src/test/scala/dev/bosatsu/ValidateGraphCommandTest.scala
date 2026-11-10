package dev.bosatsu

import cats.effect.IO
import cats.syntax.all._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

class ValidateGraphCommandTest extends FileCommandFixtures {
  private val rootFile = "graph/app.node.json"
  private val depFile = "graph/generated files/葉.node.json"
  private val leafFile = "external/dependency/leaf.node.json"
  private val stamp = "out/validated"

  private def group(label: String, pkg: String, source: String, edges: (String, String)*): String = {
    val deps = edges.toVector.map { case (dep, manifest) =>
      Json.JObject(List("label" -> Json.JString(dep), "manifest" -> Json.JString(manifest)))
    }
    val member = Json.JObject(List("package" -> Json.JString(pkg), "source" -> Json.JString(source)))
    Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "kind" -> Json.JString("group"),
      "label" -> Json.JString(label),
      "members" -> Json.JArray(Vector(member)),
      "deps" -> Json.JArray(deps)
    )).render
  }

  private def validate(runner: FileCommandRunner, root: Path, nodes: List[String], local: Boolean) =
    runner.run(root, List("validate-graph", "--root", rootFile, "--out", stamp) :::
      nodes.flatMap(n => List("--node", n)) ::: Option.when(local)("--local").toList)

  test("local and complete actions work from declared manifests alone") {
    withRunner { (root, runner) =>
      val app = group("//app:g", "App/Main", "//app:Main.bosatsu", "@repo//lib:g" -> depFile)
      val dep = group("@repo//lib:g", "Lib/Api", "@repo//lib:Api.bosatsu", "//leaf:g" -> leafFile)
      for {
        _ <- write(root, rootFile, app)
        _ <- write(root, depFile, dep)
        local <- validate(runner, root, List(depFile), local = true)
        _ = success(local)
        _ = assertEquals(local.stdout, "")
        _ = assertEquals(local.stderr, "")
        text <- read(root, stamp)
        _ = assertEquals(text.trim, "ok")
        _ <- IO.blocking(Files.setLastModifiedTime(root.resolve(stamp), FileTime.fromMillis(1000000000000L)))
        before = Files.getLastModifiedTime(root.resolve(stamp))
        // No source or implementation file exists. The leaf's node file is
        // added only for the complete composition check.
        _ <- write(root, leafFile, group("//leaf:g", "Lib/Leaf", "//leaf:Leaf.bosatsu"))
        full <- validate(runner, root, List(leafFile, depFile, depFile, rootFile), local = false)
        _ = success(full)
        _ = assertEquals(Files.getLastModifiedTime(root.resolve(stamp)), before)
        // A different valid topology also preserves the stamp and its mtime.
        _ <- write(root, depFile, group("@repo//lib:g", "Lib/New", "@repo//lib:New.bosatsu"))
        changed <- validate(runner, root, List(depFile), local = false)
        _ = success(changed)
        _ = assertEquals(Files.getLastModifiedTime(root.resolve(stamp)), before)
      } yield ()
    }
  }

  test("the local action can exclude a direct dependency's outgoing edges from its inputs") {
    withRunner { (root, runner) =>
      val membersFile = "gen/lib.members.json"
      val localRoot = "gen/app.local.json"
      def members(pkg: String) =
        s"""{"schema_version":1,"kind":"group-members","label":"//:lib","members":[{"package":"$pkg","source":"src/Lib.bosatsu"}]}"""
      val localArgs = List("validate-graph", "--local", "--root", localRoot, "--node", membersFile, "--out", stamp)
      for {
        _ <- write(root, localRoot, group("//:app", "App", "src/App.bosatsu", "//:lib" -> membersFile))
        _ <- write(root, rootFile, group("//:app", "App", "src/App.bosatsu", "//:lib" -> depFile))
        _ <- write(root, membersFile, members("Lib"))
        // A sandboxed local action has no access to lib's topology manifest.
        local <- runner.run(root, localArgs)
        _ = success(local)
        _ <- write(root, depFile, group("//:lib", "Lib", "src/Lib.bosatsu", "//:new" -> leafFile))
        _ <- write(root, leafFile, group("//:new", "New", "src/New.bosatsu"))
        full <- validate(runner, root, List(depFile, leafFile), local = false)
        _ = success(full)
        _ <- write(root, depFile, group("//:lib", "Lib", "src/Lib.bosatsu", "//:missing" -> "other.node"))
        badFull <- validate(runner, root, List(depFile), local = false)
        _ = failure(badFull)
        _ = assert(badFull.stderr.contains("//:app -> //:lib -> //:missing"), badFull.stderr)
        sameLocal <- runner.run(root, localArgs)
        _ = success(sameLocal)
        _ <- write(root, membersFile, members("App"))
        collision <- runner.run(root, localArgs)
        _ = failure(collision)
        _ = assert(collision.stderr.contains("duplicate package App"), collision.stderr)
        // A versioned library must never be interpreted in the loose group
        // namespace, even with the same file path, label and member list.
        _ <- write(root, membersFile, members("Lib").replace("group-members", "library"))
        library <- runner.run(root, localArgs)
        _ = failure(library)
        _ = assert(library.stderr.contains("unsupported graph node kind 'library'"), library.stderr)
      } yield ()
    }
  }

  test("a full scan requires topology even when a declared dependency has valid membership") {
    withRunner { (root, runner) =>
      val summary = """{"schema_version":1,"kind":"group-members","label":"//:lib","members":[{"package":"Lib","source":"Lib.bosatsu"}]}"""
      for {
        _ <- write(root, rootFile, group("//:app", "App", "App.bosatsu", "//:lib" -> depFile))
        _ <- write(root, depFile, summary)
        full <- validate(runner, root, List(depFile), local = false)
        _ = failure(full)
        _ = assert(full.stderr.contains("group-members"), full.stderr)
        _ = assert(!Files.exists(root.resolve(stamp)))
        _ <- write(root, rootFile, summary)
        rootOnly <- validate(runner, root, Nil, local = true)
        _ = failure(rootOnly)
        _ = assert(!Files.exists(root.resolve(stamp)))
      } yield ()
    }
  }

  test("a full check names both routes to a conflicting private member") {
    withRunner { (root, runner) =>
      val left = "out/left.json"
      val right = "out/right.json"
      val a = "out/a.json"
      val b = "out/b.json"
      val all = List(b, a, right, left)
      for {
        _ <- write(root, rootFile, group("//:app", "App", "App.bosatsu", "//:left" -> left, "//:right" -> right))
        _ <- write(root, left, group("//:left", "Left", "Left.bosatsu", "//:a" -> a))
        _ <- write(root, right, group("//:right", "Right", "Right.bosatsu", "//:b" -> b))
        _ <- write(root, a, group("//:a", "Util/Private", "first/Util.bosatsu"))
        _ <- write(root, b, group("//:b", "Util/Private", "second/Util.bosatsu"))
        local <- validate(runner, root, List(left, right), local = true)
        _ = success(local)
        original <- read(root, stamp)
        bad <- validate(runner, root, all, local = false)
        _ = failure(bad)
        _ = assert(bad.stderr.contains("//:app -> //:left -> //:a (first/Util.bosatsu, Util/Private)"), bad.stderr)
        _ = assert(bad.stderr.contains("//:app -> //:right -> //:b (second/Util.bosatsu, Util/Private)"), bad.stderr)
        reordered <- validate(runner, root, all.reverse, local = false)
        _ = assertEquals(reordered, bad)
        preserved <- read(root, stamp)
        _ = assertEquals(preserved, original)
        _ <- write(root, b, group("//:b", "Other/Private", "second/Util.bosatsu"))
        recovered <- validate(runner, root, all, local = false)
        _ = success(recovered)
      } yield ()
    }
  }

  test("the schema rejects mistaken node kinds and malformed member or edge identities") {
    withRunner { (root, runner) =>
      val base = """{"schema_version":1,"kind":"group","label":"//:g","members":[{"source":"s","package":"Good"}],"deps":[]}"""
      val invalid = List(
        base.replace("\"schema_version\":1", "\"schema_version\":2") -> "unsupported schema version 2",
        base.replace("\"schema_version\":1,", "") -> "schema_version",
        base.replace("\"group\"", "\"library\"") -> "unsupported graph node kind 'library'",
        base.replace("\"//:g\"", "\"\"") -> "nonempty string",
        base.replace("\"Good\"", "\"bad/name\"") -> "package",
        base.replace("\"s\"", "\"\"") -> "source",
        base.replace("{\"source\":\"s\",\"package\":\"Good\"}", "") -> "at least one group member",
        base.replace("\"source\":\"s\"", "\"source\":\"s\",\"source\":\"other\"") -> "repeated JSON fields: source",
        base.replace("\"schema_version\":1", "\"schema_version\":2,\"schema_version\":1") -> "repeated JSON fields: schema_version",
        base.replace("\"deps\":[]", "\"deps\":[{\"label\":\"//:missing\"}]") -> "manifest",
        base.replace("\"deps\":[]", "\"deps\":[{\"label\":true,\"manifest\":\"path\"}]") -> "label",
        base.dropRight(1) -> "invalid JSON"
      )
      for {
        _ <- write(root, stamp, "prior success")
        _ <- invalid.traverse_ { case (input, reason) =>
          for {
            _ <- write(root, rootFile, input)
            result <- validate(runner, root, Nil, local = false)
            _ = failure(result)
            _ = assert(result.stderr.contains(Path.of(rootFile).toString) && result.stderr.contains(reason), result.stderr)
            previous <- read(root, stamp)
            _ = assertEquals(previous, "prior success")
          } yield ()
        }
        _ <- write(root, rootFile, base)
        ok <- validate(runner, root, Nil, local = false)
        _ = success(ok)
      } yield ()
    }
  }

  test("a request cannot discover new node inputs or read surplus undeclared subtrees") {
    withRunner { (root, runner) =>
      val app = group("//:app", "App", "App.bosatsu", "//:dep" -> depFile)
      for {
        _ <- write(root, rootFile, app)
        _ <- write(root, depFile, "deliberately invalid JSON")
        undeclared <- validate(runner, root, Nil, local = false)
        _ = failure(undeclared)
        _ = assert(undeclared.stderr.contains("missing declared --node"), undeclared.stderr)
        _ = assert(undeclared.stderr.contains("//:app -> //:dep"), undeclared.stderr)
        declared <- validate(runner, root, List(depFile), local = false)
        _ = failure(declared)
        _ = assert(declared.stderr.contains("invalid JSON") && declared.stderr.contains(Path.of(depFile).toString), declared.stderr)
        _ <- write(root, depFile, group("//:dep", "Dep", "Dep.bosatsu"))
        extra <- validate(runner, root, List(depFile, "unreadable-extra"), local = false)
        _ = failure(extra)
        _ = assert(extra.stderr.contains("unreachable node manifests: unreadable-extra"), extra.stderr)
        _ = assert(!Files.exists(root.resolve(stamp)))
      } yield ()
    }
  }

  test("ill-formed UTF-8 cannot turn a different group label into the expected label") {
    withRunner { (root, runner) =>
      val label = "//:lib\ufffd"
      val marker = group("//:lib~", "Lib", "src/Lib.bosatsu")
      val corrupt = marker.getBytes(StandardCharsets.UTF_8)
      corrupt(marker.indexOf('~')) = 0xff.toByte
      for {
        _ <- write(root, rootFile, group("//:app", "App", "src/App.bosatsu", label -> depFile))
        _ <- write(root, depFile, group(label, "Lib", "src/Lib.bosatsu"))
        valid <- validate(runner, root, List(depFile), local = false)
        _ = success(valid)
        _ <- IO.blocking(Files.write(root.resolve(depFile), corrupt))
        _ <- write(root, stamp, "previous")
        invalid <- validate(runner, root, List(depFile), local = false)
        _ = failure(invalid)
        _ = assert(invalid.stderr.contains(Path.of(depFile).toString) && invalid.stderr.contains("UTF-8"), invalid.stderr)
        preserved <- read(root, stamp)
        _ = assertEquals(preserved, "previous")
      } yield ()
    }
  }
}
