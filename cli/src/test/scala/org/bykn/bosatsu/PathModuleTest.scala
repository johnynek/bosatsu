package org.bykn.bosatsu

import cats.data.NonEmptyList
import java.nio.file.{Path, Paths}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks.forAll
import org.scalatest.FunSuite

import scala.collection.JavaConverters._

class PathModuleTest extends FunSuite {

  implicit val arbPath: Arbitrary[Path] =
    Arbitrary {
      val str = Gen.identifier

      Gen.listOf(str).map { parts => Paths.get(parts.mkString("/")) }
    }

  test("test some hand written examples") {
    def pn(roots: List[String], file: String): Option[PackageName] =
      PathModule.pathPackage(roots.map(Paths.get(_)), Paths.get(file))

    assert(pn(List("/root0", "/root1"), "/root0/Bar.bosatsu") == Some(PackageName(NonEmptyList.of("Bar"))))
    assert(pn(List("/root0", "/root1"), "/root1/Bar/Baz.bosatsu") == Some(PackageName(NonEmptyList.of("Bar", "Baz"))))
    assert(pn(List("/root0", "/root0/Bar"), "/root0/Bar/Baz.bosatsu") == Some(PackageName(NonEmptyList.of("Bar", "Baz"))))
    assert(pn(List("/root0/", "/root0/Bar"), "/root0/Bar/Baz.bosatsu") == Some(PackageName(NonEmptyList.of("Bar", "Baz"))))
    assert(pn(List("/root0/ext", "/root0/Bar"), "/root0/ext/Bar/Baz.bosatsu") == Some(PackageName(NonEmptyList.of("Bar", "Baz"))))
  }

  test("no roots means no Package") {
    forAll { p: Path =>
      assert(PathModule.pathPackage(Nil, p) == None)
    }
  }

  test("empty path is not okay for a package") {
    forAll { roots: List[Path] =>
      assert(PathModule.pathPackage(roots, Paths.get("")) == None)
    }
  }

  test("if we add to a path that becomes Package") {
    def law(root: Path, otherRoots: List[Path], rest: Path) = {
      if (rest.toString != "" && root.toString != "") {
        val path = root.resolve(rest)
        val pack =
          PackageName.parse(rest.asScala.map(_.toString.toLowerCase.capitalize).mkString("/"))
        assert(PathModule.pathPackage(root :: otherRoots, path) == pack)
      }
    }

    forAll(law(_, _, _))
    // some regressions:
    val regressions: List[(Path, List[Path], Path)] =
      List(
        (Paths.get(""), Nil, Paths.get("/foo/bar")),
        (Paths.get(""), List(Paths.get("")), Paths.get("/foo/bar")))

    regressions.foreach { case (r, o, e) => law(r, o, e) }
  }

  test("if none of the roots are prefixes we have none") {
    forAll { (r0: Path, roots0: List[Path], file: Path) =>
      val roots = (r0 :: roots0).filterNot(_.toString == "")
      val pack = PathModule.pathPackage(roots, file)

      val noPrefix = !roots.exists { r => file.asScala.toList.startsWith(r.asScala.toList) }

      if (noPrefix) assert(pack == None)
    }
  }

  def run(args: String*): PathModule.Output =
    PathModule.run(args.toList) match {
      case Left(_) => fail(s"got help on command: ${args.toList}")
      case Right(io) =>
        val output = io.unsafeRunSync()
        // This is a cheat, but at least we call the code so
        // we see it doesn't crash or infinite loop or something
        PathModule.reportOutput(output)
        output
    }

  test("test direct run of a file") {
    val out = run("test --input test_workspace/List.bosatsu --input test_workspace/Nat.bosatsu --input test_workspace/Bool.bosatsu --test_file test_workspace/Queue.bosatsu".split("\\s+"): _*)
    out match {
      case PathModule.Output.TestOutput(results, _) =>
        val res = results.collect { case (pn, Some(t)) if pn.asString == "Queue" => t }
        assert(res.length == 1)
      case other => fail(s"expected test output: $other")
    }
  }

  test("test search run of a file") {
    val out = run("test --package_root test_workspace --search --test_file test_workspace/Bar.bosatsu".split("\\s+"): _*)
    out match {
      case PathModule.Output.TestOutput(results, _) =>
        val res = results.collect { case (pn, Some(t)) if pn.asString == "Bar" => t }
        assert(res.length == 1)
        assert(res.head.assertions == 1)
        assert(res.head.failureCount == 0)
      case other => fail(s"expected test output: $other")
    }
  }

  test("test search with json write") {

    val out = run("json write --package_root test_workspace --search --main_file test_workspace/Bar.bosatsu".split("\\s+"): _*)
    out match {
      case PathModule.Output.JsonOutput(j@Json.JObject(_), _) =>
        assert(j.toMap == Map("value" -> Json.JBool(true), "message" -> Json.JString("got the right string")))
        assert(j.items.length == 2)
      case other => fail(s"expected json object output: $other")
    }
  }

  test("test search json apply") {
    val cmd = "json apply --input_dir test_workspace/ --package_root test_workspace/ --value Bosatsu/Nat::mult --json_string"
      .split("\\s+").toList :+ "[2, 4]"

    run(cmd: _*) match {
      case PathModule.Output.JsonOutput(Json.JNumberStr("8"), _) => succeed
      case other => fail(s"expected json object output: $other")
    }
  }

  test("test running all test in test_workspace") {

    val out = run("test --package_root test_workspace --input_dir test_workspace".split("\\s+"): _*)
    out match {
      case PathModule.Output.TestOutput(res, _) =>
        val noTests = res.collect { case (pn, None) => pn }.toList
        assert(noTests == Nil)
        val failures = res.collect { case (pn, Some(t)) if t.failureCount > 0 => pn }
        assert(failures == Nil)
      case other => fail(s"expected test output: $other")
    }
  }

}
