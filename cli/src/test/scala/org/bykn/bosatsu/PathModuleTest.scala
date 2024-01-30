package org.bykn.bosatsu

import cats.effect.IO
import cats.data.NonEmptyList
import java.nio.file.{Path, Paths}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import org.scalatest.funsuite.AnyFunSuite

// allow us to unsafeRunSync
import cats.effect.unsafe.implicits.global

class PathModuleTest extends AnyFunSuite {

  implicit val arbPath: Arbitrary[Path] =
    Arbitrary {
      val str = Gen.identifier

      Gen.listOf(str).map { parts => Paths.get(parts.mkString("/")) }
    }

  test("test some hand written examples") {
    def pn(roots: List[String], file: String): Option[PackageName] =
      PathModule.pathPackage(roots.map(Paths.get(_)), Paths.get(file))

    assert(
      pn(List("/root0", "/root1"), "/root0/Bar.bosatsu") == Some(
        PackageName(NonEmptyList.of("Bar"))
      )
    )
    assert(
      pn(List("/root0", "/root1"), "/root1/Bar/Baz.bosatsu") == Some(
        PackageName(NonEmptyList.of("Bar", "Baz"))
      )
    )
    assert(
      pn(List("/root0", "/root0/Bar"), "/root0/Bar/Baz.bosatsu") == Some(
        PackageName(NonEmptyList.of("Bar", "Baz"))
      )
    )
    assert(
      pn(List("/root0/", "/root0/Bar"), "/root0/Bar/Baz.bosatsu") == Some(
        PackageName(NonEmptyList.of("Bar", "Baz"))
      )
    )
    assert(
      pn(
        List("/root0/ext", "/root0/Bar"),
        "/root0/ext/Bar/Baz.bosatsu"
      ) == Some(PackageName(NonEmptyList.of("Bar", "Baz")))
    )
  }

  test("no roots means no Package") {
    forAll { (p: Path) =>
      assert(PathModule.pathPackage(Nil, p) == None)
    }
  }

  test("empty path is not okay for a package") {
    forAll { (roots: List[Path]) =>
      assert(PathModule.pathPackage(roots, Paths.get("")) == None)
    }
  }

  test("if we add to a path that becomes Package") {
    def law(root: Path, otherRoots: List[Path], rest: Path) = {
      if (rest.toString != "" && root.toString != "") {
        val path = root.resolve(rest)
        val pack =
          PackageName.parse(
            rest.asScala.map(_.toString.toLowerCase.capitalize).mkString("/")
          )
        assert(PathModule.pathPackage(root :: otherRoots, path) == pack)
      }
    }

    forAll(law(_, _, _))
    // some regressions:
    val regressions: List[(Path, List[Path], Path)] =
      List(
        (Paths.get(""), Nil, Paths.get("/foo/bar")),
        (Paths.get(""), List(Paths.get("")), Paths.get("/foo/bar"))
      )

    regressions.foreach { case (r, o, e) => law(r, o, e) }
  }

  test("if none of the roots are prefixes we have none") {
    forAll { (r0: Path, roots0: List[Path], file: Path) =>
      val roots = (r0 :: roots0).filterNot(_.toString == "")
      val pack = PathModule.pathPackage(roots, file)

      val noPrefix = !roots.exists { r =>
        file.asScala.toList.startsWith(r.asScala.toList)
      }

      if (noPrefix) assert(pack == None)
    }
  }

  def run(args: String*): PathModule.Output =
    PathModule.run(args.toList) match {
      case Left(h) => fail(s"got help: $h on command: ${args.toList}")
      case Right(io) =>
        io.attempt
          .flatMap {
            case Right(out) =>
              PathModule.reportOutput(out).as(out)
            case Left(err) =>
              PathModule.reportException(err) *> IO.raiseError(err)
          }
          .unsafeRunSync()
    }

  test("test direct run of a file") {
    val out = run(
      "test --input test_workspace/List.bosatsu --input test_workspace/Nat.bosatsu --input test_workspace/Bool.bosatsu --test_file test_workspace/Queue.bosatsu"
        .split("\\s+")
        .toSeq: _*
    )
    out match {
      case PathModule.Output.TestOutput(results, _) =>
        val res = results.collect {
          case (pn, Some(t)) if pn.asString == "Queue" => t.value
        }
        assert(res.length == 1)
      case other => fail(s"expected test output: $other")
    }
  }

  test("test search run of a file") {
    val out = run(
      "test --package_root test_workspace --search --test_file test_workspace/Bar.bosatsu"
        .split("\\s+")
        .toSeq: _*
    )
    out match {
      case PathModule.Output.TestOutput(results, _) =>
        val res = results.collect {
          case (pn, Some(t)) if pn.asString == "Bar" => t.value
        }
        assert(res.length == 1)
        assert(res.head.assertions == 1)
        assert(res.head.failureCount == 0)
      case other => fail(s"expected test output: $other")
    }
  }

  test("test python transpile on the entire test_workspace") {
    val out = run(
      "transpile --input_dir test_workspace/ --outdir pyout --lang python --package_root test_workspace"
        .split("\\s+")
        .toSeq: _*
    )
    out match {
      case PathModule.Output.TranspileOut(_, _) =>
        assert(true)
      case other => fail(s"expected transpile output: $other")
    }
  }

  test("test search with json write") {

    val out = run(
      "json write --package_root test_workspace --search --main_file test_workspace/Bar.bosatsu"
        .split("\\s+")
        .toSeq: _*
    )
    out match {
      case PathModule.Output.JsonOutput(j @ Json.JObject(_), _) =>
        assert(
          j.toMap == Map(
            "value" -> Json.JBool(true),
            "message" -> Json.JString("got the right string")
          )
        )
        assert(j.items.length == 2)
      case other => fail(s"expected json object output: $other")
    }
  }

  test("test search json apply") {
    val cmd =
      "json apply --input_dir test_workspace/ --package_root test_workspace/ --main Bosatsu/Nat::mult --json_string"
        .split("\\s+")
        .toList :+ "[2, 4]"

    run(cmd: _*) match {
      case PathModule.Output.JsonOutput(Json.JNumberStr("8"), _) => succeed
      case other => fail(s"expected json object output: $other")
    }
  }

  test("test search json traverse") {
    val cmd =
      "json traverse --input_dir test_workspace/ --package_root test_workspace/ --main Bosatsu/Nat::mult --json_string"
        .split("\\s+")
        .toList :+ "[[2, 4], [3, 5]]"

    run(cmd: _*) match {
      case PathModule.Output.JsonOutput(
            Json.JArray(Vector(Json.JNumberStr("8"), Json.JNumberStr("15"))),
            _
          ) =>
        succeed
      case other => fail(s"expected json object output: $other")
    }
  }

  test("error coverage on json command") {
    def fails(str: String, suffix: String*) =
      PathModule.run(str.split("\\s+").toList ::: suffix.toList) match {
        case Left(h) => fail(s"got help: $h, expected a non-help command")
        case Right(io) =>
          Try(io.unsafeRunSync()) match {
            case Success(s) => fail(s"got Success($s) expected to fail")
            case Failure(_) => succeed
          }
      }

    // ill-typed json fails
    val cmd =
      "json apply --input_dir test_workspace/ --package_root test_workspace/ --main Bosatsu/Nat::mult --json_string"
    fails(cmd, "[\"2\", 4]")
    fails(cmd, "[2, \"4\"]")
    // wrong arity
    fails(cmd, "[2, 4, 3]")
    fails(cmd, "[2]")
    fails(cmd, "[]")
    // unknown command fails
    val badName =
      "json apply --input_dir test_workspace/ --package_root test_workspace/ --main Bosatsu/Nat::foooooo --json_string 23"
    fails(badName)
    val badPack =
      "json apply --input_dir test_workspace/ --package_root test_workspace/ --main Bosatsu/DoesNotExist --json_string 23"
    fails(badPack)
    // bad json fails
    fails(cmd, "[\"2\", foo, bla]")
    fails(cmd, "[42, 31] and some junk")
    // exercise unsupported, we cannot write mult, it is a function
    fails(
      "json write --input_dir test_workspace/ --package_root test_workspace/ --main Bosatsu/Nat::mult"
    )
    // a bad main name triggers help
    PathModule.run(
      "json write --input_dir test_workspace --main Bo//".split(' ').toList
    ) match {
      case Left(_)  => succeed
      case Right(_) => fail()
    }
    PathModule.run(
      "json write --input_dir test_workspace --main Bo:::boop".split(' ').toList
    ) match {
      case Left(_)  => succeed
      case Right(_) => fail()
    }
  }

  test("test running all test in test_workspace") {

    val out = run(
      "test --package_root test_workspace --input_dir test_workspace"
        .split("\\s+")
        .toSeq: _*
    )
    out match {
      case PathModule.Output.TestOutput(res, _) =>
        val noTests = res.collect { case (pn, None) => pn }.toList
        assert(noTests == Nil)
        val failures = res.collect {
          case (pn, Some(t)) if t.value.failureCount > 0 => pn
        }
        assert(failures == Nil)
      case other => fail(s"expected test output: $other")
    }
  }

  test("evaluation by name with shadowing") {
    run(
      "json write --package_root test_workspace --input test_workspace/Foo.bosatsu --main Foo::x"
        .split("\\s+")
        .toSeq: _*
    ) match {
      case PathModule.Output.JsonOutput(Json.JString("this is Foo"), _) =>
        succeed
      case other => fail(s"unexpeced: $other")
    }
  }

}
