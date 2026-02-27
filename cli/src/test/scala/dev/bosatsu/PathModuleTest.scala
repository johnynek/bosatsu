package dev.bosatsu

import cats.data.NonEmptyList
import java.nio.file.{Path, Paths}
import dev.bosatsu.tool.Output
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

// allow us to unsafeRunSync
import cats.effect.unsafe.implicits.global

class PathModuleTest extends munit.ScalaCheckSuite {
  override def munitTimeout: Duration = 6.minutes

  import PathModule.platformIO.pathPackage

  implicit val arbPath: Arbitrary[Path] =
    Arbitrary {
      val str = Gen.identifier

      Gen.listOf(str).map(parts => Paths.get(parts.mkString("/")))
    }

  test("tool test some hand written examples") {
    def pn(roots: List[String], file: String): Option[PackageName] =
      pathPackage(roots.map(Paths.get(_)), Paths.get(file))

    assertEquals(
      pn(List("/root0", "/root1"), "/root0/Bar.bosatsu"),
      Some(
        PackageName(NonEmptyList.of("Bar"))
      )
    )
    assertEquals(
      pn(List("/root0", "/root1"), "/root1/Bar/Baz.bosatsu"),
      Some(
        PackageName(NonEmptyList.of("Bar", "Baz"))
      )
    )
    assertEquals(
      pn(List("/root0", "/root0/Bar"), "/root0/Bar/Baz.bosatsu"),
      Some(
        PackageName(NonEmptyList.of("Bar", "Baz"))
      )
    )
    assertEquals(
      pn(List("/root0/", "/root0/Bar"), "/root0/Bar/Baz.bosatsu"),
      Some(
        PackageName(NonEmptyList.of("Bar", "Baz"))
      )
    )
    assertEquals(
      pn(
        List("/root0/ext", "/root0/Bar"),
        "/root0/ext/Bar/Baz.bosatsu"
      ),
      Some(PackageName(NonEmptyList.of("Bar", "Baz")))
    )
    assertEquals(
      pn(List("/root0"), "/root0/MyLib/Fib.bosatsu"),
      Some(PackageName(NonEmptyList.of("MyLib", "Fib")))
    )
    assertEquals(
      pn(List("/root0"), "/root0/mylib/fib.bosatsu"),
      Some(PackageName(NonEmptyList.of("Mylib", "Fib")))
    )
  }

  test("no roots means no Package") {
    forAll { (p: Path) =>
      assertEquals(pathPackage(Nil, p), None)
    }
  }

  test("empty path is not okay for a package") {
    forAll { (roots: List[Path]) =>
      assertEquals(pathPackage(roots, Paths.get("")), None)
    }
  }

  test("if we add to a path that becomes Package") {
    def dropExtension(parts: List[String]): List[String] =
      if (parts.isEmpty) Nil
      else {
        val init = parts.init
        val last = parts.last
        val idx = last.lastIndexOf('.')
        val noExt = if (idx > 0) last.substring(0, idx) else last
        init :+ noExt
      }

    def normalizePart(part: String): String =
      if (part.isEmpty) part
      else {
        val ch = part.charAt(0)
        if ('a' <= ch && ch <= 'z') ch.toUpper.toString + part.substring(1)
        else part
      }

    def expected(parts: List[String]): Option[PackageName] = {
      val noExt = dropExtension(parts)
      val raw = noExt.mkString("/")
      PackageName.parse(raw).orElse {
        val normalized = noExt.map(normalizePart).mkString("/")
        PackageName.parse(normalized)
      }
    }

    def law(root: Path, otherRoots: List[Path], rest: Path) =
      if (rest.toString != "" && root.toString != "") {
        val path = root.resolve(rest)
        val pack = expected(rest.asScala.map(_.toString).toList)
        assertEquals(pathPackage(root :: otherRoots, path), pack)
      }

    val prop = forAll(law(_, _, _))
    // some regressions:
    val regressions: List[(Path, List[Path], Path)] =
      List(
        (Paths.get(""), Nil, Paths.get("/foo/bar")),
        (Paths.get(""), List(Paths.get("")), Paths.get("/foo/bar"))
      )

    regressions.foreach { case (r, o, e) => law(r, o, e) }
    prop
  }

  test("if none of the roots are prefixes we have none") {
    forAll { (r0: Path, roots0: List[Path], file: Path) =>
      val roots = (r0 :: roots0).filterNot(_.toString == "")
      val pack = pathPackage(roots, file)

      val noPrefix = !roots.exists { r =>
        file.asScala.toList.startsWith(r.asScala.toList)
      }

      if (noPrefix) assertEquals(pack, None)
    }
  }

  def run(args: String*): Output[Path] =
    PathModule.run(args.toList) match {
      case Left(h)   => fail(s"got help: $h on command: ${args.toList}")
      case Right(io) =>
        io.attempt.unsafeRunSync() match {
          case Right(out) =>
            out
          case Left(err)  =>
            fail(s"${err.getMessage}\ncommand: ${args.toList.mkString(" ")}")
        }
    }

  test("tool test direct run of a file") {
    val deps = List("Nat", "List", "Bool", "Rand", "Properties", "BinNat")
    val inputs =
      deps.map(n => s"--input test_workspace/${n}.bosatsu").mkString(" ")
    val out = run(
      s"tool test $inputs --test_file test_workspace/Queue.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TestOutput(results, _) =>
        val res = results.collect {
          case (pn, Some(t)) if pn.asString == "Bosatsu/Collection/Queue" =>
            t.value
        }
        assertEquals(res.length, 1)
      case other => fail(s"expected test output: $other")
    }
  }

  test("tool test search run of a file") {
    val out = run(
      "tool test --package_root test_workspace --search --test_file test_workspace/Bar.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TestOutput(results, _) =>
        val res = results.collect {
          case (pn, Some(t)) if pn.asString == "Bar" => t.value
        }
        assertEquals(res.length, 1)
        assertEquals(res.head.assertions, 1)
        assertEquals(res.head.failureCount, 0)
      case other => fail(s"expected test output: $other")
    }
  }

  test("tool test python transpile on the entire test_workspace") {
    val out = run(
      "tool transpile --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace python --outdir pyout --externals test_workspace/Prog.bosatsu_externals --evaluators test_workspace/Prog.bosatsu_eval"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TranspileOut(_) =>
        assert(true)
      case other => fail(s"expected transpile output: $other")
    }
  }

  test("tool test search with json write") {

    val out = run(
      "tool json write --package_root test_workspace --search --main_file test_workspace/Bar.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.JsonOutput(j @ Json.JObject(_), _) =>
        assertEquals(
          j.toMap,
          Map(
            "value" -> Json.JBool(true),
            "message" -> Json.JString("got the right string")
          )
        )
        assertEquals(j.items.length, 2)
      case other => fail(s"expected json object output: $other")
    }
  }

  test("tool test search with json write --yaml") {
    val out = run(
      "tool json write --package_root test_workspace --search --main_file test_workspace/Bar.bosatsu --yaml"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.Basic(doc, _) =>
        val rendered = doc.render(120)
        assert(rendered.contains("value: true"), rendered)
        assert(
          rendered.contains("message: \"got the right string\""),
          rendered
        )
      case other => fail(s"expected yaml output: $other")
    }
  }

  test("tool test search json apply") {
    val cmd =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace/ --main Bosatsu/Num/Nat::mult --json_string"
        .split("\\s+")
        .toList :+ "[2, 4]"

    run(cmd*) match {
      case Output.JsonOutput(Json.JNumberStr("8"), _) => ()
      case other => fail(s"expected json object output: $other")
    }
  }

  test("tool test search json traverse") {
    val cmd =
      "tool json traverse --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace/ --main Bosatsu/Num/Nat::mult --json_string"
        .split("\\s+")
        .toList :+ "[[2, 4], [3, 5]]"

    run(cmd*) match {
      case Output.JsonOutput(
            Json.JArray(Vector(Json.JNumberStr("8"), Json.JNumberStr("15"))),
            _
          ) =>
        ()
      case other => fail(s"expected json object output: $other")
    }
  }

  test("error coverage on json command") {
    def fails(str: String, suffix: String*) =
      PathModule.run(str.split("\\s+").toList ::: suffix.toList) match {
        case Left(h)   => fail(s"got help: $h, expected a non-help command")
        case Right(io) =>
          Try(io.unsafeRunSync()) match {
            case Success(s) => fail(s"got Success($s) expected to fail")
            case Failure(_) => ()
          }
      }

    // ill-typed json fails
    val cmd =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace/ --main Bosatsu/Num/Nat::mult --json_string"
    fails(cmd, "[\"2\", 4]")
    fails(cmd, "[2, \"4\"]")
    // wrong arity
    fails(cmd, "[2, 4, 3]")
    fails(cmd, "[2]")
    fails(cmd, "[]")
    // unknown command fails
    val badName =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace/ --main Bosatsu/Num/Nat::foooooo --json_string 23"
    fails(badName)
    val badPack =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace/ --main Bosatsu/DoesNotExist --json_string 23"
    fails(badPack)
    // bad json fails
    fails(cmd, "[\"2\", foo, bla]")
    fails(cmd, "[42, 31] and some junk")
    // exercise unsupported, we cannot write mult, it is a function
    fails(
      "tool json write --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace/ --main Bosatsu/Num/Nat::mult"
    )
    // a bad main name triggers help
    PathModule.run(
      "tool json write --input_dir test_workspace --main Bo//".split(' ').toList
    ) match {
      case Left(_)  => ()
      case Right(_) => fail("expected invalid main name to fail")
    }
    PathModule.run(
      "tool json write --input_dir test_workspace --main Bo:::boop"
        .split(' ')
        .toList
    ) match {
      case Left(_)  => ()
      case Right(_) => fail("expected invalid main name to fail")
    }
  }

  test("tool test running all test in test_workspace") {

    val out = run(
      "tool test --package_root test_workspace --input_dir test_workspace --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TestOutput(res, _) =>
        val noTests = res.collect { case (pn, None) => pn }.toList
        assertEquals(noTests, Nil)
        val failures = res.collect {
          case (pn, Some(t)) if t.value.failureCount > 0 => pn
        }
        assertEquals(failures, Nil)
      case other => fail(s"expected test output: $other")
    }
  }

  test("evaluation by name with shadowing") {
    run(
      "tool json write --package_root test_workspace --input test_workspace/Foo.bosatsu --main Foo::x"
        .split("\\s+")
        .toSeq*
    ) match {
      case Output.JsonOutput(Json.JString("this is Foo"), _) =>
        ()
      case other => fail(s"unexpeced: $other")
    }
  }
}
