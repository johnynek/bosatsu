package dev.bosatsu

import cats.data.Chain
import cats.implicits._
import dev.bosatsu.library.{LibConfig, Libraries, Name, Version}
import dev.bosatsu.tool.Output
import munit.FunSuite
import scala.collection.immutable.SortedMap

class LibBuildImplicitPackageTest extends FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private val module = MemoryMain[ErrorOr]

  private def renderJson[A: Json.Writer](value: A): String =
    Json.Writer.write(value).render

  private def errMsg(err: Throwable): String =
    module.mainExceptionToString(err).orElse(Option(err.getMessage)).getOrElse(
      err.toString
    )

  private val progSrc =
    """package Bosatsu/Prog

export Main()

struct Main(x: Int)
"""

  private val fibSrc =
    """from Bosatsu/Prog import Main

main = Main(0)
"""

  private def baseFiles: List[(Chain[String], String)] = {
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> progSrc,
      Chain("repo", "src", "MyLib", "Fib.bosatsu") -> fibSrc
    )
  }

  test("lib build uses implicit package names derived from path") {
    val cmd =
      List("lib", "build", "--repo_root", "repo", "--outdir", "out", "-m",
        "MyLib/Fib")

    module.runWith(baseFiles)(cmd) match {
      case Right(Output.Basic(_, _)) => ()
      case Right(other)              => fail(s"unexpected output: $other")
      case Left(err)                 => fail(errMsg(err))
    }
  }

  test("invalid main reports known packages") {
    val cmd =
      List("lib", "build", "--repo_root", "repo", "--outdir", "out", "-m",
        "Does/NotExist")

    module.runWith(baseFiles)(cmd) match {
      case Right(out) =>
        fail(s"expected failure, got output: $out")
      case Left(err) =>
        val msg = errMsg(err)
        assert(msg.contains("known packages"), msg)
        assert(msg.contains("MyLib/Fib"), msg)
    }
  }
}
