package org.bykn.bosatsu
import java.nio.file.{Path => JPath, Paths => JPaths}
import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite

import PathModule.MainCommand.{RunTests, PackageResolver}
import PathModule.Output
import cats.data.NonEmptyList

class ExprEvaluationTest extends AnyFunSuite {
  def testDirectory() =
    RunTests(
      PathGen.ChildrenOfDir[IO, JPath](
        JPaths.get(s"test_workspace"),
        PathModule.hasExtension(".bosatsu"),
        false,
        PathModule.unfoldDir.get
      ),
      Nil,
      PathGen.Combine[IO, JPath](Nil),
      LocationMap.Colorize.Console,
      PackageResolver.LocalRoots(
        NonEmptyList.of(JPaths.get(s"test_workspace")),
        None
      ),
      (pm, ext) => Evaluation(pm, ext)
    ).run
      .map { case res @ Output.TestOutput(tests, _) =>
        tests.collect { case (p, Some(evalTest)) =>
          val test = evalTest.value
          assert(test.assertions > 0)
          assert(test.failures == None)
        }
      }
      .unsafeRunSync()

  test("test workspace") {
    testDirectory()
  }
}
