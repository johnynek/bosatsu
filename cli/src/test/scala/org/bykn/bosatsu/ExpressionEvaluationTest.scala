package org.bykn.bosatsu
import java.nio.file.{Path => JPath, Paths => JPaths}
import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import LetFreeEvaluation.{ComputedValue, LazyValue, ExtEnv, Cache}
import Value.ExternalValue
import org.scalatest.Assertion
import PathModule.MainCommand.{
  LetFreeEvaluate,
  MainIdentifier,
  PackageResolver,
  LetFreeTestRun
}
import PathModule.Output
import cats.data.NonEmptyList

class LetFreeEvaluationTest extends AnyFunSuite {

  def letFreeTest(
      fileNames: List[String],
      packageName: String,
      altAsserts: List[Output.LetFreeEvaluationResult => Assertion] = Nil
  ) =
    PackageName.parse(packageName) match {
      case None => fail(s"bad packageName: $packageName")
      case Some(pn) =>
        LetFreeEvaluate(
          PathGen.Combine(
            fileNames.map(fileName =>
              PathGen.Direct[IO, JPath](
                JPaths.get(s"test_workspace/${fileName}.bosatsu")
              )
            )
          ),
          MainIdentifier.FromPackage(pn, None),
          PathGen.Combine[IO, JPath](Nil),
          LocationMap.Colorize.Console,
          PackageResolver.ExplicitOnly
        ).run
          .map { case res @ Output.LetFreeEvaluationResult(lfe, tpe, _, _) =>
            altAsserts match {
              case Nil => {
                val v = res.value(None)
                val test = Test.fromValue(v)
                assert(test.assertions > 0)
                assert(test.failures == None)
              }
              case lst => lst.foreach(_.apply(res))
            }
          }
          .unsafeRunSync()
    }
}
