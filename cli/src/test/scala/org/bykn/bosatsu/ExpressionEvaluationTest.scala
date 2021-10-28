package org.bykn.bosatsu

import java.nio.file.{Path => JPath, Paths => JPaths}
import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
// import ExpressionEvaluation.{ComputedValue, LazyValue, ExtEnv, Cache}
import Value.ExternalValue
import org.scalatest.Assertion
import cats.data.NonEmptyList
import PathModule.MainCommand.{
  ExpressionEvaluate,
  MainIdentifier,
  PackageResolver
}

class LetFreeEvaluationTest extends AnyFunSuite {

  def letFreeTest(
      fileNames: List[String],
      packageName: String
  ) = {
    PackageName.parse(packageName) match {
      case None => fail(s"bad packageName: $packageName")
      case Some(pn) => {
        ExpressionEvaluate(
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
          .unsafeRunSync()
      }
    }
  }
}
