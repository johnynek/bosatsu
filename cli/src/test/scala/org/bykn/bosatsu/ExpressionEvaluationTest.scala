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
import PathModule.Output
import cats.effect.unsafe.implicits.global

class ExpressionEvaluationTest extends AnyFunSuite {

  def eeTest(
      fileNames: List[String],
      packageName: String,
      altAsserts: List[Output.EvaluationResult => Assertion] = Nil
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
          .map { case res @ Output.EvaluationResult(_, _, _) =>
            altAsserts match {
              case Nil => {
                val v = res.value
                val test = Test.fromValue(v.value)
                assert(test.assertions > 0)
                assert(test.failures == None)
              }
              case lst => lst.foreach(_.apply(res))
            }
          }
          .unsafeRunSync()
      }
    }
  }

  test("simple let free evaluate") {
    eeTest(List("Simple"), "Bosatsu/Simple")
  }

  test("missing module") {
    try {
      eeTest(List("Simple"), "Bosatsu/NotSimple")
      fail()
    } catch {
      case err: java.lang.Exception => {
        assert(err.getMessage() == "package Bosatsu/NotSimple not found")
      }
    }
  }

  test("list pat let free evaluate") {
    eeTest(List("ListPat", "List", "Bool", "Nat"), "ListPat")
    eeTest(
      List("ExtendedListPattern/ManyGlobs", "List", "Bool", "Nat"),
      "ManyGlobs"
    )
  }

  test("test workspace") {
    eeTest(List("euler1"), "Euler/One")
    // eeTest(List("euler2", "List", "Nat", "Bool"), "Euler/Two")
    eeTest(List("euler3", "List", "Nat", "Bool"), "Euler/Three")
    // eeTest(List("euler4", "List", "Nat", "Bool"), "Euler/Four")
    eeTest(List("euler5", "List", "Nat", "Bool"), "Euler/P5")
  }

}
