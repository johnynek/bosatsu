package org.bykn.bosatsu

import cats.data.NonEmptyList
import java.nio.file.{Path => JPath}
import java.nio.file.{Paths => JPaths}
import cats.effect.IO
import org.scalatest.FunSuite

class LetFreeExecutionTest extends FunSuite {
  import PathModule.MainCommand.LetFreeEvaluate
  import PathModule.MainCommand.PathGen
  import PathModule.MainCommand.MainIdentifier
  import PathModule.MainCommand.PackageResolver
  import PathModule.Output

  def letFreeTest(fileNames: List[String], packageName: String) =
    NonEmptyList.fromList(packageName.split("/").toList) match {
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
          MainIdentifier.FromPackage(PackageName(pn), None),
          PathGen.Combine[IO, JPath](Nil),
          LocationMap.Colorize.Console,
          PackageResolver.ExplicitOnly
        ).run
          .map {
            case res @ Output.LetFreeEvaluationResult(lfe, tpe, _, _) => {
              val v = res.value(None)
              val test = Test.fromValue(v)
              assert(test.assertions > 0)
              assert(test.failures == None)
            }
          }
          .unsafeRunSync()
    }

  test("simple let free evaluate") {
    letFreeTest(List("Simple"), "Bosatsu/Simple")
  }

  test("euler1 let free evaluate") {
    letFreeTest(List("euler1"), "Euler/One")
  }

  test("euler2 let free evaluate") {
    letFreeTest(List("euler2", "List", "Bool", "Nat"), "Euler/Two")
  }

  test("euler3 let free evaluate") {
    letFreeTest(List("euler3"), "Euler/Three")
  }

  test("euler4 let free evaluate") {
    letFreeTest(List("euler4", "List", "Bool", "Nat"), "Euler/Four")
  }

  test("euler5 let free evaluate") {
    letFreeTest(List("euler5", "List", "Bool", "Nat"), "Euler/P5")
  }

  test("euler6 let free evaluate") {
    letFreeTest(List("euler6", "List", "Bool", "Nat"), "Euler/P6")
  }

  test("euler7 let free evaluate") {
    letFreeTest(List("euler7", "List", "Bool", "Nat"), "Euler/P7")
  }

}
