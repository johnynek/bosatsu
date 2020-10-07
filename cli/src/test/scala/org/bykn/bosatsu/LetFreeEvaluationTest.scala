package org.bykn.bosatsu

import cats.Eval
import cats.data.NonEmptyList
import java.nio.file.{Path => JPath}
import java.nio.file.{Paths => JPaths}
import cats.effect.IO
import org.scalatest.FunSuite
import LetFreeEvaluation.{ComputedValue, LazyValue, ExtEnv, Cache}
import Value.ExternalValue

class LetFreeEvaluationTest extends FunSuite {
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

  test("list pat let free evaluate") {
    letFreeTest(List("ListPat", "List", "Bool", "Nat"), "ListPat")
    letFreeTest(List("ExtendedListPattern/ManyGlobs", "List", "Bool", "Nat"), "ManyGlobs")
  }

  test("LetFreeValue") {
    implicit val extEnv: ExtEnv = Map()
    implicit val cache: Cache = None
    val scope = List(
      ComputedValue(ExternalValue(100)),
      ComputedValue(ExternalValue(200)),
      ComputedValue(ExternalValue(300)),
      ComputedValue(ExternalValue(400))
    )
    val lv = LazyValue(
      LetFreeExpression.App(
        LetFreeExpression.Lambda(LetFreeExpression.LambdaVar(1)),
        LetFreeExpression.LambdaVar(3)
      ),
      scope,
      Eval.later(ExternalValue(100))
    )

    val computedJson0 = Json.JObject(
      List(
        "state" -> Json.JString("computed"),
        "data" -> Json.JString("ExternalValue(100)")
      )
    )
    val computedJson3 = Json.JObject(
      List(
        "state" -> Json.JString("computed"),
        "data" -> Json.JString("ExternalValue(400)")
      )
    )

    assert(scope(0).toJson == computedJson0)
    assert(scope(3).toJson == computedJson3)
    assert(lv.cleanedScope == List((0, scope(0)), (3, scope(3))))
    assert(
      lv.toJson == Json.JObject(
        List(
          "state" -> Json.JString("expression"),
          "expression" -> Json.JString(
            "App(Lambda(LambdaVar(1)),LambdaVar(3))"
          ),
          "scope" -> Json.JArray(
            Vector(
              Json.JArray(
                Vector(
                  Json.JNumberStr("0"),
                  computedJson0
                )
              ),
              Json.JArray(
                Vector(
                  Json.JNumberStr("3"),
                  computedJson3
                )
              )
            )
          )
        )
      )
    )
  }
}
