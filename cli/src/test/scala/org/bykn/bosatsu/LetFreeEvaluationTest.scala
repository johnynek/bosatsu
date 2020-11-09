package org.bykn.bosatsu
import java.nio.file.{Path => JPath, Paths => JPaths}
import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import LetFreeEvaluation.{ComputedValue, LazyValue, ExtEnv, Cache}
import Value.ExternalValue
import org.scalatest.Assertion
import PathModule.MainCommand.{LetFreeEvaluate, MainIdentifier, PackageResolver}
import PathModule.Output

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
          .map {
            case res @ Output.LetFreeEvaluationResult(lfe, tpe, _, _) =>
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

  test("simple let free evaluate") {
    letFreeTest(List("Simple"), "Bosatsu/Simple")
  }

  test("simple let free evaluate json") {
    letFreeTest(
      List("Simple"),
      "Bosatsu/Simple",
      List(
        { v: Output.LetFreeEvaluationResult =>
          assert(
            v.optJ(v.value(None)).left.get.render == """{
  "name": "Simple pass and fail",
  "tests": [ { "value": true, "message": "Passing test" } ]
}""",
            "should just be some json"
          )
        }
      )
    )
  }

  test("fn out json") {
    letFreeTest(
      List("FnOut"),
      "Bosatsu/FnOut",
      List(
        { v: Output.LetFreeEvaluationResult =>
          assert(
            v.optJ(v.value(None))
              .right
              .get == "cannot convert type to Json: the type forall a. a -> a isn't supported",
            "should just be some json"
          )
        }
      )
    )
  }

  test("missing module") {
    try {
      letFreeTest(List("Simple"), "Bosatsu/NotSimple")
      fail()
    } catch {
      case err: java.lang.Exception => {
        assert(err.getMessage() == "package Bosatsu/NotSimple not found")
      }
    }
  }

  test("empty package") {
    try {
      letFreeTest(List("EmptyPackage"), "Bosatsu/EmptyPackage")
      fail()
    } catch {
      case err: java.lang.Exception => {
        assert(err.getMessage() == "found no main expression")
      }
    }
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
    letFreeTest(
      List("ExtendedListPattern/ManyGlobs", "List", "Bool", "Nat"),
      "ManyGlobs"
    )
  }

  test("String Concat Example") {
    letFreeTest(List("StrConcatExample"), "StrConcatExample")
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
      scope
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
