package dev.bosatsu

import dev.bosatsu.rankn.Type

class Issue1654Test extends munit.FunSuite with ParTest {
  private val reproSource = """
package Repro/PolyTestDrop3

def for_all(xs: List[a], fn: a -> Bool) -> Bool:
  recur xs:
    case []: True
    case [h, *t] if fn(h):
      for_all(t, fn)
    case [_, *_]:
      False

string_tests = TestSuite("string", [
  Assertion(True, "string runs")
])

tests = TestSuite("all", [
  string_tests,
  Assertion(for_all([], _ -> False), "poly assertion"),
  Assertion(False, "this should fail if tests runs")
])
"""

  private val reproPackage = PackageName.parts("Repro", "PolyTestDrop3")
  private val stringTestsName = Identifier.Name("string_tests")
  private val testsName = Identifier.Name("tests")

  private def withRepro[A](
      fn: (PackageMap.Inferred, Package.Inferred) => A
  ): A = {
    var out: Option[A] = None
    TestUtils.testInferred(
      List(reproSource),
      reproPackage.asString,
      { (pm, _) =>
        val pack = pm.toMap.getOrElse(
          reproPackage,
          fail(s"missing inferred package: ${reproPackage.asString}")
        )
        out = Some(fn(pm, pack))
      }
    )
    out match {
      case Some(value) => value
      case None        => fail("failed to compute issue 1654 repro result")
    }
  }

  test(
    "issue 1654: inferred tests binding is monomorphic and test failure is not dropped"
  ) {
    withRepro { (pm, pack) =>
      val testsExpr = pack.lets.findLast(_._1 == testsName) match {
        case Some((_, _, te)) => te
        case None             =>
          fail(s"missing ${testsName.sourceCodeRepr} in inferred lets")
      }

      assertEquals(testsExpr.getType, Type.TestType)
      assertEquals(Type.forallList(testsExpr.getType), Nil)
      assertEquals(Type.existList(testsExpr.getType), Nil)
      assertEquals(Package.testValue(pack).map(_._1), Some(testsName))

      val eval =
        library.LibraryEvaluation.fromPackageMap(pm, Predef.jvmExternals)
      val testRes = eval
        .evalTest(reproPackage)
        .getOrElse(fail(s"expected test value in ${reproPackage.asString}"))
        .value

      assertEquals(testRes.assertions, 3)
      assertEquals(testRes.failureCount, 1)
    }
  }

  test("issue 1654: test discovery accepts vacuous forall test type") {
    withRepro { (_, pack) =>
      val quantifiedTestType =
        Type.forAll(
          (Type.Var.Bound("a"), Kind.Type) :: Nil,
          Type.TestType
        )

      val rewrittenLets = pack.lets.map {
        case (`testsName`, rec, te) =>
          (testsName, rec, TypedExpr.Annotation(te, quantifiedTestType, None))
        case other => other
      }
      val rewritten =
        pack.copy(program =
          (pack.program._1.copy(lets = rewrittenLets), pack.program._2)
        )

      val rewrittenTestsType =
        rewritten.lets.findLast(_._1 == testsName) match {
          case Some((_, _, te)) => te.getType
          case None             =>
            fail(s"missing ${testsName.sourceCodeRepr} after rewriting lets")
        }

      assert(
        Type.forallList(rewrittenTestsType).nonEmpty,
        rewrittenTestsType.toString
      )
      assertEquals(Package.testValue(rewritten).map(_._1), Some(testsName))
      assert(rewritten.lets.exists(_._1 == stringTestsName))
    }
  }
}
