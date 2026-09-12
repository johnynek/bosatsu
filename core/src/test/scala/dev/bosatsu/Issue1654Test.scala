package dev.bosatsu

import dev.bosatsu.rankn.Type

class Issue1654Test extends munit.FunSuite with ParTest {
  private val reproSource = """
package Repro/PolyTestDrop3

def for_all(xs: List[a], fn: a -> Bool) -> Bool:
  loop xs:
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

  private lazy val reproInferred: (PackageMap.Compiled, Package.Compiled) = {
    var out: Option[(PackageMap.Compiled, Package.Compiled)] = None
    TestUtils.testInferred(
      List(reproSource),
      reproPackage.asString,
      { (pm, _) =>
        val pack = pm.toMap.getOrElse(
          reproPackage,
          fail(s"missing inferred package: ${reproPackage.asString}")
        )
        out = Some((pm, pack))
      }
    )
    out match {
      case Some(value) => value
      case None        => fail("failed to compute issue 1654 repro result")
    }
  }

  private def withRepro[A](
      fn: (PackageMap.Compiled, Package.Compiled) => A
  ): A = {
    val (pm, pack) = reproInferred
    fn(pm, pack)
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
      val testRes = eval.evalTest(reproPackage) match {
        case Left(err) =>
          fail(s"unexpected test discovery error: $err")
        case Right(None) =>
          fail(s"expected test value in ${reproPackage.asString}")
        case Right(Some(test)) =>
          test.value
      }

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

  test("test discovery selects ProgTest over plain Test values") {
    val progSource = """
package Repro/ProgTestPick

from Bosatsu/Prog import ProgTest, pure

tests = ProgTest(_ -> pure(TestSuite("prog", [
  Assertion(True, "prog test")
])))
"""
    val progPackage = PackageName.parts("Repro", "ProgTestPick")
    val progPrelude = Predef.loadFileInCompile("test_workspace/Prog.bosatsu")

    TestUtils.testInferred(
      List(progPrelude, progSource),
      progPackage.asString,
      { (pm, _) =>
        val pack = pm.toMap.getOrElse(
          progPackage,
          fail(s"missing inferred package: ${progPackage.asString}")
        )
        Package.testEntry(pack) match {
          case Right(Some(Package.TestEntry.ProgTest(bindable, _, _))) =>
            assertEquals(bindable, Identifier.Name("tests"))
          case other =>
            fail(s"expected ProgTest entry, got: $other")
        }

        val eval =
          library.LibraryEvaluation.fromPackageMap(pm, Predef.jvmExternals)
        val testValue = eval.evalTest(progPackage) match {
          case Left(err) =>
            fail(s"unexpected test discovery error: $err")
          case Right(Some(test)) =>
            test.value
          case Right(None) =>
            fail(s"missing test output for ${progPackage.asString}")
        }

        assertEquals(testValue.assertions, 1)
        assertEquals(testValue.failureCount, 0)
      }
    )
  }

  test("test discovery fails when plain Test appears after ProgTest") {
    val progSource = """
package Repro/ProgTestOrder

from Bosatsu/Prog import ProgTest, pure

tests = ProgTest(_ -> pure(Assertion(True, "prog test")))
late = Assertion(True, "plain test after prog test")
"""
    val progPackage = PackageName.parts("Repro", "ProgTestOrder")
    val progPrelude = Predef.loadFileInCompile("test_workspace/Prog.bosatsu")

    TestUtils.testInferred(
      List(progPrelude, progSource),
      progPackage.asString,
      { (pm, _) =>
        val pack = pm.toMap.getOrElse(
          progPackage,
          fail(s"missing inferred package: ${progPackage.asString}")
        )
        Package.testEntry(pack) match {
          case Left(
                Package.TestDiscoveryError.PlainTestAfterProgTest(
                  packageName,
                  progTest,
                  plainAfter
                )
              ) =>
            assertEquals(packageName, progPackage)
            assertEquals(progTest, Identifier.Name("tests"))
            assertEquals(plainAfter.toList, List(Identifier.Name("late")))
          case other =>
            fail(s"expected ordering error, got: $other")
        }

        val eval =
          library.LibraryEvaluation.fromPackageMap(pm, Predef.jvmExternals)
        eval.evalTest(progPackage) match {
          case Left(
                Package.TestDiscoveryError.PlainTestAfterProgTest(
                  packageName,
                  _,
                  _
                )
              ) =>
            assertEquals(packageName, progPackage)
          case other =>
            fail(
              s"expected discovery error while evaluating tests, got: $other"
            )
        }
      }
    )
  }
}
