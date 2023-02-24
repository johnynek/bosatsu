package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.{DefinedType, TypeEnv}
import org.scalatest.funsuite.AnyFunSuite

class VarianceFormulaTest extends AnyFunSuite {

  def testVariance(teStr: String, variances: Map[String, List[Variance]]) = {
    val te = TestUtils.typeEnvOf(PackageName.PredefName, teStr)
    VarianceFormula.solve(TypeEnv.empty, te.allDefinedTypes) match {
      case Left(errs) => fail(s"couldn't solve: $errs")
      case Right(teVar) =>
        val teMap = DefinedType.listToMap(teVar)
        variances.foreach { case (n, vs) =>
          val dt =
            teMap((PackageName.PredefName, TypeName(Identifier.Constructor(n))))
          assert(dt.annotatedTypeParams.map(_._2) == vs)
        }
    }
  }

  test("test some basic structs") {

    testVariance(
      """#
struct Foo(a)
""",
      Map("Foo" -> List(Variance.co))
    )

    testVariance(
      """#
struct Foo(a, b)
""",
      Map("Foo" -> List(Variance.co, Variance.co))
    )

    testVariance(
      """#
struct Foo(a: x, b: x)
""",
      Map("Foo" -> List(Variance.co))
    )

    testVariance(
      """#
struct Foo(a: x -> y)
""",
      Map("Foo" -> List(Variance.contra, Variance.co))
    )

    testVariance(
      """#
struct Foo(a: x -> x)
""",
      Map("Foo" -> List(Variance.in))
    )

    testVariance(
      """#
struct Foo(a: x -> y, b: z)
""",
      Map("Foo" -> List(Variance.contra, Variance.co, Variance.co))
    )

    testVariance(
      """#
struct Foo(a: x -> y, b: x)
""",
      Map("Foo" -> List(Variance.in, Variance.co))
    )

    testVariance(
      """#
enum Opt: None, Some(a)
""",
      Map("Opt" -> List(Variance.co))
    )

    testVariance(
      """#
enum Lst: E, NE(head: a, tail: Lst[a])
""",
      Map("Lst" -> List(Variance.co))
    )

    testVariance(
      """#
enum EitherFn: LeftFn(fn: a -> b), RightV(a: a)
""",
      Map("EitherFn" -> List(Variance.in, Variance.co))
    )

    testVariance(
      """#
struct Foo(produceB: forall a. a -> b)
""",
      Map("Foo" -> List(Variance.co))
    )

    testVariance(
      """#
struct Foo(produceB: (forall a. a) -> b)
""",
      Map("Foo" -> List(Variance.co))
    )

    testVariance(
      """#
struct F(fn: a -> b)
struct Foo(produceB: (forall a. F[a])[b])
""",
      Map("Foo" -> List(Variance.co))
    )
  }

  test("test cases with references to other types") {
    testVariance(
      """#
enum Opt: N, S(a)

struct Pair(fst, snd)

struct Lst(m: Opt[Pair[a, Lst[a]]])
""",
      Map(
        "Lst" -> List(Variance.co),
        "Pair" -> List(Variance.co, Variance.co),
        "Opt" -> List(Variance.co)
      )
    )

    testVariance(
      """#
enum Lst: E, NE(head: a, tail: Lst[a])

struct Tree(root: a, children: Lst[Tree[a]])
""",
      Map("Lst" -> List(Variance.co), "Tree" -> List(Variance.co))
    )
  }

  test("test with higher-kinder vars (always invariant now)") {
    testVariance(
      """#
enum Lst: E, NE(head: a, tail: f[Lst[a]])
""",
      Map("Lst" -> List(Variance.in, Variance.in))
    )

    testVariance(
      """#
enum Lst: E, NE(head: a, tail: Lst[f[a]])
""",
      Map("Lst" -> List(Variance.in, Variance.in))
    )

    testVariance(
      """#
struct Foo(a: f[a])
""",
      Map("Foo" -> List(Variance.in, Variance.in))
    )

    testVariance(
      """#
struct Leibniz(view: forall f. f[a] -> f[b])
""",
      Map("Leibniz" -> List(Variance.in, Variance.in))
    )
  }

  test("test mutual recursion in data types") {
    testVariance(
      """#
enum Opt: N, S(as: NEList[a])

struct NEList(head: a, tail: Opt[a])
""",
      Map("NEList" -> List(Variance.co), "Opt" -> List(Variance.co))
    )

  }

  test("test with external structs") {
    testVariance(
      """#
external struct Lst[a]
""",
      Map("Lst" -> List(Variance.in))
    )

    testVariance(
      """#
external struct Lst[a]

struct Tree(root: a, children: Lst[Tree[a]])
""",
      Map("Lst" -> List(Variance.in), "Tree" -> List(Variance.in))
    )
  }
}
