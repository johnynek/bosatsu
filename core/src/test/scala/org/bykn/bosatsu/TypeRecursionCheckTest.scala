package org.bykn.bosatsu

import cats.data.Validated
import org.bykn.bosatsu.rankn.TypeEnv
import org.scalatest.funsuite.AnyFunSuite

class TypeRecursionCheckTest extends AnyFunSuite {

  def allowed(teStr: String) = {
    val te = TestUtils.typeEnvOf(PackageName.PredefName, teStr)
    VarianceFormula.solve(TypeEnv.empty, te.allDefinedTypes) match {
      case Left(errs) => fail(s"couldn't solve: $errs")
      case Right(teVar) =>
        assert(
          TypeRecursionCheck.checkLegitRecursion(TypeEnv.empty, teVar) ==
            Validated.valid(())
        )
    }
  }

  def disallowed(teStr: String) = {
    val te = TestUtils.typeEnvOf(PackageName.PredefName, teStr)
    VarianceFormula.solve(TypeEnv.empty, te.allDefinedTypes) match {
      case Left(errs) => fail(s"couldn't solve: $errs")
      case Right(teVar) =>
        assert(
          TypeRecursionCheck.checkLegitRecursion(TypeEnv.empty, teVar).isInvalid
        )
    }
  }

  test("linked list is allowed") {
    allowed("""#
enum Lst: E, N(head: a, tail: Lst[a])
""")
  }

  test("tree is allowed") {
    allowed("""#
enum Lst: E, N(head: a, tail: Lst[a])

struct Tree(root: a, children: Lst[Tree[a]])
""")
  }

  test("directory example is allowed") {
    allowed("""#
enum Lst: E, N(head: a, tail: Lst[a])

enum Path:
  Dir(name: String, children: Lst[Path])
  File(name: String, content: String)
""")
  }

  test("cont is allowed with Tree") {
    allowed("""#
struct Cont(fn: (a -> b) -> b)

struct Tree(root: a, children: Cont[Tree[a], b])
""")

    disallowed("""#
struct ICont(fn: (a -> a) -> a)

struct Tree(root: a, children: ICont[Tree[a]])
""")
  }

  test("y-combinator type is disallowed") {
    disallowed("""#
struct W(fn: W[a, b] -> a -> b)
""")
  }

  test("mutual recursion is (currently) completely unallowed") {
    disallowed("""#
struct Foo(bar: Bar)
struct Bar(foo: Foo)
""")
  }

  test("recursion with type constructors is disallowed") {
    disallowed("""#
struct Tree(root: a, children: f[Tree[a]])
""")
  }
}
