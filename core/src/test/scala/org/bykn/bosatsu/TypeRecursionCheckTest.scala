package org.bykn.bosatsu

import cats.data.{Validated, Ior}
import org.bykn.bosatsu.rankn.TypeEnv
import org.scalatest.funsuite.AnyFunSuite

class TypeRecursionCheckTest extends AnyFunSuite {

  val predefTE: TypeEnv[Kind.Arg] = {
    val te = TestUtils.predefParsedTypeEnv
    KindFormula
      .solveShapesAndKinds(
        (),
        te.allDefinedTypes.reverse
      )
      .fold(Left(_), Right(_), (a, _) => Left(a))
      .map(TypeEnv.fromDefinitions(_)) match {
        case Right(res) => res
        case Left(err) => sys.error(err.toString)
      }
  }

  def allowed(teStr: String) = {
    val te = TestUtils.parsedTypeEnvOf(PackageName.PredefName, teStr)
    KindFormula.solveShapesAndKinds(predefTE, te.allDefinedTypes.reverse) match {
      case Ior.Right(teVar) =>
        assert(
          TypeRecursionCheck.checkLegitRecursion(TypeEnv.empty, teVar) ==
            Validated.valid(()))
      case errs => fail(s"couldn't solve: $errs")
    }
  }

  def disallowed(teStr: String) = {
    val te = TestUtils.parsedTypeEnvOf(PackageName.PredefName, teStr)
    KindFormula.solveShapesAndKinds(predefTE, te.allDefinedTypes.reverse) match {
      case Ior.Right(teVar) => fail(s"expected to fail: $teVar")
      case _ =>
        // we disallow in in solving now
        ()
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
struct Cont[a, b](fn: (a -> b) -> b)

struct Tree[a, b](root: a, children: Cont[Tree[a, b], b])
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
