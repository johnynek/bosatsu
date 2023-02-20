package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.TypeEnv
import org.scalatest.funsuite.AnyFunSuite

class KindFormulaTest extends AnyFunSuite {

  def makeTE(teStr: String): Either[Any, TypeEnv[Kind.Arg]] = {
    val te = TestUtils.typeEnvOf(PackageName.PredefName, teStr)
    KindFormula.solveShapesAndKinds(
        (), te.allDefinedTypes
    )
    .fold(Left(_), Right(_), (a, _) => Left(a))
    .map(TypeEnv.fromDefinitions(_))
  }

  def testKind(teStr: String, shapes: Map[String, String]) = {
    makeTE(teStr) match {
      case Right(te) =>
        shapes.foreach { case (n, vs) =>
          val dt =
            te.getType(PackageName.PredefName, TypeName(Identifier.Constructor(n)))
          val kind = Kind.parser.parseAll(vs) match {
            case Right(k) => k
            case Left(e)  => fail(s"parse error: $e")
          }
          assert(dt.get.kindOf == kind)
        }
      case Left(errs) =>
        fail(errs.toString)
    }
  }

  def testIllKinded(teStr: String) =
    assert(makeTE(teStr).left.map(_ => ()) == Left(()))

  test("test some basic structs") {
    testKind(
      """#
struct Foo(a)
""",
      Map("Foo" -> "* -> *")
    )

    testKind(
      """#
struct Foo[a](x: a, y: a)
""",
      Map("Foo" -> "* -> *")
    )

    testKind(
      """#
struct K1[f, a](x: f[a])
struct K2[f: +* -> *, a: +*](x: f[a])
struct K3[f, a: *](x: f[a])
""",
      Map(
        "K1" -> "(* -> *) -> * -> *",
        "K2" -> "(* -> *) -> * -> *",
        "K3" -> "(* -> *) -> * -> *"
      )
    )
  }

  test("test error on illegal structs") {
    testIllKinded(
      """#
struct Foo(x: f[a], y: a[f])
""")

    testIllKinded(
      """#
struct Foo(x: f[a], y: f)
""")
  }
}
