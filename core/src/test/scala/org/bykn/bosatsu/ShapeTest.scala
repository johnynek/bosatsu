package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.TypeEnv
import org.scalatest.funsuite.AnyFunSuite

class ShapeTest extends AnyFunSuite {

  def makeTE(
      teStr: String
  ): Either[Any, TypeEnv[Either[Shape.KnownShape, Kind.Arg]]] = {
    val te = TestUtils.parsedTypeEnvOf(PackageName.PredefName, teStr)
    Shape
      .solveAll((), te.allDefinedTypes.reverse)
      .fold(Left(_), Right(_), (a, _) => Left(a))
      .map(TypeEnv.fromDefinitions(_))
  }

  def testShape(teStr: String, shapes: Map[String, String]) =
    makeTE(teStr) match {
      case Right(te) =>
        shapes.foreach { case (n, vs) =>
          val dt =
            te.getType(
              PackageName.PredefName,
              TypeName(Identifier.Constructor(n))
            ).get
          val shape = Kind.parser.parseAll(vs) match {
            case Right(k) => Shape.ShapeOf(k)
            case Left(e)  => fail(s"parse error: $e")
          }
          assert(Shape.ShapeOf(dt) === shape, s"name: $n")
        }
      case Left(errs) =>
        fail(errs.toString)
    }

  def testIllShaped(teStr: String) =
    makeTE(teStr) match {
      case Left(_)   => assert(true)
      case Right(te) =>
        fail(
          te.allDefinedTypes
            .map { dt =>
              s"${dt.name} => ${Shape.ShapeOf(dt)}"
            }
            .mkString("\n")
        )
    }

  test("test some basic structs") {
    testShape(
      """#
struct Foo(a)
""",
      Map("Foo" -> "* -> *")
    )

    testShape(
      """#
struct Foo[a](x: a, y: a)
""",
      Map("Foo" -> "* -> *")
    )

    testShape(
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

  test("test list covariance") {
    testShape(
      """#
enum Lst: Empty, Cons(head: a, tail: Lst[a])
""",
      Map(
        "Lst" -> "+* -> *"
      )
    )
  }

  test("test higher kinded covariance") {
    testShape(
      """#
enum Lst[f, a]: Empty, Cons(head: a, tail: f[Lst[f, a]])
""",
      Map(
        "Lst" -> "(* -> *) -> * -> *"
      )
    )
    testIllShaped("""#
enum Lst: Empty, Cons(head: a, tail: f[Lst[f, a]])
""")
  }

  test("a unit type") {
    testShape(
      """#
struct U
""",
      Map(
        "U" -> "*"
      )
    )
  }

  test("test contravariance") {
    testShape(
      """#
struct Funk[a: -*, b: +*]
struct U

struct Inv[a](fn: Funk[a, U], value: a)
""",
      Map(
        "U" -> "*",
        "Funk" -> "* -> * -> *",
        "Inv" -> "* -> *"
      )
    )
  }

  test("test phantom shape") {
    testShape(
      """#
struct Phantom[a]
""",
      Map(
        "Phantom" -> "* -> *"
      )
    )
  }

  test("test error on illegal structs") {
    testIllShaped("""#
struct Foo(x: f[a], y: a[f])
""")

    testIllShaped("""#
struct Foo(x: f[a], y: f)
""")
  }
}
