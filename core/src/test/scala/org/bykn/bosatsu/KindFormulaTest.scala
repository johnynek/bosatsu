package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.TypeEnv
import org.scalatest.funsuite.AnyFunSuite

class KindFormulaTest extends AnyFunSuite {

  def makeTE(teStr: String): Either[Any, TypeEnv[Kind.Arg]] = {
    val te = TestUtils.parsedTypeEnvOf(PackageName.PredefName, teStr)
    KindFormula
      .solveShapesAndKinds(
        (),
        te.allDefinedTypes.reverse
      )
      .fold(Left(_), Right(_), (a, _) => Left(a))
      .map(TypeEnv.fromDefinitions(_))
  }

  def testPredef(shapes: Map[String, String]) = {
    val te = TestUtils.predefParsedTypeEnv
    val eitherTE = KindFormula
      .solveShapesAndKinds(
        (),
        te.allDefinedTypes.reverse
      )
      .fold(Left(_), Right(_), (a, _) => Left(a))
      .map(TypeEnv.fromDefinitions(_))

    testKindEither(eitherTE, shapes)
  }

  def testKind(teStr: String, shapes: Map[String, String]) =
    testKindEither(makeTE(teStr), shapes)

  def testKindEither(te: Either[Any, TypeEnv[Kind.Arg]], shapes: Map[String, String]) =
    te match {
      case Right(te) =>
        shapes.foreach { case (n, vs) =>
          val dt =
            te.getType(
              PackageName.PredefName,
              TypeName(Identifier.Constructor(n))
            )
          val kind = Kind.parser.parseAll(vs) match {
            case Right(k) => k
            case Left(e)  => fail(s"parse error: $e")
          }
          val leftK = dt.get.kindOf
          assert(
            leftK == kind,
            s"for name: $n, ${Kind.toDoc(leftK).render(80)} != ${Kind.toDoc(kind).render(80)}"
          )
        }
      case Left(errs) =>
        fail(errs.toString)
    }

  def testIllKinded(teStr: String) =
    assert(makeTE(teStr).left.map(_ => ()) == Left(()))

  test("test some basic structs") {
    testKind(
      """#
struct Foo(a)
""",
      Map("Foo" -> "+* -> *")
    )

    testKind(
      """#
struct Foo[a](x: a, y: a)
""",
      Map("Foo" -> "+* -> *")
    )

    testKind(
      """#
struct K1[f, a](x: f[a])
struct K2[f: +* -> *, a: +*](x: f[a])
struct K3[f, a: +*](x: f[a])
struct K4[f, a: -*](x: f[a])
""",
      Map(
        "K1" -> "+(* -> *) -> * -> *",
        "K2" -> "(+* -> *) -> +* -> *",
        "K3" -> "+(+* -> *) -> +* -> *",
        "K4" -> "+(-* -> *) -> -* -> *"
      )
    )
  }

  test("test phantom") {
    testKind(
      """#
struct Phantom[a]
""",
      Map(
        "Phantom" -> "👻* -> *"
      )
    )
  }

  test("test contravariance") {
    testKind(
      """#
struct Funk[a: -*, b: +*]
struct U

struct Inv[a](fn: Funk[a, U], value: a)
""",
      Map(
        "U" -> "*",
        "Funk" -> "-* -> +* -> *",
        "Inv" -> "* -> *"
      )
    )
  }

  test("test list covariance") {
    testKind(
      """#
enum Lst[a]: Empty, Cons(head: a, tail: Lst[a])
""",
      Map(
        "Lst" -> "+* -> *"
      )
    )
  }

  test("test kind1 covariance") {
    testKind(
      """#
enum Tree[f, a]: Empty, Branch(head: a, tail: f[Tree[f, a]])
enum Tree1[f, a, phantom]: Empty, Branch(head: a, tail: f[Tree1[f, a, phantom]])
""",
      Map(
        "Tree" -> "+(+* -> *) -> +* -> *",
        "Tree1" -> "+(+* -> *) -> +* -> 👻* -> *"
      )
    )
  }

  test("test monad") {
    testKind(
      """#
struct Fn[a: -*, b: +*]

struct Monad(pure: forall a. a -> f[a], flatten: forall a. f[f[a]] -> f[a])
""",
      Map(
        "Monad" -> "(* -> *) -> *"
      )
    )
  }

  test("test leibniz") {
    testKind(
      """#
struct Fn[a: -*, b: +*]

struct Leib[a, b](cast: forall f: * -> *. f[a] -> f[b])
""",
      Map(
        "Leib" -> "* -> * -> *"
      )
    )
  }

  test("test error on illegal structs") {
    testIllKinded("""#
struct Foo(x: f[a], y: a[f])
""")

    testIllKinded("""#
struct Foo(x: f[a], y: f)
""")

    testIllKinded("""#
struct Foo[a: -*](x: a)
""")
    testIllKinded("""#
struct Foo[a: 👻*](x: a)
""")
    testIllKinded("""#
struct Fn[a: -*, b: +*]

# missing kind on f
struct Leib[a, b](cast: forall f. f[a] -> f[b])
""")
  }

  test("we can find all the kinds in predef") {
    testPredef(
      Map(
        "List" -> "+* -> *",
        "Order" -> "-* -> *",
        "Option" -> "+* -> *",
        "Tree" -> "+* -> *",
        "Unit" -> "*",
        "TupleCons" -> "+* -> +* -> *",
        // TODO: we could make this * -> +* -> * if we were more careful
        // that would allow some recursions we don't currently
        "Dict" -> "* -> * -> *"
      )
    )
  }
  
  test("test Applicative example") {
  testKind("""#
# Represents the Applicative typeclass
struct Fn[a: -*, b: +*]
struct Unit
struct TupleCons(first, second)

struct Applicative(
  pure: forall a. a -> f[a],
  map: forall a, b. (a -> b) -> f[a] -> f[b],
  ap: forall a, b. f[a -> b] -> f[a] -> f[b],
  map2: forall a, b, c. f[a] -> f[b] -> (a -> b -> c) -> f[c],
  product: forall a, b. f[a] -> f[b] -> f[(a, b)])

""", Map("Applicative" -> "(* -> *) -> *"))
  }
}
