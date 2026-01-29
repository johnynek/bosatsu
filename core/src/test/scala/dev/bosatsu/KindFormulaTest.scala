package dev.bosatsu

import dev.bosatsu.rankn.TypeEnv
class KindFormulaTest extends munit.FunSuite {

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

  def testKindEither(
      te: Either[Any, TypeEnv[Kind.Arg]],
      shapes: Map[String, String]
  ) =
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
          assertEquals(
            leftK,
            kind,
            s"for name: $n, ${Kind.toDoc(leftK).render(80)} != ${Kind.toDoc(kind).render(80)}"
          )
        }
      case Left(errs) =>
        fail(errs.toString)
    }

  def testIllKinded(teStr: String) =
    assertEquals(makeTE(teStr).left.map(_ => ()), Left(()))

  def allowed(teStr: String) = testKind(teStr, Map.empty)
  def disallowed(teStr: String) = testIllKinded(teStr)

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
        "Tuple2" -> "+* -> +* -> *",
        "Tuple3" -> "+* -> +* -> +* -> *",
        "Dict" -> "* -> +* -> *"
      )
    )
  }

  test("test Applicative example") {
    testKind(
      """#
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

""",
      Map("Applicative" -> "(* -> *) -> *")
    )
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

  test("recursion with type constructors is allowed") {
    allowed("""#
struct Tree(root: a, children: f[Tree[a, f]])
""")
  }

  test("we don't throw when a previous kind fails") {
    disallowed("""#
struct Foo[a: -*](get: a)    
struct Bar[a](foo: Foo[a])

""")
    disallowed("""#
struct Bar[a](foo: Foo[a])

""")
  }

  test("free-functor is covariant") {
    allowed("""#
enum Free[a: +*]:
  Done(get: a)
  Map(tup: exists b. (Free[b], b -> a))
""")
  }
}
