package dev.bosatsu

import cats.data.Ior
import cats.syntax.all._
import dev.bosatsu.rankn.{ParsedTypeEnv, Type => RankNType, TypeEnv}
class KindFormulaTest extends munit.FunSuite {
  private val predefTypeEnv: TypeEnv[Kind.Arg] =
    Package.interfaceOf(PackageMap.predefCompiled).exportedTypeEnv

  def makeTE(teStr: String): Either[Any, TypeEnv[Kind.Arg]] = {
    val te = TestUtils.parsedTypeEnvOf(PackageName.PredefName, teStr)
    KindFormula
      .solveShapesAndKinds(
        predefTypeEnv,
        te
      )
      .fold(Left(_), Right(_), (a, _) => Left(a))
      .map(TypeEnv.fromParsed(_))
  }

  def testPredef(shapes: Map[String, String]) = {
    val te = TestUtils.predefParsedTypeEnv
    val eitherTE = KindFormula
      .solveShapesAndKinds(
        predefTypeEnv,
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
          val name = TypeName(Identifier.Constructor(n))
          val leftK =
            te.getType(
              PackageName.PredefName,
              name
            )
              .map(_.kindOf)
              .orElse(te.getTypeAlias(PackageName.PredefName, name).map(_.kindOf))
              .getOrElse(fail(s"missing type or alias: $n"))
          val kind = Kind.parser.parseAll(vs) match {
            case Right(k) => k
            case Left(e)  => fail(s"parse error: $e")
          }
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

  private case class SolverPartial(
      errors: List[KindFormula.Error],
      solved: ParsedTypeEnv[Kind.Arg]
  )

  private def solveWithErrors(teStr: String): SolverPartial =
    KindFormula.solveShapesAndKinds(
      predefTypeEnv,
      TestUtils.parsedTypeEnvOf(PackageName.PredefName, teStr)
    ) match {
      case Ior.Both(errors, solved) => SolverPartial(errors.toList, solved)
      case Ior.Left(errors) =>
        fail(s"expected partial solver result, got only errors: $errors")
      case Ior.Right(solved) =>
        fail(s"expected solver errors, got successful result: $solved")
    }

  private def typeConst(name: String): RankNType.Const.Defined =
    RankNType.Const.predef(name)

  private def hasUnknown(
      result: SolverPartial,
      owner: String,
      unknown: String
  ): Boolean =
    result.errors.exists {
      case KindFormula.Error.FromShapeError(
            Shape.UnknownConst(typeDecl, _, const)
          ) =>
        typeDecl.name.asString == owner && const == typeConst(unknown)
      case _ => false
    }

  private def hasUnsatisfiable(
      result: SolverPartial,
      owner: String
  ): Boolean =
    result.errors.exists {
      case error: KindFormula.Error.Unsatisfiable =>
        error.typeDecl.name.asString == owner
      case _ => false
    }

  private def statementNames(
      statements: List[ParsedTypeEnv.TypeStatement[Kind.Arg]]
  ): List[String] =
    statements.map {
      case ParsedTypeEnv.TypeStatement.Defined(dt) => s"D:${dt.name.asString}"
      case ParsedTypeEnv.TypeStatement.Alias(ta)   => s"A:${ta.name.asString}"
    }

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

  test("external type parameters default to invariant") {
    testKind(
      """#
external type Box[a]
external type Mixed[a, b: +*, c: -*, d: 👻*, f: +* -> *]
struct Wrapper[a](value: Box[a])
""",
      Map(
        "Box" -> "* -> *",
        "Mixed" -> "* -> +* -> -* -> 👻* -> (+* -> *) -> *",
        "Wrapper" -> "* -> *"
      )
    )
  }

  test("type aliases infer variance through prior types and aliases") {
    testKind(
      """#
struct U
struct Bar[a](value: a)
struct Consumer[a](consume: a -> U)
external type Cell[a]
struct Tag[a]

type Foo[a] = Bar[a]
type Chain[a] = Foo[a]
type Input[a] = Consumer[a]
type Invariant[a] = Cell[a]
type Phantom[a] = Tag[a]
""",
      Map(
        "Foo" -> "+* -> *",
        "Chain" -> "+* -> *",
        "Input" -> "-* -> *",
        "Invariant" -> "* -> *",
        "Phantom" -> "👻* -> *"
      )
    )
  }

  test("type aliases infer higher kinds from their bodies") {
    testKind(
      """#
struct U
struct Bar[f](value: f[U])
struct CovariantOnly[f: +* -> *](value: f[U])

type Foo[a] = Bar[a]
type Chain[a] = Foo[a]
type NeedsCovariant[f] = CovariantOnly[f]
type Apply[f, a] = f[a]
""",
      Map(
        "Foo" -> "+(* -> *) -> *",
        "Chain" -> "+(* -> *) -> *",
        "NeedsCovariant" -> "(+* -> *) -> *",
        "Apply" -> "+(* -> *) -> * -> *"
      )
    )
  }

  test("type aliases reject annotations incompatible with their bodies") {
    testIllKinded("""#
struct U
struct Bar[f](value: f[U])
type Bad[a: *] = Bar[a]
""")

    testIllKinded("""#
external type Cell[a]
type Bad[a: +*] = Cell[a]
""")

    testIllKinded("""#
struct U
struct CovariantOnly[f: +* -> *](value: f[U])
type Bad[f: -* -> *] = CovariantOnly[f]
""")
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

  test("failed declarations suppress dependent shape and kind errors") {
    val result = solveWithErrors(
      """#
struct GoodBefore
type GoodAliasBefore = GoodBefore
external type Cell[a]

enum Bad:
  Bad(x: Missing)
struct GoodBetween
type GoodAliasBetween = GoodBetween
type AliasAfterBad = Bad
struct DefAfterAliasBad(value: AliasAfterBad)
struct UsesBadAndMissing(value: Bad, other: ExtraMissing)
type AliasAfterBadAndMissing = UsesBadAndMissing

type BadAlias = MissingAlias
struct DefAfterBadAlias(value: BadAlias)
type AliasAfterDefBadAlias = DefAfterBadAlias

struct BadKind[a: +*](value: Cell[a])
type AliasAfterBadKind = BadKind[Int]
struct DefAfterAliasBadKind(value: AliasAfterBadKind)

type BadAliasKind[a: +*] = Cell[a]
struct DefAfterBadAliasKind(value: BadAliasKind[Int])
type AliasAfterDefBadAliasKind = DefAfterBadAliasKind

type IndependentInvalid = ExtraMissing
struct GoodAfter
type GoodAliasAfter = GoodAfter
"""
    )

    val expectedErrorOwners = Set(
      typeConst("Bad"),
      typeConst("BadAlias"),
      typeConst("BadKind"),
      typeConst("BadAliasKind"),
      typeConst("IndependentInvalid")
    )
    assertEquals(result.errors.size, expectedErrorOwners.size)
    assertEquals(
      result.errors.map(_.typeDecl.toTypeConst).toSet,
      expectedErrorOwners
    )

    assert(hasUnknown(result, "Bad", "Missing"))
    assert(hasUnknown(result, "BadAlias", "MissingAlias"))
    assert(hasUnknown(result, "IndependentInvalid", "ExtraMissing"))
    assert(hasUnsatisfiable(result, "BadKind"))
    assert(hasUnsatisfiable(result, "BadAliasKind"))

    val dependentNames = Set(
      "AliasAfterBad",
      "DefAfterAliasBad",
      "UsesBadAndMissing",
      "AliasAfterBadAndMissing",
      "DefAfterBadAlias",
      "AliasAfterDefBadAlias",
      "AliasAfterBadKind",
      "DefAfterAliasBadKind",
      "DefAfterBadAliasKind",
      "AliasAfterDefBadAliasKind"
    )
    assert(
      result.errors.forall(error => !dependentNames(error.typeDecl.name.asString)),
      "dependent declarations should not own solver errors"
    )

    assertEquals(
      result.solved.allDefinedTypes.map(_.name.asString),
      List("GoodBefore", "Cell", "GoodBetween", "GoodAfter")
    )
    assertEquals(
      result.solved.typeAliases.map(_.name.asString),
      List("GoodAliasBefore", "GoodAliasBetween", "GoodAliasAfter")
    )
    assertEquals(
      statementNames(result.solved.orderedTypes),
      List(
        "D:GoodBefore",
        "A:GoodAliasBefore",
        "D:Cell",
        "D:GoodBetween",
        "A:GoodAliasBetween",
        "D:GoodAfter",
        "A:GoodAliasAfter"
      )
    )
  }

  test("free-functor is covariant") {
    allowed("""#
enum Free[a: +*]:
  Done(get: a)
  Map(tup: exists b. (Free[b], b -> a))
""")
  }

  test("regression: Eval-like variance failure reports kind failure, not invariant") {
    val res = makeTE("""#
enum Leaf[a: +*]:
  Done(done: a)

enum BindChain[f: +* -> *, a: -*, b: +*]:
  Single(fn: a -> f[b]),
  Many[c](first: BindChain[f, a, c], last: b -> f[b])

enum Eval[a: +*]:
  Pure(value: Leaf[a])
  FlatMap[b](prev: Leaf[b], chain: BindChain[Eval, b, a])

enum Stack[a, b]:
  Last(fn: a -> Eval[b])
  More[c](first: a -> Eval[c], rest: Stack[c, b])
""")

    assertEquals(res.left.map(_ => ()), Left(()))
  }
}
