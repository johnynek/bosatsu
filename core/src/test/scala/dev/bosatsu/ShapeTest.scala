package dev.bosatsu

import cats.data.{Ior, NonEmptyChain, Validated}
import dev.bosatsu.rankn.{DefinedType, ParsedTypeEnv, TypeAlias, TypeEnv}

class ShapeTest extends munit.FunSuite {

  private type SolvedType = DefinedType[Either[Shape.KnownShape, Kind.Arg]]
  private type SolvedAlias = TypeAlias[Either[Shape.KnownShape, Kind.Arg]]

  private def parsedTE(
      teStr: String
  ): ParsedTypeEnv[Option[Kind.Arg]] =
    TestUtils.parsedTypeEnvOf(PackageName.PredefName, teStr)

  private def solveDefinedTypes(
      te: ParsedTypeEnv[Option[Kind.Arg]]
  ): List[SolvedType] =
    Shape
      .solveAll((), te.allDefinedTypes.reverse)
      .fold(errs => fail(errs.toString), identity, (errs, _) => fail(errs.toString))

  private def solveAliases(
      imports: List[SolvedType],
      aliases: List[TypeAlias[Option[Kind.Arg]]]
  ): Ior[NonEmptyChain[Shape.Error], List[SolvedAlias]] = {
    val (errs, solved) =
      aliases.foldLeft((List.empty[Shape.Error], List.empty[SolvedAlias])) {
        case ((errsAcc, solvedAcc), alias) =>
          Shape.solveAlias((imports, solvedAcc), alias) match {
            case Validated.Valid(good) =>
              (errsAcc, good :: solvedAcc)
            case Validated.Invalid(nec) =>
              (nec.iterator.toList.reverse_:::(errsAcc), solvedAcc)
          }
      }

    NonEmptyChain.fromSeq(errs.reverse) match {
      case None if solved.isEmpty => Ior.Right(Nil)
      case None                   => Ior.Right(solved.reverse)
      case Some(nec) if solved.isEmpty =>
        Ior.Left(nec)
      case Some(nec) =>
        Ior.Both(nec, solved.reverse)
    }
  }

  private def parseShape(shapeStr: String): Shape.KnownShape =
    Kind.parser.parseAll(shapeStr) match {
      case Right(k) => Shape.shapeOf(k)
      case Left(e)  => fail(s"parse error: $e")
    }

  private def getAlias(
      aliases: List[TypeAlias[Option[Kind.Arg]]],
      name: String
  ): TypeAlias[Option[Kind.Arg]] =
    aliases.find(_.name.asString == name).getOrElse {
      fail(s"missing alias: $name")
    }

  def makeTE(
      teStr: String
  ): Either[Any, TypeEnv[Either[Shape.KnownShape, Kind.Arg]]] = {
    val te = parsedTE(teStr)
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
          assertEquals(Shape.ShapeOf(dt), shape, s"name: $n")
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

  test("solveAlias infers alias shape from prior local types") {
    val te = parsedTE("""#
struct U
struct Box(item: a)
type Foo = Box[U]
""")
    val solvedDts = solveDefinedTypes(te)
    val foo = getAlias(te.typeAliases.reverse, "Foo")

    Shape.solveAlias(solvedDts, foo) match {
      case Validated.Valid(alias) =>
        assertEquals(Shape.ShapeOf(alias), parseShape("*"))
      case Validated.Invalid(errs) =>
        fail(errs.toString)
    }
  }

  test("solveAliases supports alias chains") {
    val te = parsedTE("""#
struct U
struct Box(item: a)
type Foo = Box[U]
type Bar = Foo
type Wrap[a] = Box[a]
""")
    val solvedDts = solveDefinedTypes(te)

    solveAliases(solvedDts, te.typeAliases.reverse) match {
      case Ior.Right(aliases) =>
        assertEquals(aliases.map(_.name.asString), List("Foo", "Bar", "Wrap"))
        assertEquals(Shape.ShapeOf(aliases(0)), parseShape("*"))
        assertEquals(Shape.ShapeOf(aliases(1)), parseShape("*"))
        assertEquals(Shape.ShapeOf(aliases(2)), parseShape("* -> *"))
      case Ior.Left(errs) =>
        fail(errs.toString)
      case Ior.Both(errs, _) =>
        fail(errs.toString)
    }
  }

  test("solveAliases keeps prior successes when a later alias fails") {
    val te = parsedTE("""#
struct U
struct Box(item: a)
type Good = Box[U]
type Bad = Missing[U]
""")
    val solvedDts = solveDefinedTypes(te)

    solveAliases(solvedDts, te.typeAliases.reverse) match {
      case Ior.Both(errs, aliases) =>
        assertEquals(aliases.map(_.name.asString), List("Good"))
        assertEquals(Shape.ShapeOf(aliases.head), parseShape("*"))
        assert(
          errs.exists {
            case Shape.UnknownConst(typeDecl, Shape.Source.AliasBody, _) =>
              typeDecl.name.asString == "Bad"
            case _ => false
          }
        )
      case Ior.Left(errs) =>
        fail(s"expected partial success, got only errors: ${errs.toString}")
      case Ior.Right(aliases) =>
        fail(s"expected alias failure, solved: ${aliases.map(_.name.asString)}")
    }
  }
}
