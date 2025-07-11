package org.bykn.bosatsu.rankn

import cats.data.{Ior, NonEmptyList}
import org.bykn.bosatsu._

import Expr._
import Type.Var.Bound
import Type.forAll

import TestUtils.{checkLast, testPackage}

import Identifier.Constructor

import cats.syntax.all._

class RankNInferTest extends munit.FunSuite {

  val emptyRegion: Region = Region(0, 0)

  implicit val unitRegion: HasRegion[Unit] =
    HasRegion.instance(_ => emptyRegion)

  private def strToConst(str: Identifier.Constructor): Type.Const =
    str.asString match {
      case "Int"    => Type.Const.predef("Int")
      case "String" => Type.Const.predef("String")
      case "List"   => Type.Const.predef("List")
      case _        => Type.Const.Defined(testPackage, TypeName(str))
    }

  def asFullyQualified(
      ns: Iterable[(Identifier, Type)]
  ): Map[Infer.Name, Type] =
    ns.iterator.map { case (n, t) => ((Some(testPackage), n), t) }.toMap

  def typeFrom(str: String): Type = {
    val typeRef = Parser.unsafeParse(TypeRef.parser, str)
    TypeRefConverter[cats.Id](typeRef)(strToConst(_))
  }

  def runUnify(left: String, right: String) = {
    val t1 = typeFrom(left)
    val t2 = typeFrom(right)

    (Infer.substitutionCheck(t1, t2, emptyRegion, emptyRegion) >>
      Infer.substitutionCheck(t2, t1, emptyRegion, emptyRegion))
      .runFully(Map.empty, Map.empty, Type.builtInKinds)
  }

  def assertTypesUnify(left: String, right: String) = {
    val res = runUnify(left, right)
    assert(res.isRight, s"$left does not unify with $right\n\n$res")
  }

  // Test that left is strictly smaller (not equal to right)
  def assert_:<:(left: String, right: String) = {
    val t1 = typeFrom(left)
    val t2 = typeFrom(right)

    val res1 = Infer
      .substitutionCheck(t1, t2, emptyRegion, emptyRegion)
      .runFully(Map.empty, Map.empty, Type.builtInKinds)
    assert(res1.isRight, s"$left is not :<: $right\n\n$res1")

    val res2 = Infer
      .substitutionCheck(t2, t1, emptyRegion, emptyRegion)
      .runFully(Map.empty, Map.empty, Type.builtInKinds)
    assert(res2.isLeft, s"$left is unexpectedly = $right\n\n$res2")
  }

  def assertTypesDisjoint(left: String, right: String) = {
    val t1 = typeFrom(left)
    val t2 = typeFrom(right)

    val res1 = Infer
      .substitutionCheck(t1, t2, emptyRegion, emptyRegion)
      .runFully(Map.empty, Map.empty, Type.builtInKinds)
    val res2 = Infer
      .substitutionCheck(t2, t1, emptyRegion, emptyRegion)
      .runFully(Map.empty, Map.empty, Type.builtInKinds)

    assert(res1.isLeft, s"$left is unexpectedly :<: $right\n\n$res1")
    assert(res2.isLeft, s"$right is unexpectedly :<: $left\n\n$res2")
  }

  def defType(n: String): Type.Const.Defined =
    Type.Const.Defined(testPackage, TypeName(Identifier.Constructor(n)))

  def b(a: String) = (Type.Var.Bound(a), Kind.Type)
  def b1(a: String) = (Type.Var.Bound(a), Kind(Kind.Type.in))

  val withBools: Map[Infer.Name, Type] =
    Map(
      (
        Some(PackageName.PredefName),
        Identifier.unsafe("True")
      ) -> Type.BoolType,
      (
        Some(PackageName.PredefName),
        Identifier.unsafe("False")
      ) -> Type.BoolType
    )
  val boolTypes: Map[(PackageName, Constructor), Infer.Cons] =
    Map(
      (
        (PackageName.PredefName, Constructor("True")),
        (Nil, Nil, Type.Const.predef("Bool"))
      ),
      (
        (PackageName.PredefName, Constructor("False")),
        (Nil, Nil, Type.Const.predef("Bool"))
      )
    )

  def testType[A: HasRegion](term: Expr[A], ty: Type) =
    Infer.typeCheck(term).runFully(withBools, boolTypes, Map.empty) match {
      case Left(err)  => assert(false, err)
      case Right(tpe) => assert(tpe.getType.sameAs(ty), term.toString)
    }

  def testLetTypes[A: HasRegion](terms: List[(String, Expr[A], Type)]) =
    Infer
      .typeCheckLets(
        testPackage,
        terms.map { case (k, v, _) =>
          (Identifier.Name(k), RecursionKind.NonRecursive, v)
        },
        Map.empty
      )
      .runFully(withBools, boolTypes, Type.builtInKinds) match {
      case Left(err)   => assert(false, err)
      case Right(tpes) =>
        assert(tpes.size == terms.size)
        terms.zip(tpes).foreach { case ((n, exp, expt), (n1, _, te)) =>
          assert(n == n1.asString, s"the name changed: $n != $n1")
          assert(
            te.getType == expt,
            s"$n = $exp failed to typecheck to $expt, got ${te.getType}"
          )
        }
    }

  def lit(i: Int): Expr[Unit] = Literal(Lit(i.toLong), ())
  def lit(b: Boolean): Expr[Unit] =
    if (b) Global(PackageName.PredefName, Identifier.Constructor("True"), ())
    else Global(PackageName.PredefName, Identifier.Constructor("False"), ())
  def let(n: String, expr: Expr[Unit], in: Expr[Unit]): Expr[Unit] =
    Let(Identifier.Name(n), expr, in, RecursionKind.NonRecursive, ())
  def lambda(arg: String, result: Expr[Unit]): Expr[Unit] =
    Lambda(NonEmptyList.one((Identifier.Name(arg), None)), result, ())
  def v(name: String): Expr[Unit] =
    Identifier.unsafe(name) match {
      case c @ Identifier.Constructor(_) => Global(testPackage, c, ())
      case b: Identifier.Bindable        => Local(b, ())
    }
  def ann(expr: Expr[Unit], t: Type): Expr[Unit] = Annotation(expr, t, ())

  def app(fn: Expr[Unit], arg: Expr[Unit]): Expr[Unit] =
    App(fn, NonEmptyList.one(arg), ())
  def alam(arg: String, tpe: Type, res: Expr[Unit]): Expr[Unit] =
    Lambda(NonEmptyList.one((Identifier.Name(arg), Some(tpe))), res, ())

  def ife(cond: Expr[Unit], ift: Expr[Unit], iff: Expr[Unit]): Expr[Unit] =
    Expr.ifExpr(cond, ift, iff, ())
  def matche(
      arg: Expr[Unit],
      branches: NonEmptyList[(Pattern[String, Type], Expr[Unit])]
  ): Expr[Unit] =
    Match(
      arg,
      branches.map { case (p, e) =>
        val p1 = p.mapName(n => (testPackage, Constructor(n)))
        (p1, e)
      },
      ()
    )

  /** Check that a no import program has a given type
    */
  def parseProgram(statement: String, tpe: String) =
    checkLast(statement) { te0 =>
      val te = te0 // TypedExprNormalization.normalize(te0).getOrElse(te0)
      te.traverseType[cats.Id] {
        case t @ Type.TyVar(Type.Var.Skolem(_, _, _, _)) =>
          fail(s"illegate skolem ($t) escape in $te")
          t
        case t @ Type.TyMeta(_) =>
          fail(s"illegate meta ($t) escape in $te")
          t
        case good =>
          good
      }
      // make sure we can render repr:
      val rendered = te.repr
      val tp = te.getType
      lazy val teStr = Type.fullyResolvedDocument.document(tp).render(80)
      assert(
        Type.freeTyVars(tp :: Nil).isEmpty,
        s"illegal inferred type: $teStr, in: $rendered"
      )

      assert(Type.metaTvs(tp :: Nil).isEmpty, s"illegal inferred type: $teStr")
      val expectedTpe = typeFrom(tpe)
      val expectedTpeStr =
        Type.fullyResolvedDocument.document(expectedTpe).render(80)
      assert(
        te.getType.sameAs(expectedTpe),
        s"$teStr != $expectedTpeStr\n\nfound: ${te.repr.render(80)}"
      )
    }

  // this could be used to test the string representation of expressions
  def checkTERepr(statement: String, repr: String) =
    checkLast(statement)(te => assert(te.repr.render(80) == repr))

  /** Test that a program is ill-typed
    */
  def parseProgramIllTyped(statement: String) = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBody(testPackage, Nil, stmts) match {
      case Ior.Left(_) | Ior.Both(_, _) => assert(true)
      case Ior.Right(program)           =>
        fail(
          "expected an invalid program, but got:\n\n" + program.lets
            .map { case (b, r, t) =>
              s"$b: $r = ${t.repr.render(80)}"
            }
            .mkString("\n\n")
        )
    }
  }

  test("assert some basic unifications") {
    assertTypesUnify("forall a. a", "forall b. b")
    assertTypesUnify("exists a. a", "exists b. b")
    assert_:<:("forall a. a", "Int")
    assert_:<:("forall a. a", "exists a. a")
    // function is contravariant in first arg test that against the above
    assert_:<:("Int -> Int", "(forall a. a) -> Int")

    assert_:<:("forall a, b. a -> b", "forall b. b -> Int")
    assert_:<:("forall a, b. a -> b", "forall a. a -> (forall b. b -> b)")

    // forall commutes with covariant types
    assertTypesUnify("forall b. Int -> b", "Int -> (forall b. b)")
    assertTypesUnify("forall a, b. a -> b", "forall a. a -> (forall b. b)")
    assertTypesUnify("forall a. List[a]", "List[forall a. a]")

    // exists a. a is a top type
    assert_:<:("Int", "exists a. a")
    assert_:<:("(exists a. a) -> Int", "exists a. (a -> Int)")
    assert_:<:("(exists a. a) -> Int", "Int -> Int")
    assertTypesUnify("(exists a. a) -> Int", "forall a. a -> Int")
    assertTypesUnify("exists a. List[a]", "List[exists a. a]")
    assertTypesUnify("exists a. (Int -> a)", "Int -> (exists a. a)")
    assertTypesUnify("(exists a. a) -> Int", "(exists a. a) -> Int")
    assert_:<:("forall a. a -> a", "(exists a. a) -> (exists a. a)")

    assert_:<:("forall a. a -> Int", "(forall a. a) -> Int")
    assertTypesUnify(
      "((forall a. a) -> Int) -> Int",
      "forall a. (a -> Int) -> Int"
    )
    assert_:<:("List[forall a. a -> Int]", "List[(forall a. a) -> Int]")

    assertTypesUnify(
      "forall f: +* -> *. f[forall a. a]",
      "forall a. forall f: +* -> *. f[a]"
    )
    assert_:<:("forall f: * -> *. f[Int]", "forall f: +* -> *. f[Int]")
    assert_:<:("forall f: * -> *. f[Int]", "forall f: -* -> *. f[Int]")
    assert_:<:("forall f: +* -> *. f[Int]", "forall f: 👻* -> *. f[Int]")
    assert_:<:("forall f: -* -> *. f[Int]", "forall f: 👻* -> *. f[Int]")
    assert_:<:(
      "forall a. forall f: * -> *. f[a]",
      "forall f: * -> *. f[forall a. a]"
    )
    assert_:<:(
      "forall a. forall f: -* -> *. f[a]",
      "forall f: -* -> *. f[forall a. a]"
    )

    assertTypesUnify("(forall a. a) -> Int", "(forall a. a) -> Int")
    assertTypesUnify(
      "(forall a. a -> Int) -> Int",
      "(forall a. a -> Int) -> Int"
    )
    assert_:<:("forall a, b. a -> b -> b", "forall a. a -> a -> a")
    assert_:<:("forall a, b. a -> b", "forall b, c. b -> (c -> Int)")
    assert_:<:("forall a, f: * -> *. f[a]", "forall x. List[x]")
    assert_:<:("forall a, f: +* -> *. f[a]", "forall x. List[x]")
    assertTypesDisjoint("forall a, f: -* -> *. f[a]", "forall x. List[x]")

    assertTypesDisjoint("Int", "String")
    assertTypesDisjoint("Int -> Unit", "String")
    assertTypesDisjoint("Int -> Unit", "String -> a")
    assertTypesUnify("forall a. Int", "Int")

    // Test unbound vars
    assertTypesDisjoint("a", "Int")
    assertTypesDisjoint("Int", "a")

    assert_:<:(
      "forall f: * -> *, a, b. (f[a], a -> f[b]) -> f[b]",
      "forall f: +* -> *, a, b. (f[a], a -> f[b]) -> f[b]"
    )
  }

  test("Basic inferences") {

    testType(lit(100), Type.IntType)
    testType(let("x", lambda("y", v("y")), lit(100)), Type.IntType)
    testType(
      lambda("y", v("y")),
      forAll(
        NonEmptyList.of(b("a")),
        Type.Fun(Type.TyVar(Bound("a")), Type.TyVar(Bound("a")))
      )
    )
    testType(
      lambda("y", lambda("z", v("y"))),
      forAll(
        NonEmptyList.of(b("a"), b("b")),
        Type.Fun(
          Type.TyVar(Bound("a")),
          Type.Fun(Type.TyVar(Bound("b")), Type.TyVar(Bound("a")))
        )
      )
    )

    testType(app(lambda("x", v("x")), lit(100)), Type.IntType)
    testType(
      ann(app(lambda("x", v("x")), lit(100)), Type.IntType),
      Type.IntType
    )
    testType(app(alam("x", Type.IntType, v("x")), lit(100)), Type.IntType)

    // test branches
    testType(ife(lit(true), lit(0), lit(1)), Type.IntType)
    testType(let("x", lit(0), ife(lit(true), v("x"), lit(1))), Type.IntType)

    val identFnType =
      forAll(
        NonEmptyList.of(b("a")),
        Type.Fun(Type.TyVar(Bound("a")), Type.TyVar(Bound("a")))
      )
    testType(
      let(
        "x",
        lambda("y", v("y")),
        ife(lit(true), v("x"), ann(lambda("x", v("x")), identFnType))
      ),
      identFnType
    )

    // test some lets
    testLetTypes(
      List(
        ("x", lit(100), Type.IntType),
        ("y", Expr.Global(testPackage, Identifier.Name("x"), ()), Type.IntType)
      )
    )
  }

  test("match inference") {
    testType(
      matche(
        lit(10),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
        )
      ),
      Type.IntType
    )

    testType(
      matche(
        lit(true),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
        )
      ),
      Type.IntType
    )

    testType(
      matche(
        lit(true),
        NonEmptyList.of(
          (Pattern.Annotation(Pattern.WildCard, Type.BoolType), lit(0))
        )
      ),
      Type.IntType
    )
  }

  object OptionTypes {
    val optName = defType("Option")
    val optType: Type.Tau = Type.TyConst(optName)

    val pn = testPackage
    val definedOption = Map(
      ((pn, Constructor("Some")), (Nil, List(Type.IntType), optName)),
      ((pn, Constructor("None")), (Nil, Nil, optName))
    )

    val definedOptionGen = Map(
      (
        (pn, Constructor("Some")),
        (
          List((Bound("a"), Kind.Type.co)),
          List(Type.TyVar(Bound("a"))),
          optName
        )
      ),
      (
        (pn, Constructor("None")),
        (List((Bound("a"), Kind.Type.co)), Nil, optName)
      )
    )
  }

  test("match with custom non-generic types") {
    import OptionTypes._

    val constructors = Map(
      (Identifier.unsafe("Some"), Type.Fun(Type.IntType, optType))
    )
    val kinds = Type.builtInKinds.updated(optName, Kind(Kind.Type.co))
    val kindNotGen = Type.builtInKinds.updated(optName, Kind.Type)

    def testWithOpt[A: HasRegion](term: Expr[A], ty: Type) =
      Infer
        .typeCheck(term)
        .runFully(
          withBools ++ asFullyQualified(constructors),
          definedOption ++ boolTypes,
          kindNotGen
        ) match {
        case Left(err)  => assert(false, err)
        case Right(tpe) => assert(tpe.getType == ty, term.toString)
      }

    def failWithOpt[A: HasRegion](term: Expr[A]) =
      Infer
        .typeCheck(term)
        .runFully(
          withBools ++ asFullyQualified(constructors),
          definedOption ++ boolTypes,
          kinds
        ) match {
        case Left(_)    => assert(true)
        case Right(tpe) =>
          assert(false, s"expected to fail, but inferred type $tpe")
      }

    testWithOpt(
      matche(
        app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
        )
      ),
      Type.IntType
    )

    testWithOpt(
      matche(
        app(v("Some"), lit(1)),
        NonEmptyList.of(
          (
            Pattern.PositionalStruct(
              "Some",
              List(Pattern.Var(Identifier.Name("a")))
            ),
            v("a")
          ),
          (Pattern.PositionalStruct("None", Nil), lit(42))
        )
      ),
      Type.IntType
    )

    failWithOpt(
      matche(
        app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Foo", List(Pattern.WildCard)), lit(0))
        )
      )
    )
  }

  test("match with custom generic types") {
    def tv(a: String): Type.Rho = Type.TyVar(Type.Var.Bound(a))

    import OptionTypes._

    val kinds = Type.builtInKinds.updated(optName, Kind(Kind.Type.co))

    val constructors = Map(
      (
        Identifier.unsafe("Some"),
        Type.forAll(
          NonEmptyList.of(b("a")),
          Type.Fun(tv("a"), Type.TyApply(optType, tv("a")))
        )
      ),
      (
        Identifier.unsafe("None"),
        Type.forAll(NonEmptyList.of(b("a")), Type.TyApply(optType, tv("a")))
      )
    )

    def testWithOpt[A: HasRegion](term: Expr[A], ty: Type) =
      Infer
        .typeCheck(term)
        .runFully(
          withBools ++ asFullyQualified(constructors),
          definedOptionGen ++ boolTypes,
          kinds
        ) match {
        case Left(err)  => assert(false, err)
        case Right(tpe) => assert(tpe.getType == ty, term.toString)
      }

    def failWithOpt[A: HasRegion](term: Expr[A]) =
      Infer
        .typeCheck(term)
        .runFully(
          withBools ++ asFullyQualified(constructors),
          definedOptionGen ++ boolTypes,
          kinds
        ) match {
        case Left(_)    => assert(true)
        case Right(tpe) =>
          assert(false, s"expected to fail, but inferred type $tpe")
      }

    testWithOpt(
      matche(
        app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
        )
      ),
      Type.IntType
    )

    testWithOpt(
      matche(
        app(v("Some"), lit(1)),
        NonEmptyList.of(
          (
            Pattern.PositionalStruct(
              "Some",
              List(Pattern.Var(Identifier.Name("a")))
            ),
            v("a")
          ),
          (Pattern.PositionalStruct("None", Nil), lit(42))
        )
      ),
      Type.IntType
    )

    // Nested Some
    testWithOpt(
      matche(
        app(v("Some"), app(v("Some"), lit(1))),
        NonEmptyList.of(
          (
            Pattern.PositionalStruct(
              "Some",
              List(Pattern.Var(Identifier.Name("a")))
            ),
            v("a")
          )
        )
      ),
      Type.TyApply(optType, Type.IntType)
    )

    failWithOpt(
      matche(
        app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Foo", List(Pattern.WildCard)), lit(0))
        )
      )
    )
  }

  test("Test a constructor with ForAll") {
    def tv(a: String): Type.Rho = Type.TyVar(Type.Var.Bound(a))

    val pureName = defType("Pure")
    val optName = defType("Option")
    val optType: Type.Tau = Type.TyConst(optName)

    val pn = testPackage

    /** struct Pure(pure: forall a. a -> f[a])
      */
    val defined = Map(
      (
        (pn, Constructor("Pure")),
        (
          List((Type.Var.Bound("f"), Kind(Kind.Type.in).in)),
          List(
            Type.forAll(
              NonEmptyList.of((Type.Var.Bound("a"), Kind.Type)),
              Type.Fun(tv("a"), Type.TyApply(tv("f"), tv("a")))
            )
          ),
          pureName
        )
      ),
      (
        (pn, Constructor("Some")),
        (List((Type.Var.Bound("a"), Kind.Type.co)), List(tv("a")), optName)
      ),
      (
        (pn, Constructor("None")),
        (List((Type.Var.Bound("a"), Kind.Type.co)), Nil, optName)
      )
    )

    val constructors = Map(
      (
        Identifier.unsafe("Pure"),
        Type.forAll(
          NonEmptyList.of(b1("f")),
          Type.Fun(
            Type.forAll(
              NonEmptyList.of(b("a")),
              Type.Fun(tv("a"), Type.TyApply(tv("f"), tv("a")))
            ),
            Type.TyApply(Type.TyConst(pureName), tv("f"))
          )
        )
      ),
      (
        Identifier.unsafe("Some"),
        Type.forAll(
          NonEmptyList.of(b("a")),
          Type.Fun(tv("a"), Type.TyApply(optType, tv("a")))
        )
      ),
      (
        Identifier.unsafe("None"),
        Type.forAll(NonEmptyList.of(b("a")), Type.TyApply(optType, tv("a")))
      )
    )

    def testWithTypes[A: HasRegion](term: Expr[A], ty: Type) =
      Infer
        .typeCheck(term)
        .runFully(
          withBools ++ asFullyQualified(constructors),
          defined ++ boolTypes,
          Type.builtInKinds.updated(optName, Kind(Kind.Type.co))
        ) match {
        case Left(err)  => assert(false, err)
        case Right(tpe) => assert(tpe.getType == ty, term.toString)
      }

    testWithTypes(
      app(v("Pure"), v("Some")),
      Type.TyApply(Type.TyConst(pureName), optType)
    )
  }

  test("test inference of basic expressions") {
    parseProgram(
      """#
main = (x -> x)(1)
""",
      "Int"
    )

    parseProgram(
      """#
x = 1
y = x
main = y
""",
      "Int"
    )
  }

  test("test inference with partial def annotation") {
    parseProgram(
      """#

ident: forall a. a -> a =
  x -> x

main = ident(1)
""",
      "Int"
    )

    parseProgram(
      """#

def ident(x: a): x

main = ident(1)
""",
      "Int"
    )

    parseProgram(
      """#

def ident(x) -> a: x

main = ident(1)
""",
      "Int"
    )

    parseProgram(
      """#

enum MyBool: T, F
struct Pair(fst, snd)

def swap_maybe(x: a, y, swap) -> Pair[a, a]:
  match swap:
    case T: Pair(y, x)
    case F: Pair(x, y)

res = (
  Pair(r, _) = swap_maybe(1, 2, F)
  r
)

main = res
""",
      "Int"
    )

    parseProgram(
      """#

struct Pair(fst: a, snd: a)

def mkPair(y, x: a):
  Pair(x, y)

fst = (
  Pair(f, _) = mkPair(1, 2)
  f
)

main = fst
""",
      "Int"
    )
  }

  test("test inference with some defined types") {
    parseProgram(
      """#
struct Unit

main = Unit
""",
      "Unit"
    )

    parseProgram(
      """#
enum Option:
  None
  Some(a)

main = Some(1)
""",
      "Option[Int]"
    )

    parseProgram(
      """#
enum Option:
  None
  Some(a)

main = Some
""",
      "forall a. a -> Option[a]"
    )

    parseProgram(
      """#
id = x -> x
main = id
""",
      "forall a. a -> a"
    )

    parseProgram(
      """#
id = x -> x
main = id(1)
""",
      "Int"
    )

    parseProgram(
      """#
enum Option:
  None
  Some(a)

x = Some(1)
main = match x:
  case None: 0
  case Some(y): y
""",
      "Int"
    )

    parseProgram(
      """#
enum List:
  Empty
  NonEmpty(a: a, tail: b)

x = NonEmpty(1, Empty)
main = match x:
  case Empty: 0
  case NonEmpty(y, _): y
""",
      "Int"
    )

    parseProgram(
      """#
enum Opt:
  None, Some(a)

struct Monad(pure: forall a. a -> f[a], bind: forall a, b. (f[a], a -> f[b]) -> f[b])

def optBind(opt, bindFn):
  match opt:
    case None: None
    case Some(a): bindFn(a)

main = Monad(Some, optBind)
""",
      "Monad[Opt]"
    )

    parseProgram(
      """#
enum Opt:
  None, Some(a)

struct Monad(pure: forall a. a -> f[a], bind: forall a, b. (f[a], a -> f[b]) -> f[b])

def opt_bind(opt, bind_fn):
  match opt:
    case None: None
    case Some(a): bind_fn(a)

option_monad = Monad(Some, opt_bind)

# todo support syntax
#def use_bind[f: * -> *](m: Monad[f], a, b, c):
def use_bind(m, a, b, c):
  Monad { pure, bind } = m
  a1 = bind(a, pure)
  b1 = bind(b, pure)
  c1 = bind(c, pure)
  a1.bind(_ -> b1.bind(_ -> c1))

main = use_bind(option_monad, None, None, None)
""",
      "forall a. Opt[a]"
    )

    // TODO:
    // The challenge here is that the naive curried form of the
    // def will not see the forall until the final parameter
    // we need to bubble up the forall on the whole function.
    //
    // same as the above with a different order in use_bind
    parseProgram(
      """#
enum Opt:
  None, Some(a)

struct Monad(pure: forall a. a -> f[a], bind: forall a, b. (f[a], a -> f[b]) -> f[b])

def opt_bind(opt, bind_fn):
  match opt:
    case None: None
    case Some(a): bind_fn(a)

option_monad = Monad(Some, opt_bind)

# TODO support
#def use_bind[f: * -> *](a, b, c, m: Monad[f]):
def use_bind(a, b, c, m):
  Monad { pure, bind } = m
  a1 = bind(a, pure)
  b1 = bind(b, pure)
  c1 = bind(c, pure)
  a1.bind(_ -> b1.bind(_ -> c1))

main = use_bind(None, None, None, option_monad)
""",
      "forall a. Opt[a]"
    )
  }

  test("test zero arg defs") {
    parseProgram(
      """#

struct Foo

fst: Foo = Foo

main = fst
""",
      "Foo"
    )

    parseProgram(
      """#

enum Foo:
  Bar, Baz(a)

fst: Foo[a] = Bar

main = fst
""",
      "forall a. Foo[a]"
    )
  }

  test("substition works correctly") {

    parseProgram(
      """#
(id: forall a. a -> a) = x -> x

struct Foo

def apply(fn, arg: Foo): fn(arg)

main = apply(id, Foo)
""",
      "Foo"
    )

    parseProgram(
      """#
(id: forall a. a -> a) = x -> x

struct Foo

(idFoo: Foo -> Foo) = id

def apply(fn, arg: Foo): fn(arg)

main = apply(id, Foo)
""",
      "Foo"
    )

    parseProgram(
      """#

struct FnWrapper(fn: a -> a)

(id: forall a. FnWrapper[a]) = FnWrapper(x -> x)

struct Foo

(idFoo: FnWrapper[Foo]) = id

def apply(fn, arg: Foo):
  FnWrapper(f) = fn
  f(arg)

main = apply(id, Foo)
""",
      "Foo"
    )

    parseProgram(
      """#
struct Foo
(id: forall a. a -> Foo) = _ -> Foo

(idFoo: Foo -> Foo) = id

(id2: Foo -> Foo) = x -> x
(idGen2: (forall a. a) -> Foo) = id2

main = Foo
""",
      "Foo"
    )

    parseProgramIllTyped("""#

struct Foo
(idFooRet: forall a. a -> Foo) = _ -> Foo

(id: forall a. a -> a) = idFooRet

main = Foo
""")

    parseProgram(
      """#
enum Foo: Bar, Baz

(bar1: forall a. (Foo -> a) -> a) = fn -> fn(Bar)
(baz1: forall a. (Foo -> a) -> a) = fn -> fn(Baz)
(bar2: forall a. (a -> Foo) -> Foo) = _ -> Bar
(baz2: forall a. (a -> Foo) -> Foo) = _ -> Baz
(bar3: ((forall a. a) -> Foo) -> Foo) = _ -> Bar
(baz3: ((forall a. a) -> Foo) -> Foo) = _ -> Baz

(bar41: (Foo -> Foo) -> Foo) = bar1
(bar42: (Foo -> Foo) -> Foo) = bar2
(baz41: (Foo -> Foo) -> Foo) = baz1
(baz42: (Foo -> Foo) -> Foo) = baz2
# since (a -> b) -> b is covariant in a, we can substitute bar3 and baz3
(baz43: (Foo -> Foo) -> Foo) = baz3
(baz43: (Foo -> Foo) -> Foo) = baz3

(producer: Foo -> (forall a. (Foo -> a) -> a)) = _ -> bar1
# in the covariant position, we can substitute
(producer1: Foo -> ((Foo -> Foo) -> Foo)) = producer

main = Bar
""",
      "Foo"
    )
    parseProgram(
      """#
enum Foo: Bar, Baz

struct Cont[b: +*, a](cont: (b -> a) -> a)

(bar1: forall a. Cont[Foo, a]) = Cont(fn -> fn(Bar))
(baz1: forall a. Cont[Foo, a]) = Cont(fn -> fn(Baz))
(bar2: forall a. Cont[a, Foo]) = Cont(_ -> Bar)
(baz2: forall a. Cont[a, Foo]) = Cont(_ -> Baz)
(bar3: Cont[forall a. a, Foo]) = Cont(_ -> Bar)
(baz3: Cont[forall a. a, Foo]) = Cont(_ -> Baz)

(bar41: Cont[Foo, Foo]) = bar1
(bar42: Cont[Foo, Foo]) = bar2
(baz41: Cont[Foo, Foo]) = baz1
(baz42: Cont[Foo, Foo]) = baz2
# Cont is covariant in a, this should be allowed
(baz43: Cont[Foo, Foo]) = baz3
(baz43: Cont[Foo, Foo]) = baz3

(producer: Foo -> (forall a. Cont[Foo, a])) = _ -> bar1
# in the covariant position, we can substitute
(producer1: Foo -> Cont[Foo, Foo]) = producer

main = Bar
""",
      "Foo"
    )

    parseProgramIllTyped("""#
enum Foo: Bar, Baz

struct Cont(cont: (b -> a) -> a)

(consumer: (forall a. Cont[Foo, a]) -> Foo) = x -> Bar
# in the contravariant position, we cannot substitute
(consumer1: Cont[Foo, Foo] -> Foo) = consumer

main = Bar
""")

    parseProgram(
      """#
struct Foo
enum Opt: Nope, Yep(a)

(producer: Foo -> forall a. Opt[a]) = _ -> Nope
# in the covariant position, we can substitute
(producer1: Foo -> Opt[Foo]) = producer
(consumer: Opt[Foo]-> Foo) = _ -> Foo
# in the contravariant position, we can generalize
(consumer1: (forall a. Opt[a]) -> Foo) = consumer

main = Foo
""",
      "Foo"
    )

    parseProgramIllTyped("""#
struct Foo
enum Opt: Nope, Yep(a)

(consumer: (forall a. Opt[a]) -> Foo) = x -> Foo
# in the contravariant position, we cannot substitute
(consumer1: Opt[Foo] -> Foo) = consumer

main = Foo
""")
    parseProgramIllTyped("""#
struct Foo
enum Opt: Nope, Yep(a)

(producer: Foo -> Opt[Foo]) = x -> Nope
# the variance forbid generalizing in this direction
(producer1: Foo -> forall a. Opt[a]) = producer

main = Foo
""")

    parseProgram(
      """#
struct Foo
enum Opt: Nope, Yep(a)

struct FnWrapper(fn: a -> b)

(producer: FnWrapper[Foo, forall a. Opt[a]]) = FnWrapper(_ -> Nope)
# in the covariant position, we can substitute
(producer1: FnWrapper[Foo, Opt[Foo]]) = producer
(consumer: FnWrapper[Opt[Foo], Foo]) = FnWrapper(_ -> Foo)
# in the contravariant position, we can generalize
(consumer1: FnWrapper[forall a. Opt[a], Foo]) = consumer

main = Foo
""",
      "Foo"
    )

    parseProgramIllTyped("""#
struct Foo
enum Opt: Nope, Yep(a)

struct FnWrapper(fn: a -> b)

(consumer: FnWrapper[forall a. Opt[a], Foo]) = FnWrapper(x -> Foo)
# in the contravariant position, we cannot substitute
(consumer1: FnWrapper[Opt[Foo], Foo]) = consumer

main = Foo
""")
    parseProgramIllTyped("""#
struct Foo
enum Opt: Nope, Yep(a)

struct FnWrapper(fn: a -> b)

(producer: FnWrapper[Foo, Opt[Foo]]) = FnWrapper(x -> Nope)
# in the covariant position, we can't generalize
(producer1: FnWrapper[Foo, forall a. Opt[a]]) = producer

main = Foo
""")

  }

  test("def with type annotation and use the types inside") {
    parseProgram(
      """#

struct Pair(fst, snd)

def fst(p: Pair[a, b]) -> a:
  Pair(f, _) = p
  f

main = fst(Pair(1, "1"))
""",
      "Int"
    )
  }

  test("test that we see some ill typed programs") {
    parseProgramIllTyped("""#

def foo(i: Int): i

main = foo("Not an Int")
""")
  }

  test("using a literal the wrong type is ill-typed") {

    parseProgramIllTyped("""#

x = "foo"

main = match x:
  case 1: "can't really be an int"
  case y: y
""")

    parseProgramIllTyped("""#

x = 1

main = match x:
  case "1": "can't really be a string"
  case y: y
""")
  }

  test("badly shaped top-level match fails to compile") {
    parseProgramIllTyped("""#

struct Foo(x)
x = 1

Foo(_) = x
main = 1
""")

    parseProgramIllTyped("""#

enum LR: L(a), R(b)

# this isn't legit: it is a non-total match
L(_) = L(1)
main = 1
""")
  }

  test("structural recursion can be typed") {
    parseProgram(
      """#

enum Nat: Zero, Succ(prev: Nat)

def len(l):
  recur l:
    case Zero: 0
    case Succ(p): len(p)

main = len(Succ(Succ(Zero)))
""",
      "Int"
    )

    parseProgram(
      """#

enum Nat: Zero, Succ(prev: Nat)

def len(l):
  def len0(l):
    recur l:
      case Zero: 0
      case Succ(p): len0(p)
  len0(l)

main = len(Succ(Succ(Zero)))
""",
      "Int"
    )
  }

  test("nested def example") {

    parseProgram(
      """#
struct Pair(first, second)

def bar(x):
  def baz(y):
    Pair(x, y)

  baz(10)

main = bar(5)
""",
      "Pair[Int, Int]"
    )
  }

  test("test checkRho on annotated lambda") {

    parseProgram(
      """#
struct Foo
struct Bar

(fn: forall a. a -> Bar) = _ -> Bar
#(fn: Bar -> Bar) = x -> Bar

#dontCall = \(fn: forall a. a -> Bar) -> Foo
#dontCall = \(fn: Bar -> Bar) -> Foo
dontCall = \(_: (forall a. a) -> Bar) -> Foo

(main: Foo) = dontCall(fn)
""",
      "Foo"
    )
  }
  test("ForAll as function arg") {
    parseProgram(
      """#
struct Wrap[bbbb](y1: bbbb)
struct Foo[cccc](y2: cccc)
struct Nil

# TODO: These variants don't work, but the one with a fully
# ascribed type does. There is a problem here with using subsCheck
# which can never substitute a metavariable for a sigma type (outer forall)
#Wrap(_: ((forall x. Foo[x]) -> Nil)) = cra_fn
#def foo(cra_fn: Wrap[(forall ssss. ssss) -> Nil]):
# Wrap(_: ((forall x. x) -> Nil)) = cra_fn
#Nil
def foo(cra_fn: Wrap[(forall ssss. Foo[ssss]) -> Nil]):
  match cra_fn:
    case (_: Wrap[(forall x. Foo[x]) -> Nil]): Nil
main = foo
""",
      "Wrap[(forall ssss. Foo[ssss]) -> Nil] -> Nil"
    )
  }

  test("use a type annotation inside a def") {
    parseProgram(
      """#
struct Foo
struct Bar
def ignore(_): Foo
def add(x):
  (y: Foo) = x
  _ = ignore(y)
  Bar
""",
      "Foo -> Bar"
    )

    parseProgram(
      """#
struct Foo
struct Bar(f: Foo)
def ignore(_): Foo
def add(x):
  ((y: Foo) as b) = x
  _ = ignore(y)
  Bar(b)
""",
      "Foo -> Bar"
    )
  }

  test("top level matches don't introduce colliding bindings") {
    parseProgramIllTyped("""#
struct Pair(fst, snd)

Pair(a, b) = Pair(1, 2)
d = c
""")
  }

  test("check that annotations work") {
    parseProgramIllTyped("""#
struct Foo
struct Bar

x = (
  f = Foo
  f: Bar
)
""")

    parseProgramIllTyped("""#
struct Foo
struct Bar

x = (
  f: Bar = Foo
  f
)
""")
    parseProgramIllTyped("""#
struct Pair(a, b)
struct Foo
struct Bar

x = (
  Pair(f: Bar, g) = Pair(Foo, Foo)
  f
)
""")

    parseProgramIllTyped("""#
struct Pair(a, b)
struct Foo
struct Bar

x = (
  Pair(f, g) = Pair(Foo: Bar, Foo)
  f
)
""")

    parseProgramIllTyped("""#
struct Foo
struct Bar

x: Bar = Foo
""")

    parseProgram(
      """#
struct Foo
struct Bar

x = (
  f = Foo
  f: Foo
)
""",
      "Foo"
    )

    parseProgram(
      """#
struct Foo
struct Bar

x = (
  f: Foo = Foo
  f
)
""",
      "Foo"
    )
    parseProgram(
      """#
struct Pair(a, b)
struct Foo

def ignore(_): Foo

x = (
  Pair(f: Foo, g) = Pair(Foo, Foo)
  _ = ignore(g)
  f
)
""",
      "Foo"
    )

    parseProgram(
      """#
struct Pair(a, b)
struct Foo

x = (
  Pair(f, _) = Pair(Foo: Foo, Foo)
  f
)
""",
      "Foo"
    )

    parseProgram(
      """#
struct Foo

x: Foo = Foo
""",
      "Foo"
    )
  }

  test("test inner quantification") {
    parseProgram(
      """#
struct Foo

# this should just be: type Foo
foo = (
  # TODO, we would like this test to pass with
  # the annotations below
  #def ident(x: a) -> a: x
  def ident(x): x
  ident(Foo)
)

""",
      "Foo"
    )
  }

  test("widening inside a match") {
    parseProgram(
      """#
enum B: True, False

def not(b):
  match b:
    case True: False
    case False: True

def branch(x):
  match x:
    case True: (x -> x): forall a. a -> a    
    case False: i -> not(i)

res = branch(True)(True)
""",
      "B"
    )

    parseProgramIllTyped("""#
enum B: True, False

def not(b):
  match b:
    case True: False
    case False: True

def branch[a](x: B) -> (a -> a):
  match x:
    case True: (x -> x): forall a. a -> a    
    case False: i -> not(i)

res = branch(True)(True)
""")
  }

  test("basic existential types") {
    parseProgram(
      """#
x: exists b. b = 1
""",
      "exists b. b"
    )

    parseProgram(
      """#
def hide[b](x: b) -> exists a. a: x
""",
      "forall a. a -> (exists a. a)"
    )

    parseProgram(
      """#
def hide[b](x: b) -> exists a. a: x
x = hide(1)
""",
      "exists a. a"
    )

    parseProgram(
      """#
def hide[b](x: b) -> exists a. a: x
y: exists x. x = 1
x = hide(y)
""",
      "exists a. a"
    )

    parseProgram(
      """#
def hide[b](x: b) -> exists a. a: x
y = hide(1)
x = hide(y)
""",
      "exists a. a"
    )

    parseProgram(
      """#
struct Tup(a, b)

def hide[b](x: b) -> exists a. a: x
x = hide(1)
y = hide("1")
z: Tup[exists a. a, exists b. b] = Tup(x, y)
""",
      "Tup[exists a. a, exists b. b]"
    )
    parseProgram(
      """#
struct Tup(a, b)

def hide[b](x: b) -> exists a. a: x
def makeTup[a, b](x: a, y: b) -> Tup[a, b]: Tup(x, y)
x = hide(1)
y = hide("1")
z: Tup[exists a. a, exists b. b] = makeTup(x, y)
""",
      "Tup[exists a. a, exists b. b]"
    )
    parseProgram(
      """#
enum B: T, F

struct Inv[a: *](item: a)

any: exists a. a = T
x: exists a. Inv[a] = Inv(any)
""",
      // TODO: it would be nice to be able to annotate this as
      // Inv[exists a. a] and get that to pass too
      // even though, I think exists a. Inv[a] is a tighter type
      //
      "exists a. Inv[a]"
    )
  }

  test("we can use existentials in branches") {
    parseProgram(
      """#
enum MyBool: T, F

def branch(b) -> exists a. a:
  match b:
    case T: 1
    case F: "1"

x = branch(T)
""",
      "exists a. a"
    )

    parseProgram(
      """#
enum Maybe: Nothing, Something(item: exists a. a)
enum Opt[a]: None, Some(a: a)

x = Something(1: exists a. a)

def branch(b: Maybe) -> exists a. Opt[a]:
  match b:
    case Something(x): Some(x)
    case Nothing: None

x = branch(x)
""",
      "exists a. Opt[a]"
    )

    parseProgram(
      """#
struct MyTup(a, b)
enum MyBool: T, F

b = T

x = MyTup((match b:
  case T: F
  case F: T), (x: MyBool) -> x): exists a. MyTup[a, a -> MyBool]
""",
      "exists a. MyTup[a, a -> MyBool]"
    )

    parseProgramIllTyped("""#
struct MyTup(a, b)
enum MyBool: T, F

b = T

x = MyTup((match b:
  case T: F
  case F: 1), (x: MyBool) -> x): exists a. MyTup[a, a -> MyBool]
""")
  }

  test("use existentials in ADTs") {
    parseProgram(
      """#
struct Tup(a, b)
enum FreeF[a]:
  Pure(a: a)
  Mapped(tup: exists b. Tup[FreeF[b], b -> a])

enum Opt[a]: None, Some(a: a)

# this seems to work
n: exists b. Opt[Tup[FreeF[b], b -> a]] = None

def branch[a](b: FreeF[a]) -> exists b. Opt[Tup[FreeF[b], b -> a]]:
  match b:
    case Mapped(x): Some(x)
    case _: None

""",
      "forall a. FreeF[a] -> exists b. Opt[Tup[FreeF[b], b -> a]]"
    )

  }

  test("we can use existentials to delay calls") {
    parseProgram(
      """#
struct MyTup(a, b)

def delay[a, b](fn: a -> b, a: a) -> exists c. MyTup[c -> b, c]:
  MyTup(fn, a)

def call[a](tup: exists c. MyTup[c -> a, c]) -> a:
  MyTup(fn, arg) = tup
  fn(arg)

x = call(delay(x -> x, 1))
""",
      "Int"
    )
  }

  test("we can't see through existentials") {
    parseProgramIllTyped("""#
enum MyBool: T, F

b: exists a. a = T
c: MyBool = b
""")
  }

  test("pattern instantiation doesn't violate kinds") {
    parseProgramIllTyped("""#
struct B[f: * -> *]
struct C[f: +* -> *]

def foo[f: * -> *](b: B[f]) -> C[f]:
  match b:
    case B: C
x = 1
""")
  }

  test("rule out unsound kind operations") {
    parseProgramIllTyped("""#
def cast[f: 👻* -> *, a, b](in: f[a]) -> f[b]: in

struct Box(item)
struct Foo
struct Bar

x = Box(Foo)
y: Box[Bar] = cast(x)
""")
    parseProgramIllTyped("""#
def widen[f: +* -> *](in: f[forall a. a]) -> forall a. f[a]: in

enum B: T, F

struct Contra[a](fn: a -> B)

c: Contra[forall a. a] = Contra(nothing -> nothing)

# this is unsound
d: forall a. Contra[a] = widen(c) 
""")
    parseProgramIllTyped("""#
def narrow[f: -* -> *](in: f[exists a. a]) -> forall a. f[a]: in

enum B: T, F

struct Co[a](item: a)

x: Co[exists a. a] = Co(T)

# this is unsound
y: forall a. Co[a] = narrow(x) 
""")

    parseProgramIllTyped("""#
struct Pair(a, b)

def narrow(
  in: exists f: -* -> *. Pair[f[exists a. a],
  forall c. f[c] -> c]) -> forall a. exists f: -* -> *. Pair[f[a], forall c. f[c] -> c]: in

def unsound[f: * -> *](fany: f[exists a. a], get: forall a. f[a] -> a) -> forall a. a:
  Pair(fa, ex) = narrow(Pair(fany, get))
  ex(fa)
""")
  }

  test("we can use existentials with invariant types") {
    parseProgram(
      """#
struct Box(a)

x: exists a. a = 1

fn: (exists a. a) -> Box[exists a. a] = Box
y = fn(x)
""",
      "Box[exists a. a]"
    )

    parseProgram(
      """#
struct Box(a)

x: exists a. a = 1

y: Box[exists a. a] = Box(x)
""",
      "Box[exists a. a]"
    )
  }

  test("invariant instantiation regression") {
    parseProgram(
      """#
struct Box[x: *](a: x)
struct One
enum Opt[a]: None, Some(a: a)

# we could infer this as forall a. Box[Opt[a]]
#   or: Box[forall a. Opt[a]]
#   if we infer Box[forall a. Opt[a]], then we need to see that
#   Box[forall a. Opt[a]] <:< Box[Opt[One]]
#   but that's not true in general for an invariant type is it?
# consider: struct C[a](fn: a -> List[a])
# C[forall a. a] is like (forall a. a) -> List[forall a. a]
# but forall a. C[a] is forall a. (a -> List[a])
# the first one could be x -> x, but that's not a valid member of the second
# so, C[forall a. a] can't be <:< forall a. C[a]
# but possibly forall a. C[a] <:< C[forall a. a]
y: forall a. Box[Opt[a]] = Box(None)
def process(o: Box[Opt[One]]) -> One: 
  match o:
    case Box(Some(o)): o
    case Box(None): One

z = process(y)
""",
      "One"
    )
  }

  test("some subtyping relationships") {
    parseProgram(
      """
struct Foo[a: *]

f1: forall a. Foo[a] = Foo
f2: Foo[forall a. a] = Foo

f3: Foo[forall a. a] = f1
""",
      "Foo[forall a. a]"
    )
  }

  test("test Liskov example") {
    parseProgram(
      """
struct Sub[a: -*, b: +*](sub: forall f: +* -> *. f[a] -> f[b])
struct Tup(a, b, c, d)
struct Foo

refl_sub: forall a. Sub[a, a] = Sub(x -> x) 
refl_bottom: forall b. Sub[forall a. a, b] = refl_sub
refl_bottom1: Sub[forall a. a, forall a. a] = refl_sub
refl_Foo: Sub[forall a. a, Foo] = refl_sub
refl_any: Sub[forall a. a, exists a. a] = refl_sub
refl_any1: Sub[exists a. a, exists a. a] = refl_sub
refl_Foo_any: Sub[Foo, exists a. a] = refl_sub

ignore = Tup(refl_bottom, refl_bottom1, refl_Foo, refl_any)
""",
      "forall a. Tup[Sub[forall a. a, a], Sub[forall a. a, forall a. a]," +
        "Sub[forall a. a, Foo], Sub[forall a. a, exists a. a]]"
    )

    parseProgram(
      """
struct Sub[a: -*, b: +*](sub: forall f: +* -> *. f[a] -> f[b])
struct Tup(a, b, c, d)
struct Foo

refl_sub: forall a. Sub[a, a] = Sub(x -> x) 
refl_bottom: forall b. Sub[forall a. a, b] = refl_sub
refl_bottom1: Sub[forall a. a, forall a. a] = refl_sub
refl_Foo: Sub[forall a. a, Foo] = refl_sub
refl_any: Sub[forall a. a, exists a. a] = refl_sub
refl_any1: Sub[exists a. a, exists a. a] = refl_sub
refl_Foo_any: Sub[Foo, exists a. a] = refl_sub

ignore: exists a. a = Tup(refl_bottom, refl_bottom1, refl_Foo, refl_any)
""",
      "exists a. a"
    )
  }

  test("test external def with kinds") {
    parseProgram(
      """
struct Foo
external def foo[f: * -> *](f: f[Foo]) -> Foo

struct Box[a](item: a)

f = foo(Box(Foo))
    """,
      "Foo"
    )
  }

  test("ill kinded external defs are not allowed") {
    parseProgramIllTyped("""#
struct Foo
external def foo[f: * -> *](function: f) -> f

f = Foo
""")

    parseProgramIllTyped("""#
struct Box[a](item: a)
external foo: Box

struct Foo
f = Foo
""")
  }

  test("identity function with existential") {
    parseProgram(
      """
struct Prog[a: -*, e: +*, b: +*]

def pass_thru(f: Prog[exists a. a, e, b]) -> Prog[exists a. a, e, b]:
  f
    """,
      "forall e, b. Prog[exists a. a, e, b] -> Prog[exists a. a, e, b]"
    )

    parseProgram(
      """
struct Foo
struct Prog[a: -*, e: +*, b: +*]

def pass_thru(f: Prog[exists a. a, Foo, Foo]) -> Prog[exists a. a, Foo, Foo]:
  f
    """,
      "Prog[exists a. a, Foo, Foo] -> Prog[exists a. a, Foo, Foo]"
    )
  }
}
