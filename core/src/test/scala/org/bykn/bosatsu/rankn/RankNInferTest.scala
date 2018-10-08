package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.scalatest.FunSuite
import org.bykn.bosatsu.{Expr, Lit, PackageName, Package, Pattern, TypeRef, ConstructorName, Statement}

import fastparse.all.Parsed

import Expr._
import Type.Var.Bound
import Type.ForAll

class RankNInferTest extends FunSuite {

  def typeFrom(str: String): Type =
    TypeRef.parser.parse(str) match {
      case Parsed.Success(typeRef, _) =>
        typeRef.toNType {
          case "Integer" => Type.Const.predef("Integer")
          case "String" => Type.Const.predef("String")
          case s =>
            Type.Const.Defined(PackageName.parts("Test"), s)
        }
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
    }

  def runUnify(left: String, right: String) = {
    val t1 = typeFrom(left)
    val t2 = typeFrom(right)

    Infer.substitutionCheck(t1, t2)
      .runFully(Map.empty, Map.empty)
  }

  def assertTypesUnify(left: String, right: String) =
    assert(runUnify(left, right).isRight, s"$left does not unify with $right")

  def assertTypesDisjoint(left: String, right: String) =
    assert(runUnify(left, right).isLeft, s"$left unexpectedly unifies with $right")

  def defType(n: String): Type.Const.Defined =
    Type.Const.Defined(PackageName.parts("Test"), n)

  val withBools: Map[String, Type] =
    Map(
      "True" -> Type.BoolType,
      "False" -> Type.BoolType)

  def testType(term: Expr[_], ty: Type) =
    Infer.typeCheck(term).runFully(withBools, Map.empty) match {
      case Left(err) => assert(false, err)
      case Right(tpe) => assert(tpe.getType == ty, term.toString)
    }

  def testLetTypes[A](terms: List[(String, Expr[A], Type)]) =
    Infer.typeCheckLets(terms.map { case (k, v, _) => (k, v) })
      .runFully(withBools, Map.empty) match {
        case Left(err) => assert(false, err)
        case Right(tpes) =>
          assert(tpes.size == terms.size)
          terms.zip(tpes).foreach { case ((n, exp, expt), (n1, te)) =>
            assert(n == n1, s"the name changed: $n != $n1")
            assert(te.getType == expt, s"$n = $exp failed to typecheck to $expt, got ${te.getType}")
          }
      }

  def lit(i: Int): Expr[Unit] = Literal(Lit(i), ())
  def lit(b: Boolean): Expr[Unit] = if (b) Var("True", ()) else Var("False", ())
  def let(n: String, expr: Expr[Unit], in: Expr[Unit]): Expr[Unit] = Let(n, expr, in, ())
  def lambda(arg: String, result: Expr[Unit]): Expr[Unit] = Lambda(arg, result, ())
  def v(name: String): Expr[Unit] = Var(name, ())
  def ann(expr: Expr[Unit], t: Type): Expr[Unit] = Annotation(expr, t, ())

  def app(fn: Expr[Unit], arg: Expr[Unit]): Expr[Unit] = App(fn, arg, ())
  def alam(arg: String, tpe: Type, res: Expr[Unit]): Expr[Unit] = AnnotatedLambda(arg, tpe, res, ())

  def ife(cond: Expr[Unit], ift: Expr[Unit], iff: Expr[Unit]): Expr[Unit] = If(cond, ift, iff, ())
  def matche(arg: Expr[Unit], branches: NonEmptyList[(Pattern[String, Type], Expr[Unit])]): Expr[Unit] =
    Match(arg,
      branches.map { case (p, e) =>
        val p1 = p.mapName { n => (PackageName.parts("Test"), ConstructorName(n)) }
        (p1, e)
      },
      ())

  /**
   * Check that a no import program has a given type
   */
  def parseProgram(statement: String, tpe: String) =
    Statement.parser.parse(statement) match {
      case Parsed.Success(stmt, _) =>
        Package.inferBody(PackageName.parts("Test"), Nil, stmt) match {
          case Left(err) => fail(err.message)
          case Right((tpeEnv, lets)) =>
            val parsedType = typeFrom(tpe)
            assert(lets.last._2.getType == parsedType)
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $statement: $exp at $idx with trace: ${extra.traced.trace}")
    }

  test("assert some basic unifications") {
    assertTypesUnify("forall a. a", "forall b. b")
    assertTypesUnify("forall a. a", "Int")
    assertTypesUnify("forall a, b. a -> b", "forall b. b -> Int")
    assertTypesUnify("forall a, b. a -> b", "forall b, c. b -> (c -> Int)")
    // assertTypesUnify("(forall a. a)[Int]", "Int")
    // assertTypesUnify("(forall a. Int)[b]", "Int")
    assertTypesUnify("forall a, f. f[a]", "forall x. List[x]")
    //assertTypesUnify("(forall a, b. a -> b)[x, y]", "z -> w")

    assertTypesDisjoint("Int", "String")
    assertTypesDisjoint("Int -> Unit", "String")
    assertTypesDisjoint("Int -> Unit", "String -> a")
    //assertTypesDisjoint("forall a. Int", "Int") // the type on the left has * -> * but the right is *
  }

  test("Basic inferences") {

    testType(lit(100), Type.IntType)
    testType(let("x", lambda("y", v("y")), lit(100)), Type.IntType)
    testType(lambda("y", v("y")),
      ForAll(NonEmptyList.of(Bound("a")),
        Type.Fun(Type.TyVar(Bound("a")),Type.TyVar(Bound("a")))))
    testType(lambda("y", lambda("z", v("y"))),
      ForAll(NonEmptyList.of(Bound("a"), Bound("b")),
        Type.Fun(Type.TyVar(Bound("a")),
          Type.Fun(Type.TyVar(Bound("b")),Type.TyVar(Bound("a"))))))

    testType(app(lambda("x", v("x")), lit(100)), Type.IntType)
    testType(ann(app(lambda("x", v("x")), lit(100)), Type.IntType), Type.IntType)
    testType(app(alam("x", Type.IntType, v("x")), lit(100)), Type.IntType)

    // test branches
    testType(ife(lit(true), lit(0), lit(1)), Type.IntType)
    testType(let("x", lit(0), ife(lit(true), v("x"), lit(1))), Type.IntType)

    val identFnType =
      ForAll(NonEmptyList.of(Bound("a")),
        Type.Fun(Type.TyVar(Bound("a")), Type.TyVar(Bound("a"))))
    testType(let("x", lambda("y", v("y")),
      ife(lit(true), v("x"),
        ann(lambda("x", v("x")), identFnType))), identFnType)

    // test some lets
    testLetTypes(
      List(
        ("x", lit(100), Type.IntType),
        ("y", v("x"), Type.IntType)))
  }

  test("match inference") {
    testType(
      matche(lit(10),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
          )), Type.IntType)

    testType(
      matche(lit(true),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
          )), Type.IntType)

    testType(
      matche(lit(true),
        NonEmptyList.of(
          (Pattern.Annotation(Pattern.WildCard, Type.BoolType), lit(0))
          )), Type.IntType)
  }

  object OptionTypes {
    val optName = defType("Option")
    val optType: Type.Tau = Type.TyConst(optName)

    val pn = PackageName.parts("Test")
    val definedOption = Map(
      ((pn, ConstructorName("Some")), (Nil, List(Type.IntType), optName)),
      ((pn, ConstructorName("None")), (Nil, Nil, optName)))

    val definedOptionGen = Map(
      ((pn, ConstructorName("Some")), (List(Bound("a")), List(Type.TyVar(Bound("a"))), optName)),
      ((pn, ConstructorName("None")), (List(Bound("a")), Nil, optName)))
  }

  test("match with custom non-generic types") {
    def b(a: String): Type.Var.Bound = Type.Var.Bound(a)
    def tv(a: String): Type = Type.TyVar(b(a))

    import OptionTypes._

    val constructors = Map(
      ("Some", Type.Fun(Type.IntType, optType))
    )

    def testWithOpt(term: Expr[_], ty: Type) =
      Infer.typeCheck(term).runFully(withBools ++ constructors, definedOption) match {
        case Left(err) => assert(false, err)
        case Right(tpe) => assert(tpe.getType == ty, term.toString)
      }

    def failWithOpt(term: Expr[_]) =
      Infer.typeCheck(term).runFully(withBools ++ constructors, definedOption) match {
        case Left(err) => assert(true)
        case Right(tpe) => assert(false, s"expected to fail, but inferred type $tpe")
      }

    testWithOpt(
      matche(app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
          )), Type.IntType)

    testWithOpt(
      matche(app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Some", List(Pattern.Var("a"))), v("a")),
          (Pattern.PositionalStruct("None", Nil), lit(42))
          )), Type.IntType)

    failWithOpt(
      matche(app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Foo", List(Pattern.WildCard)), lit(0))
          )))
  }

  test("match with custom generic types") {
    def b(a: String): Type.Var.Bound = Type.Var.Bound(a)
    def tv(a: String): Type = Type.TyVar(b(a))

    import OptionTypes._

    val constructors = Map(
      ("Some", Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(tv("a"), Type.TyApply(optType, tv("a"))))),
      ("None", Type.ForAll(NonEmptyList.of(b("a")), Type.TyApply(optType, tv("a"))))
    )

    def testWithOpt(term: Expr[_], ty: Type) =
      Infer.typeCheck(term).runFully(withBools ++ constructors, definedOptionGen) match {
        case Left(err) => assert(false, err)
        case Right(tpe) => assert(tpe.getType == ty, term.toString)
      }

    def failWithOpt(term: Expr[_]) =
      Infer.typeCheck(term).runFully(withBools ++ constructors, definedOptionGen) match {
        case Left(err) => assert(true)
        case Right(tpe) => assert(false, s"expected to fail, but inferred type $tpe")
      }

    testWithOpt(
      matche(app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.WildCard, lit(0))
          )), Type.IntType)

    testWithOpt(
      matche(app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Some", List(Pattern.Var("a"))), v("a")),
          (Pattern.PositionalStruct("None", Nil), lit(42))
          )), Type.IntType)

    // Nested Some
    testWithOpt(
      matche(app(v("Some"), app(v("Some"), lit(1))),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Some", List(Pattern.Var("a"))), v("a"))
          )), Type.TyApply(optType, Type.IntType))

    failWithOpt(
      matche(app(v("Some"), lit(1)),
        NonEmptyList.of(
          (Pattern.PositionalStruct("Foo", List(Pattern.WildCard)), lit(0))
          )))
  }

  test("Test a constructor with ForAll") {
    def b(a: String): Type.Var.Bound = Type.Var.Bound(a)
    def tv(a: String): Type = Type.TyVar(b(a))

    val pureName = defType("Pure")
    val pureType: Type.Tau = Type.TyConst(pureName)
    val optName = defType("Option")
    val optType: Type.Tau = Type.TyConst(optName)

    val pn = PackageName.parts("Test")
    /**
     * struct Pure(pure: forall a. a -> f[a])
     */
    val defined = Map(
      ((pn, ConstructorName("Pure")), (List(b("f")),
        List(Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(tv("a"), Type.TyApply(tv("f"), tv("a"))))),
        pureName)),
      ((pn, ConstructorName("Some")), (List(b("a")), List(tv("a")), optName)),
      ((pn, ConstructorName("None")), (List(b("a")), Nil, optName)))

    val constructors = Map(
      ("Pure", Type.ForAll(NonEmptyList.of(b("f")),
        Type.Fun(Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(tv("a"), Type.TyApply(tv("f"), tv("a")))),
          Type.TyApply(Type.TyConst(pureName), tv("f")) ))),
      ("Some", Type.ForAll(NonEmptyList.of(b("a")), Type.Fun(tv("a"), Type.TyApply(optType, tv("a"))))),
      ("None", Type.ForAll(NonEmptyList.of(b("a")), Type.TyApply(optType, tv("a"))))
    )

    def testWithTypes(term: Expr[_], ty: Type) =
      Infer.typeCheck(term).runFully(withBools ++ constructors, defined) match {
        case Left(err) => assert(false, err)
        case Right(tpe) => assert(tpe.getType == ty, term.toString)
      }

    testWithTypes(
      app(v("Pure"), v("Some")), Type.TyApply(Type.TyConst(pureName), optType))
  }

  test("test all binders") {
    assert(Type.allBinders.filter(_.name.startsWith("a")).take(100).map(_.name) ==
      ("a" #:: Stream.iterate(0)(_ + 1).map { i => s"a$i" }).take(100))
  }

  test("test inference with some defined types") {
    parseProgram("""#
struct Unit

main = Unit
""", "Unit")

    parseProgram("""#
enum Option:
  None
  Some(a)

main = Some(1)
""", "Option[Integer]")

    parseProgram("""#
enum Option:
  None
  Some(a)

main = Some
""", "forall a. a -> Option[a]")

   parseProgram("""#
enum Option:
  None
  Some(a)

x = Some(1)
main = match x:
  None:
    0
  Some(y):
    y
""", "Integer")
  }
  // TODO this does not unify with rankn types
   // parseProgram("""#
// enum List:
  // Empty
  // NonEmpty(a: a, tail: List[a])

// x = NonEmpty(1, Empty)
// main = match x:
  // Empty:
   //  0
  // NonEmpty(y, z):
   //  y
// """, "Integer")
  // }
}
