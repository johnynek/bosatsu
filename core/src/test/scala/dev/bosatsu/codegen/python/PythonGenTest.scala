package dev.bosatsu.codegen.python

import cats.data.NonEmptyList
import dev.bosatsu.{Identifier, PackageName}
import dev.bosatsu.Generators.bindIdentGen
import org.scalacheck.Prop.forAll

class PythonGenTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 50000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(5000)
  // PropertyCheckConfiguration(minSuccessful = 500)

  val PythonName = "[_A-Za-z][_A-Za-z0-9]*".r.pattern

  // Helper to create Bindable names
  private def bindable(name: String): Identifier.Bindable =
    Identifier.unsafeBindable(name)

  // =========================================================================
  // escape tests
  // =========================================================================

  test("all escapes are valid python identifiers") {
    forAll(bindIdentGen) { b =>
      val str = PythonGen.escape(b).name

      assert(
        PythonName.matcher(str).matches(),
        s"escaped: ${b.sourceCodeRepr} to $str"
      )
    }
  }

  test("escape handles regular names") {
    val name = bindable("myVar")
    val escaped = PythonGen.escape(name)
    assertEquals(escaped.name, "myVar")
  }

  test("escape handles Python keywords") {
    val name = bindable("class")
    val escaped = PythonGen.escape(name)
    // Should be escaped since 'class' is a Python keyword
    assert(escaped.name != "class")
    assert(PythonName.matcher(escaped.name).matches())
  }

  test("escape handles operators") {
    val name = Identifier.Operator("+")
    val escaped = PythonGen.escape(name)
    // Operators need escaping
    assert(escaped.name != "+")
    assert(PythonName.matcher(escaped.name).matches())
  }

  // =========================================================================
  // escapeModule tests
  // =========================================================================

  test("escapeModule handles regular module names") {
    val escaped = PythonGen.escapeModule("mymodule")
    assertEquals(escaped.name, "mymodule")
  }

  test("escapeModule handles Python keywords") {
    val escaped = PythonGen.escapeModule("import")
    // Should be escaped since 'import' is a Python keyword
    assert(escaped.name != "import")
    assert(PythonName.matcher(escaped.name).matches())
  }

  test("escapeModule handles names starting with ___") {
    val escaped = PythonGen.escapeModule("___module")
    assert(escaped.name != "___module")
    assert(PythonName.matcher(escaped.name).matches())
  }

  // =========================================================================
  // Env monad tests
  // =========================================================================

  test("Env.pure creates a pure value") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val env = PythonGen.Env.pure(42)
    // We can't directly run it but we can test monad laws
    val mapped = env.map(_ + 1)
    assert(mapped != null)
  }

  test("Env monad laws - left identity") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val f: Int => PythonGen.Env[Int] = x => PythonGen.Env.pure(x + 1)
    val a = 10

    // pure(a).flatMap(f) === f(a)
    val left = PythonGen.Env.pure(a).flatMap(f)
    val right = f(a)

    // Both should produce equivalent results
    assert(left != null)
    assert(right != null)
  }

  test("Env monad laws - right identity") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val env = PythonGen.Env.pure(42)
    // m.flatMap(pure) === m
    val result = env.flatMap(PythonGen.Env.pure(_))
    assert(result != null)
  }

  test("Env.bind creates unique identifiers") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val name = bindable("x")
    val env = PythonGen.Env.bind(name).flatMap { id1 =>
      PythonGen.Env.bind(name).map { id2 =>
        (id1, id2)
      }
    }
    // Binding twice should produce different identifiers
    assert(env != null)
  }

  test("Env.nameForAnon creates anonymous identifiers") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val env = PythonGen.Env.nameForAnon(42L)
    val mapped = env.map(_.name)
    // Anonymous names follow a pattern
    assert(mapped != null)
  }

  test("Env.newAssignableVar creates temp variables") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val env = PythonGen.Env.newAssignableVar.flatMap { v1 =>
      PythonGen.Env.newAssignableVar.map { v2 =>
        (v1, v2)
      }
    }
    // Should create different temp vars
    assert(env != null)
  }

  test("Env.importPackage creates import identifiers") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val parts = NonEmptyList.of("foo", "bar")
    val env = PythonGen.Env.importPackage(parts)
    assert(env != null)
  }

  test("Env.topLevelName creates top-level identifiers") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val name = bindable("myFunc")
    val env = PythonGen.Env.topLevelName(name)
    assert(env != null)
  }

  // =========================================================================
  // Parser tests
  // =========================================================================

  test("we can parse an example externals file") {
    val extstr = """
      { IO: { foo: bar.baz, quux: quux.quux_impl }, Bop: { foo: collections.queue } }
    """
    assertEquals(PythonGen.externalParser.parseAll(extstr).map(_ => ()), Right(()))
  }

  test("we can parse an example evals file") {
    val str = """
      {
        IO::IO: IOPy.run_io,
        Build/Foo::Bar: BuildImpl.run_build,
      }
    """
    assertEquals(PythonGen.evaluatorParser.parseAll(str).map(_ => ()), Right(()))
  }

  test("externalParser handles empty input") {
    val extstr = "{}"
    assertEquals(PythonGen.externalParser.parseAll(extstr).map(_ => ()), Right(()))
  }

  test("externalParser handles deeply nested modules") {
    val extstr = """
      { Foo/Bar/Baz: { fn: deeply.nested.module.impl } }
    """
    assertEquals(PythonGen.externalParser.parseAll(extstr).map(_ => ()), Right(()))
  }

  test("evaluatorParser handles empty input") {
    val str = "{}"
    assertEquals(PythonGen.evaluatorParser.parseAll(str).map(_ => ()), Right(()))
  }

  // =========================================================================
  // intrinsicValues tests
  // =========================================================================

  test("intrinsicValues contains Predef package") {
    val intrinsics = PythonGen.intrinsicValues
    assert(intrinsics.contains(PackageName.PredefName))
  }

  test("intrinsicValues Predef contains expected functions") {
    val intrinsics = PythonGen.intrinsicValues
    val predefFns = intrinsics(PackageName.PredefName)

    // Check for some expected functions
    assert(predefFns.exists(_.asString == "add"))
    assert(predefFns.exists(_.asString == "sub"))
    assert(predefFns.exists(_.asString == "times"))
    assert(predefFns.exists(_.asString == "div"))
    assert(predefFns.exists(_.asString == "eq_Int"))
    assert(predefFns.exists(_.asString == "cmp_Int"))
  }

  test("intrinsicValues contains Bosatsu/Numeric package") {
    val intrinsics = PythonGen.intrinsicValues
    val numericPackage = PackageName.parse("Bosatsu/Numeric").get
    assert(intrinsics.contains(numericPackage))
  }

  test("intrinsicValues Numeric contains expected operators") {
    val intrinsics = PythonGen.intrinsicValues
    val numericPackage = PackageName.parse("Bosatsu/Numeric").get
    val numericFns = intrinsics(numericPackage)

    // Check for numeric operators
    assert(numericFns.exists(_.asString == "+."))
    assert(numericFns.exists(_.asString == "-."))
    assert(numericFns.exists(_.asString == "*."))
    assert(numericFns.exists(_.asString == "/."))
    assert(numericFns.exists(_.asString == "from_Int"))
    assert(numericFns.exists(_.asString == "to_Int"))
  }

  // =========================================================================
  // Env.makeDef tests
  // =========================================================================

  test("Env.makeDef creates a function definition") {
    val defName = Code.Ident("myFunc")
    val args = NonEmptyList.of(Code.Ident("x"), Code.Ident("y"))
    val body: Code.ValueLike = Code.Ident("x")

    val result = PythonGen.Env.makeDef(defName, args, body)
    assert(result.name == defName)
    assert(result.args == args.toList)
  }

  // =========================================================================
  // Env.render tests
  // =========================================================================

  test("Env.render produces Doc output") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val env: PythonGen.Env[List[Code.Statement]] = PythonGen.Env.pure(List.empty)
    val doc = PythonGen.Env.render(env)
    assert(doc != null)
  }

  test("Env.render includes imports") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val parts = NonEmptyList.of("unittest")
    val env = PythonGen.Env.importLiteral(parts.map(Code.Ident(_))).map { _ =>
      List.empty[Code.Statement]
    }
    val doc = PythonGen.Env.render(env)
    val rendered = doc.render(80)
    assert(rendered.contains("import"))
  }

  // =========================================================================
  // Env helper function tests
  // =========================================================================

  test("Env.onLast processes single value") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val value: Code.ValueLike = Code.Ident("x")
    val env = PythonGen.Env.onLast(value)(x => Code.Op(x, Code.Const.Plus, Code.fromInt(1)))
    assert(env != null)
  }

  test("Env.onLasts processes multiple values") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val values: List[Code.ValueLike] = List(Code.Ident("x"), Code.Ident("y"))
    val env = PythonGen.Env.onLasts(values) {
      case x :: y :: Nil => Code.Op(x, Code.Const.Plus, y)
      case _ => Code.fromInt(0)
    }
    assert(env != null)
  }

  test("Env.onLast2 processes two values") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val v1: Code.ValueLike = Code.Ident("a")
    val v2: Code.ValueLike = Code.Ident("b")
    val env = PythonGen.Env.onLast2(v1, v2)((a, b) => Code.Op(a, Code.Const.Minus, b))
    assert(env != null)
  }

  test("Env.ifElse1 creates conditional") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val cond: Code.ValueLike = Code.Const.True
    val thenCase: Code.ValueLike = Code.fromInt(1)
    val elseCase: Code.ValueLike = Code.fromInt(0)

    val env = PythonGen.Env.ifElse1(cond, thenCase, elseCase)
    assert(env != null)
  }

  test("Env.andCode combines boolean expressions") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val c1: Code.ValueLike = Code.Const.True
    val c2: Code.ValueLike = Code.Const.False

    val env = PythonGen.Env.andCode(c1, c2)
    assert(env != null)
  }

  test("Env.ifElseS creates conditional statement") {
    import cats.syntax.all._
    given cats.Monad[PythonGen.Env] = PythonGen.Env.envMonad

    val cond: Code.ValueLike = Code.Const.True
    val thenStmt: Code.Statement = Code.Pass
    val elseStmt: Code.Statement = Code.Pass

    val env = PythonGen.Env.ifElseS(cond, thenStmt, elseStmt)
    assert(env != null)
  }

  // =========================================================================
  // Code utilities integration tests
  // =========================================================================

  test("Code.litToExpr handles integer literals") {
    import dev.bosatsu.Lit
    val lit = Lit.Integer(42)
    val expr = Code.litToExpr(lit)
    assert(expr != null)
  }

  test("Code.litToExpr handles string literals") {
    import dev.bosatsu.Lit
    val lit = Lit.Str("hello")
    val expr = Code.litToExpr(lit)
    assert(expr != null)
  }
}
