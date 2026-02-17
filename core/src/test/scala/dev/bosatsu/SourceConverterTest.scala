package dev.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import cats.data.Ior
import cats.data.NonEmptyList
import Identifier.Bindable

import cats.implicits._

class SourceConverterTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 3000 else 20
    )

  val genRec = Gen.oneOf(RecursionKind.NonRecursive, RecursionKind.Recursive)

  private def convertProgram(code: String): Program[
    (rankn.TypeEnv[Kind.Arg], rankn.ParsedTypeEnv[Option[Kind.Arg]]),
    Expr[Declaration],
    List[Statement]
  ] =
    TestUtils.sourceConvertedProgramOf(TestUtils.testPackage, code)

  private def convertProgramResult(
      code: String
  ): Ior[ cats.data.NonEmptyChain[SourceConverter.Error], Program[
    (rankn.TypeEnv[Kind.Arg], rankn.ParsedTypeEnv[Option[Kind.Arg]]),
    Expr[Declaration],
    List[Statement]
  ]] =
    SourceConverter.toProgram(
      TestUtils.testPackage,
      Nil,
      Parser.unsafeParse(Statement.parser, code)
    )

  private def conversionErrors(code: String): List[SourceConverter.Error] =
    convertProgramResult(code) match {
      case Ior.Left(errs)    => errs.toList
      case Ior.Both(errs, _) => errs.toList
      case Ior.Right(_)      => Nil
    }

  private def defaultBindingAt(
      code: String,
      constructorName: Identifier.Constructor,
      paramIndex: Int
  ): Bindable = {
    val params = rankn.TypeEnv
      .fromParsed(convertProgram(code).types._2)
      .getConstructorParams(
        TestUtils.testPackage,
        constructorName
      )
    val found = for {
      ps <- params
      p <- ps.lift(paramIndex)
      b <- p.defaultBinding
    } yield b

    found.getOrElse {
      fail(
        s"expected default binding for ${constructorName.sourceCodeRepr} index $paramIndex"
      )
    }
  }

  private def stripWrapperExpr(
      expr: Expr[Declaration]
  ): Expr[Declaration] =
    expr match {
      case Expr.Annotation(in, _, _) => stripWrapperExpr(in)
      case Expr.Generic(_, in)       => stripWrapperExpr(in)
      case other                     => other
    }

  private def mainBranches(
      code: String
  ): NonEmptyList[Expr.Branch[Declaration]] =
    stripWrapperExpr(mainExpr(code)) match {
      case Expr.Match(_, branches, _) => branches
      case other => fail(s"expected match expression, got: $other")
    }

  private def mainExpr(code: String): Expr[Declaration] =
    convertProgram(code)
      .getLet(Identifier.Name("main"))
      .getOrElse(fail("expected a `main` binding"))
      ._2

  private def assertMainDesugarsAs(
      actualCode: String,
      expectedCode: String
  ): Unit = {
    val actual = mainExpr(actualCode).eraseTags
    val expected = mainExpr(expectedCode).eraseTags
    assertEquals(actual, expected)
  }

  test("makeLetsUnique preserves let count") {
    val genLets = for {
      cnt <- Gen.choose(0, 100)
      lets <- Gen.listOfN(
        cnt,
        Gen.zip(Generators.bindIdentGen, genRec, Gen.const(()))
      )
    } yield lets

    forAll(genLets) { lets =>
      val p1 = SourceConverter.makeLetsUnique(lets) { (b, idx) =>
        (Identifier.Backticked(b.asString + s"____${idx}"), identity[Unit])
      }

      val p1sz = p1.size
      // the total number of lets is unchanged
      assertEquals(p1sz, lets.size)
      // the result has distinct names
      assertEquals(p1sz, p1.iterator.map(_._1).toSet.size)
      // recursiveness is not changed:
      assertEquals(p1.map(_._2), lets.map(_._2))
    }
  }

  test("makeLetsUnique is identity if binds are unique") {
    val genLets = for {
      cnt <- Gen.choose(0, 100)
      names <- Gen.listOfN(cnt, Generators.bindIdentGen)
      namesDistinct = names.distinct
      lets <- Generators.traverseGen(namesDistinct) { nm =>
        Gen
          .zip(genRec, Gen.choose(0, 10))
          .map { case (r, d) => (nm, r, d) }
      }
    } yield lets

    forAll(genLets) { lets =>
      val p1 = SourceConverter.makeLetsUnique(lets) { (b, idx) =>
        (Identifier.Backticked(b.asString + s"____${idx}"), { _ => -idx })
      }

      assert(p1 eq lets)
    }
  }

  test("makeLetsUnique applies to rhs for recursive binds") {
    val genLets = for {
      cnt <- Gen.choose(0, 100)
      lets <- Gen.listOfN(
        cnt,
        Generators.bindIdentGen.map(b => (b, RecursionKind.Recursive, b))
      )
    } yield lets

    forAll(genLets) { lets =>
      val p1 = SourceConverter.makeLetsUnique(lets) { (b, idx) =>
        val res = Identifier.Backticked(b.asString + s"____${idx}")
        (res, { (br: Bindable) => if (br == b) res else br })
      }

      p1.foreach { case (bl, _, br) =>
        assertEquals(bl, br)
      }
    }
  }

  test("test some examples") {
    {
      // non recursive
      val l1 = List(
        (
          Identifier.Name("b"),
          RecursionKind.NonRecursive,
          Option.empty[String]
        ),
        (
          Identifier.Name("a"),
          RecursionKind.NonRecursive,
          Option.empty[String]
        ),
        (
          Identifier.Name("c"),
          RecursionKind.NonRecursive,
          Option.empty[String]
        ),
        (
          Identifier.Name("a"),
          RecursionKind.NonRecursive,
          Option.empty[String]
        ),
        (
          Identifier.Name("d"),
          RecursionKind.NonRecursive,
          Option.empty[String]
        ),
        (Identifier.Name("a"), RecursionKind.NonRecursive, Option.empty[String])
      )

      val up1 = SourceConverter.makeLetsUnique(l1) {
        case (Identifier.Name(n), idx) =>
          val b1 = Identifier.Name(n + idx)
          val fn: Option[String] => Option[String] = { _ => Some(n + idx) }

          (b1, fn)
        case (b, _) => (b, identity[Option[String]])
      }

      val expectl1 = List(
        (Identifier.Name("b"), RecursionKind.NonRecursive, None),
        (Identifier.Name("a0"), RecursionKind.NonRecursive, None),
        (Identifier.Name("c"), RecursionKind.NonRecursive, Some("a0")),
        (Identifier.Name("a1"), RecursionKind.NonRecursive, Some("a0")),
        (Identifier.Name("d"), RecursionKind.NonRecursive, Some("a1")),
        (Identifier.Name("a"), RecursionKind.NonRecursive, Some("a1"))
      )
      assertEquals(up1, expectl1)
    }

    {
      // recursive
      val l1 = List(
        (Identifier.Name("b"), RecursionKind.Recursive, Option.empty[String]),
        (Identifier.Name("a"), RecursionKind.Recursive, Option.empty[String]),
        (Identifier.Name("c"), RecursionKind.Recursive, Option.empty[String]),
        (Identifier.Name("a"), RecursionKind.Recursive, Option.empty[String]),
        (Identifier.Name("d"), RecursionKind.Recursive, Option.empty[String]),
        (Identifier.Name("a"), RecursionKind.Recursive, Option.empty[String])
      )

      val up1 = SourceConverter.makeLetsUnique(l1) {
        case (Identifier.Name(n), idx) =>
          val b1 = Identifier.Name(n + idx)
          val fn: Option[String] => Option[String] = { _ => Some(n + idx) }

          (b1, fn)
        case (b, _) => (b, identity[Option[String]])
      }

      val expectl1 = List(
        (Identifier.Name("b"), RecursionKind.Recursive, None),
        (Identifier.Name("a0"), RecursionKind.Recursive, Some("a0")),
        (Identifier.Name("c"), RecursionKind.Recursive, Some("a0")),
        (Identifier.Name("a1"), RecursionKind.Recursive, Some("a1")),
        (Identifier.Name("d"), RecursionKind.Recursive, Some("a1")),
        (Identifier.Name("a"), RecursionKind.Recursive, None)
      )
      assertEquals(up1, expectl1)
    }
  }

  test("non-Predef constructor named True is not canonicalized as bool True") {
    val branches = mainBranches("""#
enum LocalBool: True, False

main = match True:
  case x if True: x
  case _: False
""")

    branches.head.guard match {
      case Some(Expr.Global(p, Identifier.Constructor("True"), _))
          if p == TestUtils.testPackage =>
        ()
      case other =>
        fail(s"expected local True guard to remain guarded, got: $other")
    }
  }

  test("left-apply desugars by appending continuation to the outermost apply") {
    val examples = List(
      (
        """main = foo(a, b)""",
        """main = foo(a, b)"""
      ),
      (
        """main = x.await()""",
        """main = x.await()"""
      ),
      (
        """main = (
  p <- foo(a, b)
  p
)""",
        """main = foo(a, b, p -> p)"""
      ),
      (
        """main = (
  p <- foo(a)(b)
  p
)""",
        """main = foo(a)(b, p -> p)"""
      ),
      (
        """main = (
  p <- x.await()
  p
)""",
        """main = await(x, p -> p)"""
      ),
      (
        """main = (
  p <- x.await().map(f)
  p
)""",
        """main = map(await(x), f, p -> p)"""
      ),
      (
        """main = (
  p <- (foo(a))
  p
)""",
        """main = foo(a, p -> p)"""
      )
    )

    examples.foreach { case (actual, expected) =>
      assertMainDesugarsAs(actual, expected)
    }
  }

  test("record constructors fill omitted fields with default helpers") {
    val expr = stripWrapperExpr(mainExpr("""#
struct S(a: Int = 1, b: Int, c: Int = 3)
main = S { b: 2 }
"""))

    expr match {
      case Expr.App(
            Expr.Global(pack, Identifier.Constructor("S"), _),
            args,
            _
          ) =>
        assertEquals(pack, TestUtils.testPackage)
        val argList = args.toList
        assertEquals(argList.length, 3)

        def assertSyntheticGlobal(in: Expr[Declaration]): Unit =
          stripWrapperExpr(in) match {
            case Expr.Global(p, b: Identifier.Bindable, _) =>
              assertEquals(p, TestUtils.testPackage)
              assert(Identifier.isSynthetic(b), s"expected synthetic binding, got: $b")
            case other =>
              fail(s"expected synthetic global, got: $other")
          }

        assertSyntheticGlobal(argList(0))
        stripWrapperExpr(argList(1)) match {
          case Expr.Literal(lit, _) =>
            assertEquals(lit, Lit.fromInt(2))
          case other =>
            fail(s"expected explicit literal argument, got: $other")
        }
        assertSyntheticGlobal(argList(2))
      case other =>
        fail(s"expected constructor application, got: $other")
    }
  }

  test("positional constructor calls do not use default filling") {
    val expr = stripWrapperExpr(mainExpr("""#
struct S(a: Int = 1, b: Int = 2)
main = S(9)
"""))

    expr match {
      case Expr.App(
            Expr.Global(_, Identifier.Constructor("S"), _),
            args,
            _
          ) =>
        assertEquals(args.length, 1)
      case other =>
        fail(s"expected constructor application, got: $other")
    }
  }

  test("constructor defaults cannot reference constructor parameters") {
    val errs = conversionErrors("""#
struct S(a: Int, b: Int = a)
main = S { a: 1 }
""")

    assert(
      errs.exists {
        case SourceConverter.ConstructorDefaultReferencesParam(
              Identifier.Constructor("S"),
              Identifier.Name("b"),
              Identifier.Name("a"),
              _
            ) =>
          true
        case _ =>
          false
      },
      s"missing ConstructorDefaultReferencesParam in errors: $errs"
    )
  }

  test("constructor defaults cannot reference later top-level bindings") {
    val errs = conversionErrors("""#
struct S(a: Int = later)
later = 1
main = S {}
""")

    assert(
      errs.exists {
        case SourceConverter.ConstructorDefaultOutOfScope(
              Identifier.Constructor("S"),
              Identifier.Name("a"),
              Identifier.Name("later"),
              _
            ) =>
          true
        case _ =>
          false
      },
      s"missing ConstructorDefaultOutOfScope in errors: $errs"
    )
  }

  test("constructor defaults require explicit type annotations") {
    val errs = conversionErrors("""#
struct S(a = 1)
main = S {}
""")

    assert(
      errs.exists {
        case SourceConverter.ConstructorDefaultRequiresTypeAnnotation(
              Identifier.Constructor("S"),
              Identifier.Name("a"),
              _
            ) =>
          true
        case _ =>
          false
      },
      s"missing ConstructorDefaultRequiresTypeAnnotation in errors: $errs"
    )
  }

  test("default helper naming is stable across default body changes") {
    val ctor = Identifier.Constructor("S")
    val d1 = defaultBindingAt(
      """#
struct S(a: Int = 1)
main = S {}
""",
      ctor,
      0
    )
    val d2 = defaultBindingAt(
      """#
struct S(a: Int = 2)
main = S {}
""",
      ctor,
      0
    )

    assertEquals(d1, d2)
  }

  test("default helper names for struct params are golden") {
    val code = """#
struct S(a: Int = 1, b: Int = 2)
main = S {}
"""
    val expected = List(
      "_default$36294736f849c2349886d1d73449f07eec152f7063e7efdcf18e38b58f20dd4e",
      "_default$6c758775f4a15b41049ac536a8e9acbf6b0f23735edde50976ee815531afe650"
    )
    val actual = List(
      defaultBindingAt(code, Identifier.Constructor("S"), 0).asString,
      defaultBindingAt(code, Identifier.Constructor("S"), 1).asString
    )

    assertEquals(actual, expected)
  }

  test("default helper names for enum constructor params are golden") {
    val code = """#
enum E:
  A(x: Int = 1)
  B(y: String = "")
main = A {}
"""
    val expected = List(
      "_default$65c012a00b9f18e2b592cdca73543d7addbd8824c51ed8da36f0bd46f141424a",
      "_default$c2d538919e2883cb892e2c9b039ab4bdb14f8d47543f7c8af96f5b698b5205e3"
    )
    val actual = List(
      defaultBindingAt(code, Identifier.Constructor("A"), 0).asString,
      defaultBindingAt(code, Identifier.Constructor("B"), 0).asString
    )

    assertEquals(actual, expected)
  }

  test("default helper names for generic default param are golden") {
    val code = """#
struct G[a](x: Option[a] = None)
"""
    val expected =
      "_default$84f689fe3fe28be9ea01931afc62a63821a041dd5c2c98f5ef36eb99bb17ba5c"
    val actual = defaultBindingAt(code, Identifier.Constructor("G"), 0).asString

    assertEquals(actual, expected)
  }

  test("generic struct defaults close non-canonical type variable names") {
    val code = """#
enum O[a]:
  N
  S(value: a)
struct G[with_t](x: O[with_t] = N)
main = G {}
"""

    assertEquals(conversionErrors(code), Nil)

    val helper = defaultBindingAt(code, Identifier.Constructor("G"), 0)
    val helperExpr = convertProgram(code)
      .getLet(helper)
      .getOrElse(fail(s"missing helper binding for ${helper.sourceCodeRepr}"))
      ._2

    def annotatedTypeOf(expr: Expr[Declaration]): Option[rankn.Type] =
      expr match {
        case Expr.Annotation(_, tpe, _) => Some(tpe)
        case Expr.Generic(_, in)        => annotatedTypeOf(in)
        case _                          => None
      }

    val actualType = annotatedTypeOf(helperExpr)
      .getOrElse(fail(s"missing annotation on helper expression: $helperExpr"))

    val withT = rankn.Type.Var.Bound("with_t")
    val oTy = rankn.Type.TyConst(
      rankn.Type.Const.Defined(
        TestUtils.testPackage,
        TypeName(Identifier.Constructor("O"))
      )
    )
    val expectedType = rankn.Type.forAll(
      List((withT, Kind.Type)),
      rankn.Type.TyApply(oTy, rankn.Type.TyVar(withT))
    )

    assert(
      actualType.sameAs(expectedType),
      s"expected helper type ${expectedType} but found ${actualType}"
    )
  }

  test("enum defaults to earlier constructors with polymorphic helper type") {
    val code = """#
enum MyList[a]:
  MyEmpty
  MyCons(head: a, tail: MyList[a] = MyEmpty)

main = MyCons { head: 1 }
"""

    assertEquals(conversionErrors(code), Nil)

    val helper = defaultBindingAt(code, Identifier.Constructor("MyCons"), 1)
    val helperExpr = convertProgram(code)
      .getLet(helper)
      .getOrElse(fail(s"missing helper binding for ${helper.sourceCodeRepr}"))
      ._2

    def annotatedTypeOf(expr: Expr[Declaration]): Option[rankn.Type] =
      expr match {
        case Expr.Annotation(_, tpe, _) => Some(tpe)
        case Expr.Generic(_, in)        => annotatedTypeOf(in)
        case _                          => None
      }

    val actualType = annotatedTypeOf(helperExpr)
      .getOrElse(fail(s"missing annotation on helper expression: $helperExpr"))

    val a = rankn.Type.Var.Bound("a")
    val myList = rankn.Type.TyConst(
      rankn.Type.Const.Defined(
        TestUtils.testPackage,
        TypeName(Identifier.Constructor("MyList"))
      )
    )
    val expectedType = rankn.Type.forAll(
      List((a, Kind.Type)),
      rankn.Type.TyApply(myList, rankn.Type.TyVar(a))
    )

    assert(
      actualType.sameAs(expectedType),
      s"expected helper type ${expectedType} but found ${actualType}"
    )
  }
}
