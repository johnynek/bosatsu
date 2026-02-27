package dev.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import cats.data.{Ior, NonEmptyChain, NonEmptyList}
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
  ): Ior[
    cats.data.NonEmptyChain[SourceConverter.Error],
    Program[
      (rankn.TypeEnv[Kind.Arg], rankn.ParsedTypeEnv[Option[Kind.Arg]]),
      Expr[Declaration],
      List[Statement]
    ]
  ] =
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

  private def defaultBindingNameAt(
      code: String,
      constructorName: Identifier.Constructor,
      paramIndex: Int
  ): String =
    defaultBindingAt(code, constructorName, paramIndex).asString

  private def assertSameDefaultBindingName(
      leftCode: String,
      rightCode: String,
      constructorName: Identifier.Constructor,
      paramIndex: Int
  ): Unit = {
    val left = defaultBindingNameAt(leftCode, constructorName, paramIndex)
    val right = defaultBindingNameAt(rightCode, constructorName, paramIndex)
    assertEquals(left, right)
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

  private def genericBinders(
      expr: Expr[Declaration]
  ): List[NonEmptyList[(rankn.Type.Var.Bound, Kind)]] =
    expr match {
      case Expr.Annotation(in, _, _) => genericBinders(in)
      case Expr.Local(_, _)          => Nil
      case Expr.Global(_, _, _)      => Nil
      case Expr.Generic(typeVars, in) =>
        typeVars :: genericBinders(in)
      case Expr.App(fn, args, _) =>
        genericBinders(fn) ::: args.toList.flatMap(genericBinders)
      case Expr.Lambda(_, in, _) =>
        genericBinders(in)
      case Expr.Let(_, ex, in, _, _) =>
        genericBinders(ex) ::: genericBinders(in)
      case Expr.Literal(_, _) =>
        Nil
      case Expr.Match(arg, branches, _) =>
        genericBinders(arg) ::: branches.toList.flatMap { b =>
          b.guard.fold(Nil: List[NonEmptyList[(rankn.Type.Var.Bound, Kind)]])(
            genericBinders
          ) ::: genericBinders(b.expr)
        }
    }

  private def assertMainDesugarsAs(
      actualCode: String,
      expectedCode: String
  ): Unit = {
    val actual = mainExpr(actualCode).eraseTags
    val expected = mainExpr(expectedCode).eraseTags
    assertEquals(actual, expected)
  }

  test(
    "addErrorKeepGoing preserves successful values while accumulating errors"
  ) {
    val e1 = SourceConverter.UnknownTypeName(
      Identifier.Constructor("One"),
      Region(0, 0)
    )
    val e2 = SourceConverter.UnknownTypeName(
      Identifier.Constructor("Two"),
      Region(1, 1)
    )

    val fromRight =
      SourceConverter.addErrorKeepGoing(SourceConverter.success(1), e1)
    assertEquals(fromRight, Ior.Both(NonEmptyChain.one(e1), 1))

    val fromLeft = SourceConverter.addErrorKeepGoing[Int](
      Ior.Left(NonEmptyChain.one(e1)),
      e2
    )
    fromLeft match {
      case Ior.Left(errs) =>
        assertEquals(errs.toList, List(e1, e2))
      case other =>
        fail(s"expected Left with accumulated errors, got: $other")
    }

    val fromBoth = SourceConverter.addErrorKeepGoing(
      Ior.Both(NonEmptyChain.one(e1), 7),
      e2
    )
    fromBoth match {
      case Ior.Both(errs, value) =>
        assertEquals(value, 7)
        assertEquals(errs.toList, List(e1, e2))
      case other =>
        fail(s"expected Both with accumulated errors, got: $other")
    }
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
              assert(
                Identifier.isSynthetic(b),
                s"expected synthetic binding, got: $b"
              )
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

  test(
    "record update desugars to a single-branch match for single-constructor structs"
  ) {
    val expr = stripWrapperExpr(mainExpr("""#
struct Foo(a, b, c)
base = Foo(1, 2, 3)
main = Foo { b: 9, ..base }
"""))

    expr match {
      case Expr.Match(scrutinee, branches, _) =>
        assertEquals(branches.length, 1)
        stripWrapperExpr(scrutinee) match {
          case Expr.Global(p, Identifier.Name("base"), _) =>
            assertEquals(p, TestUtils.testPackage)
          case other =>
            fail(s"expected base scrutinee, got: $other")
        }

        val branch = branches.head
        branch.pattern match {
          case Pattern.PositionalStruct(
                (pack, Identifier.Constructor("Foo")),
                List(aPat, bPat, cPat)
              ) =>
            assertEquals(pack, TestUtils.testPackage)
            val aName = aPat match {
              case Pattern.Var(v) => v
              case other          =>
                fail(s"expected var pattern for field a, got: $other")
            }
            assertEquals(bPat, Pattern.WildCard)
            val cName = cPat match {
              case Pattern.Var(v) => v
              case other          =>
                fail(s"expected var pattern for field c, got: $other")
            }

            branch.expr match {
              case Expr.App(
                    Expr.Global(p, Identifier.Constructor("Foo"), _),
                    args,
                    _
                  ) =>
                assertEquals(p, TestUtils.testPackage)
                val argList = args.toList
                assertEquals(argList.length, 3)

                stripWrapperExpr(argList(0)) match {
                  case Expr.Local(v, _) => assertEquals(v, aName)
                  case other            =>
                    fail(s"expected copied local for field a, got: $other")
                }
                stripWrapperExpr(argList(1)) match {
                  case Expr.Literal(lit, _) =>
                    assertEquals(lit, Lit.fromInt(9))
                  case other =>
                    fail(s"expected explicit literal for field b, got: $other")
                }
                stripWrapperExpr(argList(2)) match {
                  case Expr.Local(v, _) => assertEquals(v, cName)
                  case other            =>
                    fail(s"expected copied local for field c, got: $other")
                }
              case other =>
                fail(s"expected rebuilt Foo constructor, got: $other")
            }
          case other =>
            fail(s"expected Foo positional pattern, got: $other")
        }
      case other =>
        fail(s"expected match expression, got: $other")
    }
  }

  test("record update supports shorthand explicit fields") {
    val code = """#
struct Foo(a, b, c)
a = 9
base = Foo(1, 2, 3)
main = Foo { a, ..base }
"""

    assertEquals(conversionErrors(code), Nil)

    mainBranches(code).head.expr match {
      case Expr.App(
            Expr.Global(_, Identifier.Constructor("Foo"), _),
            args,
            _
          ) =>
        val argList = args.toList
        stripWrapperExpr(argList(0)) match {
          case Expr.Global(p, Identifier.Name("a"), _) =>
            assertEquals(p, TestUtils.testPackage)
          case other =>
            fail(
              s"expected shorthand field `a` to resolve to global, got: $other"
            )
        }
      case other =>
        fail(s"expected rebuilt Foo constructor, got: $other")
    }
  }

  test("record update errors on non-single-constructor enum types") {
    val errs = conversionErrors("""#
enum Foo:
  A(a, b)
  B(v)
base = A(1, 2)
main = A { a: 3, ..base }
""")

    assert(
      errs.exists {
        case SourceConverter.RecordUpdateRequiresSingleConstructor(
              Identifier.Constructor("A"),
              TypeName(Identifier.Constructor("Foo")),
              2,
              _,
              _
            ) =>
          true
        case _ =>
          false
      },
      s"missing RecordUpdateRequiresSingleConstructor in errors: $errs"
    )
  }

  test("record update errors when no fields are sourced from base") {
    val errs = conversionErrors("""#
struct Foo(a, b)
base = Foo(1, 2)
main = Foo { a: 3, b: 4, ..base }
""")

    assert(
      errs.exists {
        case SourceConverter.RecordUpdateNoFieldsFromBase(
              Identifier.Constructor("Foo"),
              _,
              _
            ) =>
          true
        case _ =>
          false
      },
      s"missing RecordUpdateNoFieldsFromBase in errors: $errs"
    )
  }

  test("record update errors when there are no explicit field overrides") {
    val errs = conversionErrors("""#
struct Foo(a, b)
base = Foo(1, 2)
main = Foo { ..base }
""")

    assert(
      errs.exists {
        case SourceConverter.RecordUpdateRequiresExplicitField(
              Identifier.Constructor("Foo"),
              _,
              _
            ) =>
          true
        case _ =>
          false
      },
      s"missing RecordUpdateRequiresExplicitField in errors: $errs"
    )
  }

  test(
    "record update duplicate fields report an error and use last-write-wins"
  ) {
    val code = """#
struct Foo(a, b)
base = Foo(1, 2)
main = Foo { a: 3, a: 4, ..base }
"""
    val errs = conversionErrors(code)

    assert(
      errs.exists {
        case SourceConverter.RecordUpdateDuplicateField(
              Identifier.Constructor("Foo"),
              _,
              duplicated,
              _
            ) =>
          duplicated.toList.contains(Identifier.Name("a"))
        case _ =>
          false
      },
      s"missing RecordUpdateDuplicateField in errors: $errs"
    )

    mainBranches(code).head.expr match {
      case Expr.App(
            Expr.Global(_, Identifier.Constructor("Foo"), _),
            args,
            _
          ) =>
        stripWrapperExpr(args.toList.head) match {
          case Expr.Literal(lit, _) =>
            assertEquals(lit, Lit.fromInt(4))
          case other =>
            fail(s"expected last explicit `a` value to win, got: $other")
        }
      case other =>
        fail(s"expected rebuilt Foo constructor, got: $other")
    }
  }

  test(
    "record update duplicate fields listed once even with repeated duplicates"
  ) {
    val code = """#
struct Foo(a, b)
base = Foo(1, 2)
main = Foo { a: 3, a: 4, a: 5, ..base }
"""
    val errs = conversionErrors(code)

    val dups = errs.collect {
      case SourceConverter.RecordUpdateDuplicateField(
            Identifier.Constructor("Foo"),
            _,
            duplicated,
            _
          ) =>
        duplicated.toList
    }

    assertEquals(dups, List(List(Identifier.Name("a"))))

    mainBranches(code).head.expr match {
      case Expr.App(
            Expr.Global(_, Identifier.Constructor("Foo"), _),
            args,
            _
          ) =>
        stripWrapperExpr(args.toList.head) match {
          case Expr.Literal(lit, _) =>
            assertEquals(lit, Lit.fromInt(5))
          case other =>
            fail(s"expected final explicit `a` value to win, got: $other")
        }
      case other =>
        fail(s"expected rebuilt Foo constructor, got: $other")
    }
  }

  test("record update retains unexpected-field validation behavior") {
    val errs = conversionErrors("""#
struct Foo(a, b)
base = Foo(1, 2)
main = Foo { a: 3, nope: 4, ..base }
""")

    assert(
      errs.exists {
        case SourceConverter.UnexpectedField(
              Identifier.Constructor("Foo"),
              _,
              unexpected,
              expected,
              _
            ) =>
          unexpected.toList.contains(Identifier.Name("nope")) &&
          expected == List(Identifier.Name("a"), Identifier.Name("b"))
        case _ =>
          false
      },
      s"missing UnexpectedField in errors: $errs"
    )
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

  test(
    "constructor defaults may reference earlier top-level values and earlier defaults"
  ) {
    val code = """#
enum I:
  I1
seed = I1
struct Foo(a: I = seed)
struct Bar(f: Foo = Foo { })
main = Bar { }
"""

    assertEquals(conversionErrors(code), Nil)

    val fooDefault = defaultBindingAt(code, Identifier.Constructor("Foo"), 0)
    val barDefault = defaultBindingAt(code, Identifier.Constructor("Bar"), 0)
    val program = convertProgram(code)

    val fooHelperExpr = program
      .getLet(fooDefault)
      .getOrElse(
        fail(s"missing helper binding for ${fooDefault.sourceCodeRepr}")
      )
      ._2
    val barHelperExpr = program
      .getLet(barDefault)
      .getOrElse(
        fail(s"missing helper binding for ${barDefault.sourceCodeRepr}")
      )
      ._2

    assert(
      fooHelperExpr.globals.exists(g =>
        (g.pack == TestUtils.testPackage) && (g.name == Identifier.Name("seed"))
      ),
      s"expected helper ${fooDefault.sourceCodeRepr} to reference seed, got: $fooHelperExpr"
    )
    assert(
      barHelperExpr.globals.exists(g =>
        (g.pack == TestUtils.testPackage) && (g.name == (fooDefault: Identifier))
      ),
      s"expected helper ${barDefault.sourceCodeRepr} to reference helper ${fooDefault.sourceCodeRepr}, got: $barHelperExpr"
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
    val expected =
      "_default$50073f0ba049aa53c2d877eb2ea956b4648234d29c416431e9a3c610bca9ecd2"
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
    assertEquals(d1.asString, expected)
    assertEquals(d2.asString, expected)
  }

  test("default helper names for struct params are golden") {
    val code = """#
struct S(a: Int = 1, b: Int = 2)
main = S {}
"""
    val expected = List(
      "_default$50073f0ba049aa53c2d877eb2ea956b4648234d29c416431e9a3c610bca9ecd2",
      "_default$7d5827fcef00708b1a0c1a8572d85a027bd5fe92707e318dd24596fad59bf0a4"
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
      "_default$98133e4e32637ba2366a6d2f3608ec36ba054c7e3ec53d9668ec9313bada7895",
      "_default$02bc9f74616311159c7eb5cac21e0e62cbf4ea541888f91860f8957f0d2b537c"
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
      "_default$37c39e2374b4af5ba5b2da649d3a5c570cceaba00e107ef0e14a658805af05f6"
    val actual = defaultBindingAt(code, Identifier.Constructor("G"), 0).asString

    assertEquals(actual, expected)
  }

  test(
    "generic default helper naming is stable across type parameter renaming"
  ) {
    val codeA = """#
struct Foo[a](opt: Option[a] = None)
"""
    val codeB = """#
struct Foo[b](opt: Option[b] = None)
"""

    assertSameDefaultBindingName(codeA, codeB, Identifier.Constructor("Foo"), 0)
  }

  test("generic default helper naming is stable across many alpha renames") {
    val names = List("a", "b", "with_t", "x", "value", "qq")
    val ctor = Identifier.Constructor("Foo")
    val mkCode = (tv: String) => s"""#
struct Foo[$tv](opt: Option[$tv] = None)
"""

    val base = defaultBindingNameAt(mkCode(names.head), ctor, 0)
    names.foreach { tv =>
      val next = defaultBindingNameAt(mkCode(tv), ctor, 0)
      assertEquals(
        next,
        base,
        s"type parameter rename changed default hash: $tv"
      )
    }
  }

  test("generic default helper naming is stable for multiple type parameters") {
    val codeAB = """#
struct Foo[a, b](pair: (Option[a], Option[b]) = (None, None))
"""
    val codeXY = """#
struct Foo[x, y](pair: (Option[x], Option[y]) = (None, None))
"""

    assertSameDefaultBindingName(
      codeAB,
      codeXY,
      Identifier.Constructor("Foo"),
      0
    )
  }

  test("generic default helper naming is stable in nested function types") {
    val codeA = """#
struct Foo[a](fn: a -> Option[a] = x -> None)
"""
    val codeT = """#
struct Foo[t](fn: t -> Option[t] = y -> None)
"""

    assertSameDefaultBindingName(codeA, codeT, Identifier.Constructor("Foo"), 0)
  }

  test("generic default helper naming ignores explicit vs implicit * kind") {
    val codeImplicit = """#
struct Foo[a](opt: Option[a] = None)
"""
    val codeExplicit = """#
struct Foo[a: *](opt: Option[a] = None)
"""

    assertSameDefaultBindingName(
      codeImplicit,
      codeExplicit,
      Identifier.Constructor("Foo"),
      0
    )
  }

  test(
    "generic default helper naming is stable for equivalent type-arg parenthesization"
  ) {
    val codeFlat = """#
struct Foo[a](opt: Option[Option[a]] = None)
"""
    val codeParen = """#
struct Foo[a](opt: Option[(Option[a])] = None)
"""

    assertSameDefaultBindingName(
      codeFlat,
      codeParen,
      Identifier.Constructor("Foo"),
      0
    )
  }

  test(
    "generic default helper naming is stable for equivalent function parenthesization"
  ) {
    val codeFlat = """#
struct Foo[a](fn: a -> Option[a] -> Option[a] = x -> y -> None)
"""
    val codeParen = """#
struct Foo[a](fn: a -> (Option[a] -> Option[a]) = x -> y -> None)
"""

    assertSameDefaultBindingName(
      codeFlat,
      codeParen,
      Identifier.Constructor("Foo"),
      0
    )
  }

  test(
    "generic default helper naming is stable for equivalent tuple parenthesization"
  ) {
    val codeFlat = """#
struct Foo[a, b](pair: (Option[a], Option[b]) = (None, None))
"""
    val codeParen = """#
struct Foo[a, b](pair: ((Option[a]), (Option[b])) = (None, None))
"""

    assertSameDefaultBindingName(
      codeFlat,
      codeParen,
      Identifier.Constructor("Foo"),
      0
    )
  }

  test("generic default helper naming is stable under forall binder renaming") {
    val codeA = """#
struct Foo[a](fn: forall t. t -> Option[a] = x -> None)
"""
    val codeB = """#
struct Foo[a](fn: forall z. z -> Option[a] = y -> None)
"""

    assertSameDefaultBindingName(codeA, codeB, Identifier.Constructor("Foo"), 0)
  }

  test(
    "enum branch existential default helper naming is stable across branch type-parameter renaming"
  ) {
    val codeB = """#
enum FreeF[a]:
  Pure(a: a)
  Mapped[b](opt: Option[b] = None, fn: b -> a)
"""
    val codeZ = """#
enum FreeF[a]:
  Pure(a: a)
  Mapped[z](opt: Option[z] = None, fn: z -> a)
"""

    assertSameDefaultBindingName(
      codeB,
      codeZ,
      Identifier.Constructor("Mapped"),
      0
    )
  }

  test(
    "enum branch existential default helper naming is stable across many renames"
  ) {
    val names = List("b", "x", "with_t", "inner", "elem")
    val ctor = Identifier.Constructor("Mapped")
    def mkCode(tv: String): String = s"""#
enum FreeF[a]:
  Pure(a: a)
  Mapped[$tv](opt: Option[$tv] = None, fn: $tv -> a)
"""

    val base = defaultBindingNameAt(mkCode(names.head), ctor, 0)
    names.foreach { tv =>
      val next = defaultBindingNameAt(mkCode(tv), ctor, 0)
      assertEquals(
        next,
        base,
        s"branch type parameter rename changed default hash: $tv"
      )
    }
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

  test("nested def reuses outer type parameter instead of introducing inner generic") {
    val code = """#
def foo[a](lst: List[a]) -> Int:
  def loop(list: List[a], acc: Int) -> Int:
    match list:
      case []: acc
      case [_, *t]: loop(t, acc.add(1))
  loop(lst, 0)

main = foo([1, 2, 3])
"""

    val fooExpr = convertProgram(code)
      .getLet(Identifier.Name("foo"))
      .getOrElse(fail("expected a `foo` binding"))
      ._2

    val generics = genericBinders(fooExpr)
    assertEquals(generics.length, 1)
    assertEquals(generics.head.map(_._1.name).toList, List("a"))
  }

  test(
    "nested def reuses inferred outer type parameter instead of introducing inner generic"
  ) {
    val code = """#
def foo(lst: List[a]) -> Int:
  def loop(list: List[a], acc: Int) -> Int:
    match list:
      case []: acc
      case [_, *t]: loop(t, acc.add(1))
  loop(lst, 0)

main = foo([1, 2, 3])
"""

    val fooExpr = convertProgram(code)
      .getLet(Identifier.Name("foo"))
      .getOrElse(fail("expected a `foo` binding"))
      ._2

    val generics = genericBinders(fooExpr)
    assertEquals(generics.length, 1)
    assertEquals(generics.head.map(_._1.name).toList, List("a"))
  }

  test("nested def with explicit inner type parameter introduces a second generic") {
    val code = """#
def foo(lst: List[a]) -> Int:
  def loop[a](list: List[a], acc: Int) -> Int:
    match list:
      case []: acc
      case [_, *t]: loop(t, acc.add(1))
  loop(lst, 0)

main = foo([1, 2, 3])
"""

    val fooExpr = convertProgram(code)
      .getLet(Identifier.Name("foo"))
      .getOrElse(fail("expected a `foo` binding"))
      ._2

    val generics = genericBinders(fooExpr)
    assertEquals(generics.length, 2)
    assertEquals(generics.map(_.map(_._1.name).toList), List(List("a"), List("a")))
  }
}
