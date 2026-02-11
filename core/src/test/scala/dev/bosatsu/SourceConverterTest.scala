package dev.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

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

  private def stripWrapperExpr(
      expr: Expr[Declaration]
  ): Expr[Declaration] =
    expr match {
      case Expr.Annotation(in, _, _) => stripWrapperExpr(in)
      case Expr.Generic(_, in)       => stripWrapperExpr(in)
      case other                     => other
    }

  private def mainBranches(code: String): NonEmptyList[Expr.Branch[Declaration]] = {
    stripWrapperExpr(mainExpr(code)) match {
      case Expr.Match(_, branches, _) => branches
      case other                      => fail(s"expected match expression, got: $other")
    }
  }

  private def mainExpr(code: String): Expr[Declaration] =
    convertProgram(code)
      .getLet(Identifier.Name("main"))
      .getOrElse(fail("expected a `main` binding"))
      ._2

  private def eraseTags(expr: Expr[Declaration]): Expr[Unit] =
    expr match {
      case Expr.Annotation(in, tpe, _) =>
        Expr.Annotation(eraseTags(in), tpe, ())
      case Expr.Local(name, _) =>
        Expr.Local(name, ())
      case Expr.Generic(tvs, in) =>
        Expr.Generic(tvs, eraseTags(in))
      case Expr.Global(pack, name, _) =>
        Expr.Global(pack, name, ())
      case Expr.App(fn, args, _) =>
        Expr.App(eraseTags(fn), args.map(eraseTags), ())
      case Expr.Lambda(args, in, _) =>
        Expr.Lambda(args, eraseTags(in), ())
      case Expr.Let(arg, exp, in, rec, _) =>
        Expr.Let(arg, eraseTags(exp), eraseTags(in), rec, ())
      case Expr.Literal(lit, _) =>
        Expr.Literal(lit, ())
      case Expr.Match(arg, branches, _) =>
        Expr.Match(
          eraseTags(arg),
          branches.map { b =>
            Expr.Branch(b.pattern, b.guard.map(eraseTags), eraseTags(b.expr))
          },
          ()
        )
    }

  private def assertMainDesugarsAs(
      actualCode: String,
      expectedCode: String
  ): Unit = {
    val actual = eraseTags(mainExpr(actualCode))
    val expected = eraseTags(mainExpr(expectedCode))
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
}
