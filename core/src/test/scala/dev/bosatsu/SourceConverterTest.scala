package dev.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import cats.data.{Ior, NonEmptyList}
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
  ] = {
    val stmts = TestUtils.statementsOf(code)
    SourceConverter.toProgram(TestUtils.testPackage, Nil, stmts) match {
      case Ior.Right(prog)   => prog
      case Ior.Both(_, prog) => prog
      case Ior.Left(errs)    => fail(s"conversion failed: $errs")
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

  private def mainBranches(code: String): NonEmptyList[Expr.Branch[Declaration]] = {
    val prog = convertProgram(code)
    val (_, mainExpr) = prog
      .getLet(Identifier.Name("main"))
      .getOrElse(fail("expected a `main` binding"))
    stripWrapperExpr(mainExpr) match {
      case Expr.Match(_, branches, _) => branches
      case other                      => fail(s"expected match expression, got: $other")
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
}
