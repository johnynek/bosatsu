package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}

import Identifier.Bindable

import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite

class SourceConverterTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful =
      if (Platform.isScalaJvm) 3000 else 20
    )

  val genRec = Gen.oneOf(RecursionKind.NonRecursive, RecursionKind.Recursive)

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
      assert(p1sz == lets.size)
      // the result has distinct names
      assert(p1sz == p1.iterator.map(_._1).toSet.size)
      // recursiveness is not changed:
      assert(p1.map(_._2) == lets.map(_._2))
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
        Generators.bindIdentGen.map { b => (b, RecursionKind.Recursive, b) }
      )
    } yield lets

    forAll(genLets) { lets =>
      val p1 = SourceConverter.makeLetsUnique(lets) { (b, idx) =>
        val res = Identifier.Backticked(b.asString + s"____${idx}")
        (res, { (br: Bindable) => if (br == b) res else br })
      }

      p1.foreach { case (bl, _, br) =>
        assert(bl == br)
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
      assert(up1 == expectl1)
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
      assert(up1 == expectl1)
    }
  }
}
