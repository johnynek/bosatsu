package dev.bosatsu

import cats.instances.all.*
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

class NullableTest extends munit.ScalaCheckSuite:

  given [A: Arbitrary]: Arbitrary[Nullable[A]] =
    Arbitrary(Arbitrary.arbitrary[Option[A]].map(Nullable.fromOption(_)))

  property("map isomorphic to Option.map") {
    forAll { (n: Nullable[Int], fn: Int => String) =>
      assertEquals(n.map(fn).toOption, n.toOption.map(fn))
    }
  }

  property("flatMap isomorphic to Option.flatMap") {
    forAll { (n: Nullable[Int], fn: Int => Option[String]) =>
      assertEquals(
        n.flatMap(i => Nullable.fromOption(fn(i))).toOption,
        n.toOption.flatMap(fn)
      )
    }
  }

  property("fold isomorphic to Option.fold") {
    forAll { (n: Nullable[Int], ifNull: Long, fn: Int => Long) =>
      val left: Long = n.fold(ifNull)(fn)
      val right: Long = n.toOption.fold(ifNull)(fn)
      assertEquals(left, right)
    }
  }

  property("isNull/nonNull match Option emptiness") {
    forAll { (n: Nullable[Int]) =>
      assertEquals(n.isNull, n.toOption.isEmpty)
      assertEquals(n.nonNull, n.toOption.nonEmpty)
    }
  }

  property("iterator isomorphic to Option.iterator") {
    forAll { (n: Nullable[Int]) =>
      assertEquals(n.iterator.toList, n.toOption.iterator.toList)
    }
  }

  property("fromOption(toOption) round trips") {
    forAll { (n: Nullable[Int]) =>
      assert(Nullable.fromOption(n.toOption) === n)
    }
  }

  test("fromOption(Option(null)) equals Nullable(null)") {
    val left: Nullable[String] = Nullable.fromOption(Option(null.asInstanceOf[String]))
    val right: Nullable[String] = Nullable(null)
    assert(left === right)
  }

  test("works for literal lambdas in fold using == null") {
    given CanEqual[String | Null, Null] = CanEqual.derived

    val nonNull: Nullable[String | Null] = Nullable("abc")
    val isNull: Nullable[String | Null] = Nullable.empty

    assert(!nonNull.fold(false)(_ == null))
    assert(!isNull.fold(false)(_ == null))
  }

  test("works for literal lambdas in fold using != null") {
    given CanEqual[String | Null, Null] = CanEqual.derived

    val nonNull: Nullable[String | Null] = Nullable("abc")
    val isNull: Nullable[String | Null] = Nullable.empty

    assert(nonNull.fold(true)(_ != null))
    assert(isNull.fold(true)(_ != null))
  }
