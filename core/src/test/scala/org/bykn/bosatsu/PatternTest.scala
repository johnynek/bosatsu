package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite

class PatternTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 300)

  val patGen = Gen.choose(0, 5).flatMap(Generators.genPattern(_))

  def pat(s: String): Pattern.Parsed =
    Parser.unsafeParse(Pattern.bindParser, s)

  test("Pattern.unbind is the same as filterVars(Set.empty)") {
    forAll(patGen) { p =>
      assert(p.unbind == p.filterVars(Set.empty))
    }
  }

  test("filtering for names not in a pattern is unbind") {
    forAll(patGen, Gen.listOf(Gen.identifier)) { (p, ids0) =>
      val ids = ids0.map(Identifier.unsafe(_))
      assert(
        p.unbind == p.filterVars(ids.toSet.filterNot(p.names.toSet[Identifier]))
      )
    }
  }

  test("filtering and keeping all names is identity") {
    forAll(patGen) { p =>
      assert(p.filterVars(p.names.toSet) == p)
    }
  }

  test("substructures is a subset of names") {
    forAll(patGen) { p =>
      p.substructures.toSet.subsetOf(p.names.toSet)
    }
  }

  test("topNames is a subset of names") {
    forAll(patGen) { p =>
      p.topNames.toSet.subsetOf(p.names.toSet)
    }
  }

  test("topNames and substructures are disjoint") {
    forAll(patGen) { p =>
      p.topNames.toSet.intersect(p.substructures.toSet).isEmpty
    }
  }

  test("singlynamed implies there is exacly one name") {
    def law(p: Pattern.Parsed) =
      p match {
        case Pattern.SinglyNamed(n) =>
          assert(p.topNames == (n :: Nil))
          assert(p.names == (n :: Nil))
          // we can name with the same name, and still be singly named
          assert(Pattern.SinglyNamed.unapply(Pattern.Named(n, p)) == Some(n))
          // we can annotate and not lose singly named-ness
          assert(
            Pattern.SinglyNamed.unapply(Pattern.Annotation(p, null)) == Some(n)
          )
          // we can make a union and not lose singly named-ness
          assert(
            Pattern.SinglyNamed.unapply(
              Pattern.union(Pattern.Var(n), p :: Nil)
            ) == Some(n)
          )
        case _ =>
      }

    forAll(patGen) { p => law(p) }
    law(
      Pattern.Named(
        Identifier.Name("x"),
        Pattern.Named(Identifier.Name("x"), Pattern.WildCard)
      )
    )
  }

  test("test some examples for singly named") {
    def check(str: String, nm: String) =
      pat(str) match {
        case Pattern.SinglyNamed(n) => assert(n == Identifier.unsafe(nm))
        case other                  => fail(s"expected singlynamed: $other")
      }

    def checkNot(str: String) =
      pat(str) match {
        case Pattern.SinglyNamed(n) => fail(s"unexpected singlynamed: $n")
        case _                      => succeed
      }

    check("foo", "foo")
    check("foo as foo", "foo")
    check("(foo as foo) as foo", "foo")
    check("Foo(_, _)as foo", "foo")
    check("foo: T", "foo")
    check("(_: T) as foo", "foo")
    check("(_, _) as foo", "foo")
    check("foo | foo", "foo")
    check("[*_] as foo", "foo")
    check("(X as foo) | (Y as foo)", "foo")

    checkNot("foo | bar")
    checkNot("Foo(x, y)as foo")
    checkNot("(x, y)as foo")
    checkNot("_")
    checkNot("[foo]")
    checkNot("Foo(x, y)")
    checkNot("bar as foo")
    checkNot("Foo(foo)")
  }

  test("substructures don't include total matches") {
    val foo = Identifier.Name("foo")
    val bar = Identifier.Name("bar")
    assert(Pattern.Var(foo).substructures.isEmpty)
    assert(Pattern.Annotation(Pattern.Var(foo), "Type").substructures.isEmpty)
    assert(
      Pattern
        .Union(Pattern.Var(foo), NonEmptyList.of(Pattern.Var(bar)))
        .substructures
        .isEmpty
    )
  }

  test("unions with total matches work correctly") {
    val foo = Identifier.Name("foo")
    val inner = Pattern.Var(foo)
    val struct = Pattern.PositionalStruct("Foo", inner :: Nil)
    // Note, foo can't be substructural because on the right it is total
    assert(Pattern.Union(struct, NonEmptyList.of(inner)).substructures.isEmpty)
  }
}
