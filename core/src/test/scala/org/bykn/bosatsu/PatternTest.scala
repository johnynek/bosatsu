package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{forAll, PropertyCheckConfiguration}

class PatternTest extends FunSuite {
  implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 300)

  val patGen = Gen.choose(0, 5).flatMap(Generators.genPattern(_))

  def pat(s: String): Pattern.Parsed =
    Pattern.bindParser.parse(s) match {
      case fastparse.all.Parsed.Success(p, idx) if idx == s.length => p
      case other => sys.error(s"could not parse $s, $other")
    }

  test("Pattern.unbind is the same as filterVars(Set.empty)") {
    forAll(patGen) { p =>
      assert(p.unbind == p.filterVars(Set.empty))
    }
  }

  test("filtering for names not in a pattern is unbind") {
    forAll(patGen, Gen.listOf(Gen.identifier)) { (p, ids0) =>
      val ids = ids0.map(Identifier.unsafe(_))
      assert(p.unbind == p.filterVars(ids.toSet.filterNot(p.names.toSet[Identifier])))
    }
  }

  test("filtering and keeping all names is identity") {
    forAll(patGen, Gen.listOf(Gen.identifier)) { (p, ids) =>
      assert(p.filterVars(p.names.toSet) == p)
    }
  }

  test("substructures is a subset of names") {
    forAll(patGen) { p =>
      p.substructures.toSet.subsetOf(p.names.toSet)
    }
  }

  test("singlynamed implies there is exacly one name") {
    forAll(patGen) { p =>
      p match {
        case Pattern.SinglyNamed(n) =>
          assert(p.names == (n :: Nil))
          // we can name with the same name, and still be singly named
          assert(Pattern.SinglyNamed.unapply(Pattern.Named(n, p)) == Some(n))
          // we can annotate and not lose singly named-ness
          assert(Pattern.SinglyNamed.unapply(Pattern.Annotation(p, null)) == Some(n))
          // we can make a union and not lose singly named-ness
          assert(Pattern.SinglyNamed.unapply(Pattern.union(Pattern.Var(n), p :: Nil)) == Some(n))
        case _ =>
      }
    }
  }

  test("test some examples for singly named") {
    def check(str: String, nm: String) =
      pat(str) match {
        case Pattern.SinglyNamed(n) => assert(n == Identifier.unsafe(nm))
        case other => fail(s"expected singlynamed: $other")
      }

    def checkNot(str: String) =
      pat(str) match {
        case Pattern.SinglyNamed(n) => fail(s"unexpected singlynamed: $n")
        case other => succeed
      }

    check("foo", "foo")
    check("foo@foo", "foo")
    check("foo@(foo@foo)", "foo")
    check("foo@Foo(_, _)", "foo")
    check("foo: T", "foo")
    check("foo@(_: T)", "foo")
    check("foo@(_, _)", "foo")
    check("foo | foo", "foo")
    check("foo@[*_]", "foo")
    check("(foo@X) | (foo@Y)", "foo")

    checkNot("foo | bar")
    checkNot("foo@Foo(x, y)")
    checkNot("foo@(x, y)")
    checkNot("_")
    checkNot("[foo]")
    checkNot("Foo(x, y)")
    checkNot("foo@bar")
    checkNot("Foo(foo)")
  }

  test("substructures don't include total matches") {
    val foo = Identifier.Name("foo")
    val bar = Identifier.Name("bar")
    assert(Pattern.Var(foo).substructures.isEmpty)
    assert(Pattern.Annotation(Pattern.Var(foo), "Type").substructures.isEmpty)
    assert(Pattern.Union(Pattern.Var(foo), NonEmptyList.of(Pattern.Var(bar))).substructures.isEmpty)
  }

  test("unions with total matches work correctly") {
    val foo = Identifier.Name("foo")
    val inner = Pattern.Var(foo)
    val struct = Pattern.PositionalStruct("Foo", inner :: Nil)
    // Note, foo can't be substructural because on the right it is total
    assert(Pattern.Union(struct, NonEmptyList.of(inner)).substructures.isEmpty)
  }
}
