package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.forAll

class PatternTest extends FunSuite {
  val patGen = Gen.choose(0, 5).flatMap(Generators.genPattern(_))

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
