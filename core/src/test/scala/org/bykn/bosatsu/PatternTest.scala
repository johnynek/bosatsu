package org.bykn.bosatsu

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
    forAll(patGen, Gen.listOf(Gen.identifier)) { (p, ids) =>
      assert(p.unbind == p.filterVars(ids.toSet.filterNot(p.names.toSet)))
    }
  }

  test("filtering and keeping all names is identity") {
    forAll(patGen, Gen.listOf(Gen.identifier)) { (p, ids) =>
      assert(p.filterVars(p.names.toSet) == p)
    }
  }
}
