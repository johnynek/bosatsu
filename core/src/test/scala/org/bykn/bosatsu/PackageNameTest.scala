package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import rankn.NTypeGen.packageNameGen

class PackageNameTest extends munit.ScalaCheckSuite {
  property("ordering is lawful") {
    forAll(packageNameGen, packageNameGen, packageNameGen)(OrderingLaws.law(_, _, _))
  }

  property("distinctNonPredef does what it says") {
    forAll(Gen.listOf(packageNameGen)) { packs =>
      assertEquals(PackageName.distinctNonPredef(packs),
        packs.distinct.sorted.filterNot(_ == PackageName.PredefName))
    }
  }

  property("distinctNonPredef doesn't have predef") {
    assertEquals(PackageName.distinctNonPredef(PackageName.PredefName :: Nil), Nil)
  }
}