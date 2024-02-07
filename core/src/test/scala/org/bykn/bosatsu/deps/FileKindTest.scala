package org.bykn.bosatsu.deps

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

class FileKindTest extends munit.ScalaCheckSuite {
  val genFileKind = Gen.oneOf[FileKind](FileKind.Source, FileKind.Iface, FileKind.Pack)

  property("ordering works") {
    forAll(genFileKind, genFileKind, genFileKind) { (a, b, c) =>
      org.bykn.bosatsu.OrderingLaws.law(a, b, c)
    }
  }
}