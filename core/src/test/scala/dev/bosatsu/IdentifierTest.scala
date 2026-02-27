package dev.bosatsu

import cats.Order
import dev.bosatsu.hashing.{Algo, Hashable}

import scala.collection.immutable.SortedSet

class IdentifierTest extends munit.FunSuite {
  test("synthetic identifiers are disjoint from same-text non-synthetic names") {
    val synthetic = Identifier.synthetic("x")
    val backticked = Identifier.Backticked("_x")

    assert(synthetic.isInstanceOf[Identifier.Synthetic])
    assertNotEquals(synthetic, backticked)
    assertNotEquals(Order[Identifier].compare(synthetic, backticked), 0)

    val hashSynthetic = Hashable.hash(Algo.blake3Algo, synthetic).hash
    val hashBackticked = Hashable.hash(Algo.blake3Algo, backticked).hash
    assertNotEquals(hashSynthetic, hashBackticked)

    val asMap = Map[Identifier, Int](synthetic -> 1, backticked -> 2)
    assertEquals(asMap.size, 2)

    val asSortedSet = SortedSet.empty[Identifier] + synthetic + backticked
    assertEquals(asSortedSet.size, 2)
  }

  test("non-synthetic identifier compatibility by asString is preserved") {
    val name = Identifier.Name("same")
    val backticked = Identifier.Backticked("same")
    val nameAsIdent: Identifier = name
    val backtickedAsIdent: Identifier = backticked

    assertEquals(nameAsIdent, backtickedAsIdent)
    assertEquals(Order[Identifier].compare(name, backticked), 0)

    val hashName = Hashable.hash(Algo.blake3Algo, nameAsIdent).hash
    val hashBackticked = Hashable.hash(Algo.blake3Algo, backtickedAsIdent).hash
    assertEquals(hashName, hashBackticked)

    assertEquals(Set[Identifier](name, backticked).size, 1)
  }

  test("isSynthetic supports explicit and legacy synthetic encodings") {
    assert(Identifier.isSynthetic(Identifier.synthetic("fresh")))
    assert(Identifier.isSynthetic(Identifier.Name("_legacy")))
    assert(!Identifier.isSynthetic(Identifier.Backticked("_legacy")))
  }

  test("synthetic sourceCodeRepr round trips with bindableWithSynthetic") {
    val synthetic = Identifier.synthetic("roundtrip")
    val repr = synthetic.sourceCodeRepr

    assertEquals(repr, "_roundtrip")
    assertEquals(
      Identifier.bindableWithSynthetic.parseAll(repr),
      Right(synthetic)
    )
  }
}
