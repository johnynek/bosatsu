package dev.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class InSetCompilerTest extends munit.ScalaCheckSuite {
  private def membersFromMask(size: Int, mask: Int): NonEmptyList[Int] =
    NonEmptyList.fromListUnsafe(
      (0 until size).toList.filter(idx => ((mask >> idx) & 1) == 1)
    )

  private def comparisonMasks(size: Int): Set[Int] = {
    val universe = (1 << size) - 1
    val eqAndNe = (0 until size).flatMap { idx =>
      val eq = 1 << idx
      val ne = universe ^ eq
      eq :: ne :: Nil
    }
    val bounds = (0 to size).flatMap { idx =>
      val lt =
        if (idx == 0) 0
        else (1 << idx) - 1
      val ge = universe ^ lt
      lt :: ge :: Nil
    }
    (eqAndNe ++ bounds).toSet
  }

  private def minimalComparisonCosts(size: Int): Array[Int] = {
    val stateCount = 1 << size
    val universe = stateCount - 1
    val inf = Int.MaxValue / 4
    val costs = Array.fill(stateCount)(inf)
    costs(0) = 0
    costs(universe) = 0

    comparisonMasks(size).foreach { mask =>
      if ((mask != 0) && (mask != universe)) costs(mask) = 1
    }

    var changed = true
    while (changed) {
      changed = false

      var mask = 0
      while (mask < stateCount) {
        val cost = costs(mask)
        if (cost < inf) {
          val comp = universe ^ mask
          if (cost < costs(comp)) {
            costs(comp) = cost
            changed = true
          }
        }
        mask += 1
      }

      val finite = (0 until stateCount).filter(mask => costs(mask) < inf).toArray
      var i = 0
      while (i < finite.length) {
        val left = finite(i)
        val leftCost = costs(left)
        var j = 0
        while (j < finite.length) {
          val right = finite(j)
          val sum = leftCost + costs(right)

          val union = left | right
          if (sum < costs(union)) {
            costs(union) = sum
            changed = true
          }

          val inter = left & right
          if (sum < costs(inter)) {
            costs(inter) = sum
            changed = true
          }

          j += 1
        }
        i += 1
      }
    }

    costs
  }

  private val costCache = scala.collection.mutable.Map.empty[Int, Array[Int]]
  private def minimalCost(size: Int, mask: Int): Int =
    costCache.getOrElseUpdate(size, minimalComparisonCosts(size))(mask)

  test("InSetCompiler.eval matches naive set membership") {
    val gen = for {
      size <- Gen.choose(1, 12)
      mask <- Gen.chooseNum(1, (1 << size) - 1)
    } yield (size, mask)

    forAll(gen) { case (size, mask) =>
      val members = membersFromMask(size, mask)
      val compiled = InSetCompiler.compile(size, members)
      (0 until size).foreach { idx =>
        val naive = ((mask >> idx) & 1) == 1
        assertEquals(InSetCompiler.eval(compiled, idx), naive)
      }
    }
  }

  test("InSetCompiler minimizes comparison count for small sets") {
    val maxSize = 8
    (1 to maxSize).foreach { size =>
      val upper = (1 << size) - 1
      (1 to upper).foreach { mask =>
        val members = membersFromMask(size, mask)
        val compiled = InSetCompiler.compile(size, members)
        val expected = minimalCost(size, mask)
        val actual = InSetCompiler.comparisonCount(compiled)
        assertEquals(
          actual,
          expected,
          s"size=$size mask=$mask formula=$compiled"
        )
      }
    }
  }
}
