package org.bykn.bosatsu.pattern

import cats.Eq
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks.PropertyCheckConfiguration

import cats.implicits._

class StringSeqPatternSetLaws extends SetOpsLaws[SeqPattern[Char]] {
  type Pattern = SeqPattern[Char]
  val Pattern = SeqPattern

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 5)

  // if there are too many wildcards the intersections will blow up
  def genItem: Gen[Pattern] = StringSeqPatternGen.genPat.map { p =>
    Pattern.fromList(p.toList.take(4)).normalize
  }

  val pmatcher = Pattern.stringUnitMatcher

  def matches(p: Pattern, s: String): Boolean = pmatcher(p)(s).isDefined

  def eqUnion: Gen[Eq[List[Pattern]]] =
    Gen.listOfN(1000, StringSeqPatternGen.genBitString).map { tests =>
      new Eq[List[Pattern]] {
        // this can flake because if two things are different,
        // but happen to have the same match results for this
        // set of items, then you are hosed
        def eqv(a: List[Pattern], b: List[Pattern]) =
          (a.toSet == b.toSet) || tests.forall { s =>
            a.exists(matches(_, s)) == b.exists(matches(_, s))
          }
      }
    }

  implicit val setOpsChar: SetOps[Char] = SetOps.distinct[Char]
  val setOps: SetOps[Pattern] = Pattern.seqPatternSetOps[Char]

  test("subsetConsistencyLaw regressions") {
    import SeqPattern.{Cat, Empty}
    import SeqPart.Lit

    val regressions: List[(SeqPattern[Char], SeqPattern[Char])] =
      (Cat(Lit('1'),Cat(Lit('1'),Cat(Lit('1'),Cat(Lit('1'),Empty)))),
        Cat(Lit('0'),Cat(Lit('1'),Cat(Lit('1'),Empty)))) ::
      Nil

    regressions.foreach { case (a, b) => subsetConsistencyLaw(a, b, Eq.fromUniversalEquals) }
  }
}
