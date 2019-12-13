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
    Gen.listOfN(100, StringSeqPatternGen.genBitString).map { tests =>
      new Eq[List[Pattern]] {
        def eqv(a: List[Pattern], b: List[Pattern]) =
          tests.forall { s =>
            a.exists(matches(_, s)) == b.exists(matches(_, s))
          }
      }
    }

  implicit val setOpsChar: SetOps[Char] = SetOps.distinct[Char]
  val setOps: SetOps[Pattern] = Pattern.seqPatternSetOps[Char]
}
