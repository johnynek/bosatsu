package org.bykn.bosatsu.pattern

import org.bykn.bosatsu.Platform
import org.bykn.bosatsu.set.{Rel, SetOps, SetOpsLaws}
import cats.Eq
import org.scalacheck.Gen

import SeqPattern.{Cat, Empty}
import SeqPart.{AnyElem, Lit, Wildcard}

import cats.implicits._

class StringSeqPatternSetLaws extends SetOpsLaws[SeqPattern[Char]] {
  type Pattern = SeqPattern[Char]
  val Pattern = SeqPattern

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 500 else 10)
      .withMaxDiscardRatio(10)

  // if there are too many wildcards the intersections will blow up
  def genItem: Gen[Pattern] = StringSeqPatternGen.genPat.map { p =>
    Pattern.fromList(p.toList.take(4)).normalize
  }

  lazy val allStrings: LazyList[String] =
    // we only use 0/1 strings in Lit, but AnyElem can match others
    "" #:: (for {
      t <- allStrings // it's important to put this first
      h <- LazyList('0', '1', '2')
    } yield s"$h$t")

  val pmatcher = Pattern.stringUnitMatcher

  def matches(p: Pattern, s: String): Boolean = pmatcher(p)(s).isDefined

  def eqUnion: Gen[Eq[List[Pattern]]] =
    Gen
      .listOfN(
        5000,
        Gen.frequency(
          5 -> StringSeqPatternGen.genBitString,
          1 -> Gen.listOf(Gen.oneOf(List('0', '1', '2'))).map(_.mkString)
        )
      )
      .map { tests0 =>
        // we have to generate more than just 01 strings,
        // since Any can match more than that
        val tests = tests0 ::: allStrings.take(10000).toList
        new Eq[List[Pattern]] {
          // this can flake because if two things are different,
          // but happen to have the same match results for this
          // set of items, then you are hosed
          def eqv(a: List[Pattern], b: List[Pattern]) =
            (a, b) match {
              case (ah :: Nil, bh :: Nil) =>
                setOps.equiv(ah, bh)
              case _ =>
                (a.toSet == b.toSet) || tests.forall { s =>
                  a.exists(matches(_, s)) == b.exists(matches(_, s))
                }
            }

          override def toString = s"Eq via tests = $tests"
        }
      }

  implicit val setOpsChar: SetOps[Char] = SetOps.distinct[Char]
  val setOps: SetOps[Pattern] = Pattern.seqPatternSetOps[Char]

  test("subsetConsistencyLaw regressions") {
    import SeqPattern.{Cat, Empty}
    import SeqPart.Lit

    val regressions: List[(SeqPattern[Char], SeqPattern[Char])] =
      (
        Cat(Lit('1'), Cat(Lit('1'), Cat(Lit('1'), Cat(Lit('1'), Empty)))),
        Cat(Lit('0'), Cat(Lit('1'), Cat(Lit('1'), Empty)))
      ) ::
        (
          Cat(Lit('1'), Cat(Lit('0'), Cat(Lit('1'), Cat(Lit('0'), Empty)))),
          Cat(Lit('0'), Cat(Lit('1'), Empty))
        ) ::
        Nil

    regressions.foreach { case (a, b) =>
      subsetConsistencyLaw(a, b, Eq.fromUniversalEquals)
    }
  }

  test("*x* problems") {
    import SeqPattern.{Cat, Empty}
    import SeqPart.{Lit, Wildcard}

    val x = Cat(
      Wildcard,
      Cat(Lit('q'), Cat(Wildcard, Cat(Lit('p'), Cat(Wildcard, Empty))))
    )
    val y = Cat(Wildcard, Cat(Lit('p'), Cat(Wildcard, Empty)))
    val z = Cat(Wildcard, Cat(Lit('q'), Cat(Wildcard, Empty)))
    // note y and z are clearly bigger than x because they are prefix/suffix that end/start with
    // Wildcard
    assert(setOps.difference(x, y).isEmpty)
    assert(setOps.difference(x, z).isEmpty)
  }

  test("(a - b) n c = (a n c) - (b n c) regressions") {
    val regressions
        : List[(SeqPattern[Char], SeqPattern[Char], SeqPattern[Char])] =
      (
        Cat(Wildcard, Empty),
        Cat(AnyElem, Cat(Lit('1'), Cat(AnyElem, Empty))),
        Cat(AnyElem, Cat(Lit('1'), Cat(Lit('0'), Empty)))
      ) ::
        (
          Cat(Wildcard, Cat(Lit('0'), Empty)),
          Cat(AnyElem, Cat(Lit('1'), Cat(AnyElem, Cat(Lit('0'), Empty)))),
          Cat(AnyElem, Cat(Lit('1'), Cat(Lit('0'), Cat(Lit('0'), Empty))))
        ) ::
        (
          Cat(Wildcard, Cat(Lit('q'), Cat(Wildcard, Empty))),
          Cat(Wildcard, Empty),
          Cat(Wildcard, Cat(Lit('p'), Cat(Wildcard, Empty)))
        ) ::
        /*
         * This fails currently
         * see: https://github.com/johnynek/bosatsu/issues/486
      {
        val p1 = Cat(Wildcard,Cat(Lit('1'),Cat(Lit('0'),Cat(Lit('0'),Empty))))
        val p2  = Cat(AnyElem,Cat(Lit('1'),Cat(Wildcard,Cat(Lit('0'),Empty))))
        val p3 = Cat(Lit('1'),Cat(Lit('1'),Cat(Wildcard,Cat(Lit('0'),Empty))))
        (p1, p2, p3)
      } ::
         */
        Nil

    regressions.foreach { case (a, b, c) => diffIntersectionLaw(a, b, c) }
  }

  test("intersection regression") {
    val p1 = Cat(Wildcard, Cat(Lit('0'), Cat(Lit('1'), Empty)))
    val p2 = Cat(Lit('0'), Cat(Lit('0'), Cat(Lit('0'), Cat(Wildcard, Empty))))

    assert(setOps.relate(p1, p2) == Rel.Intersects)
    assert(setOps.relate(p2, p1) == Rel.Intersects)
    assert(setOps.intersection(p1, p2).nonEmpty)
    assert(setOps.intersection(p2, p1).nonEmpty)
  }
}
