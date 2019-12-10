package org.bykn.bosatsu.pattern

import cats.Eq
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

object StringSeqPatternGen {
  import StringSeqPattern._

  // generate a string of 0s and 1s to make matches more likely
  val genBitString: Gen[String] =
    for {
      sz <- Gen.choose(0, 4)
      g = Gen.oneOf(0, 1)
      list <- Gen.listOfN(sz, g)
    } yield list.mkString

  val genPart: Gen[Part] =
    Gen.frequency(
      (15, Gen.oneOf(Lit('0'), Lit('1'))),
      (2, Gen.const(AnyElem)),
      (1, Gen.const(Wildcard)))

  val genPat: Gen[Pattern] = {

    val cat = for {
      h <- genPart
      t <- genPat
    } yield Cat(h, t)

    Gen.frequency(
      (1, Gen.const(Empty)),
      (5, cat))
  }

  implicit val arbPattern: Arbitrary[Pattern] = Arbitrary(genPat)

  implicit lazy val shrinkPat: Shrink[Pattern] =
    Shrink {
      case Empty => Empty #:: Stream.empty
      case Cat(Wildcard, t) =>
        (Cat(AnyElem, t) #:: t #:: Stream.empty).flatMap(shrinkPat.shrink)
      case Cat(_, t) =>
        t #:: shrinkPat.shrink(t)
    }

  def genNamedFn(nextId: Int): Gen[(Int, Named)] = {
    lazy val recur = Gen.lzy(res)

    lazy val genNm: Gen[(Int, Named.Bind)] =
      recur.map { case (i, n) => (i + 1, Named.Bind(i.toString, n)) }

    // expected length = L = p_c * 2 * L + pn + L + pe * 0 + pp
    //
    // L = pp/(1 - (2*pc + pn))
    // so we need 2*pc + pn < 1
    // e.g. pc = 1/3, pn = 1/9 would work
    // L = (4/9) / (1 - (2/3 + 1/9)) = 4 / (9 - 5) = 1
    lazy val res: Gen[(Int, Named)] =
      Gen.frequency(
        (3, for {
          (i0, n0) <- recur
          (i1, n1) <- genNamedFn(i0)
        } yield (i1, Named.NCat(n0, n1))),
        (1, genNm),
        (1, Gen.const((nextId, Named.NEmpty))),
        (4, genPart.map { p => (nextId, Named.NPart(p)) }))

    res
  }

  val genNamed: Gen[Named] =
    genNamedFn(0).map(_._2)

  implicit val arbNamed: Arbitrary[Named] = Arbitrary(genNamed)

  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
    if (s1.isEmpty) s2 else if (s2.isEmpty) s1 else {
      s1.head #:: interleave(s2, s1.tail)
    }

  implicit val shrinkNamed: Shrink[Named] =
    Shrink {
      case Named.NEmpty => Stream.Empty
      case Named.Bind(n, p) =>
        val sp = shrinkNamed.shrink(p)
        val binded = sp.map(Named.Bind(n, _))
        interleave(sp, binded)
      case Named.NPart(p) =>
        val sp = p match {
          case Wildcard => AnyElem #:: Lit('0') #:: Lit('1') #:: Stream.Empty
          case AnyElem => Lit('0') #:: Lit('1') #:: Stream.Empty
          case Lit(_) => Stream.Empty
        }
        sp.map(Named.NPart(_))
      case Named.NCat(fst, snd) =>
        val s1 = shrinkNamed.shrink(fst)
        val s2 = shrinkNamed.shrink(snd)
        interleave(s1, s2).iterator.sliding(2).map {
          case Seq(a, b) => Named.NCat(a, b)
          case _ => Named.NEmpty
        }
        .toStream
    }

  def unany(p: Pattern): Pattern =
    p match {
      case Empty => Empty
      case Cat(AnyElem, t) => unany(t)
      case Cat(h, t) => Cat(h, unany(t))
    }

  def unwild(p: Pattern): Pattern =
    p match {
      case Empty => Empty
      case Cat(Wildcard, t) => unwild(t)
      case Cat(h, t) => Cat(h, unwild(t))
    }

  implicit val arbString: Arbitrary[String] = Arbitrary(genBitString)
  implicit val shrinkString: Shrink[String] =
    Shrink { str =>
      for {
        i <- (0 until str.length).toStream
        j <- (i until str.length).toStream
      } yield str.substring(i, j)
    }

}

abstract class SeqPatternLaws[E, S] extends FunSuite {
  val seqPattern: SeqPattern.Aux[E, S]

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 5)

  import seqPattern._

  def genPattern: Gen[Pattern]
  def genNamed: Gen[Named]
  def genSeq: Gen[S]

  test("reverse is idempotent") {
    forAll(genPattern) { p: Pattern =>
      assert(p.reverse.reverse == p)
    }
  }

  test("reverse matches the reverse string") {
    forAll(genPattern, genSeq) { (p: Pattern, str: Sequence) =>
      val rstr = listToSeq(seqToList(str).reverse)
      assert(p.matches(str) == p.reverse.matches(rstr), s"p.reverse = ${p.reverse}")
    }
  }

  test("unlit patterns match everything") {
    def unlit(p: Pattern): Pattern =
      p match {
        case Empty => Empty
        case Cat(Lit(_) | AnyElem, t) => unlit(t)
        case Cat(h, t) => Cat(h, unlit(t))
      }

    forAll(genPattern, genSeq) { (p0, str) =>
      val p = unlit(p0)
      assert(matches(p, str) || (p == Empty))
    }
  }

  test("normalized patterns have the same matches as unnormalized ones") {
    forAll(genPattern, genSeq) { (p0, s) =>
      val pnorm = p0.normalize
      assert(matches(pnorm, s) == matches(p0, s))
    }
  }

  test("normalized patterns don't have adjacent Wildcard") {
    forAll(genPattern) { p0 =>
      val list = p0.normalize.toList
      list.sliding(2).foreach {
        case bad@Seq(Wildcard, Wildcard) =>
          fail(s"saw adjacent: $bad in ${p0.normalize}")
        case _ => ()
      }
    }
  }

  def intersectionMatchLaw(p10: Pattern, p20: Pattern, x: S) = {
    // intersections get really slow if they get too big
    val p1 = Pattern.fromList(p10.toList.take(5))
    val p2 = Pattern.fromList(p20.toList.take(5))
    val n1 = p1.normalize
    val n2 = p2.normalize
    val rawintr = p1.intersection(p2)
    val intersect = rawintr.map(_.normalize).distinct
    val sep = p1.matches(x) && p2.matches(x)
    val together = intersect.exists(_.matches(x))

    assert(together == sep, s"n1: $n1, n2: $n2, intersection: $intersect")
    //if (together != sep) sys.error(s"n1: $n1, n2: $n2, intersection: $intersect")
    //else succeed
  }

  test("intersection(p1, p2).matches(x) == p1.matches(x) && p2.matches(x)") {
    forAll(genPattern, genPattern, genSeq)(intersectionMatchLaw(_, _, _))
  }

  test("matches any does match any") {
    forAll(genPattern, genSeq) { (p: Pattern, s: S) =>
      if (p.matchesAny) assert(p.matches(s))
    }
  }

  test("matchesEmpty is right") {
    forAll(genPattern) { p: Pattern =>
      if (p.matchesEmpty) assert(p.matches(emptySeq))
    }
  }

  test("toList/fromList match the same") {
    forAll(genPattern, genSeq) { (p, s) =>
      val p1 = Pattern.fromList(p.toList)
      assert(p1.matches(s) == p.matches(s), s"p1 = $p1")
    }
  }

  test("isEmpty only matches empty") {
    forAll(genPattern, genSeq) { (p: Pattern, s: S) =>
      if (p.isEmpty) {
        assert(p.matches(emptySeq))
        if (s != emptySeq) {
          assert(!p.matches(s))
        }
      }
    }
  }

  test("unify union preserves matching") {
    forAll(Gen.listOf(genPattern), genSeq) { (ps: List[Pattern], s: S) =>
      val u1 = ps.exists(_.matches(s))
      val unified = seqPattern.Pattern.patternSetOps.unifyUnion(ps)
      val u2 = unified.exists(_.matches(s))

      assert(u2 == u1, s"unified = $unified")
    }
  }

  def subsetConsistentWithMatchLaw(a: Pattern, b: Pattern, s: S) = {
    if (a.matches(s)) {
      if (seqPattern.Pattern.patternSetOps.subset(a, b)) {
        assert(b.matches(s))
      }
    }
  }

  test("if subset(a, b) then matching a implies matching b") {
    forAll(genPattern, genPattern, genSeq)(subsetConsistentWithMatchLaw(_, _, _))
  }

  test("difference is an upper bound") {
    forAll(genPattern, genPattern, genSeq) { (p10: Pattern, p20: Pattern, s: S) =>
      // intersections get really slow if they get too big
      val p1 = Pattern.fromList(p10.toList.take(5))
      val p2 = Pattern.fromList(p20.toList.take(5))
      // true difference is <= diff
      val diff = seqPattern.Pattern.patternSetOps.difference(p1, p2)
      val diffmatch = diff.exists(_.matches(s))

      if (diffmatch) assert(p1.matches(s), s"diff = $diff")

      if (p1.matches(s) && !p2.matches(s)) {
        assert(diffmatch, s"diff = $diff")
      }

      if (diff.isEmpty && p1.matches(s)) {
        assert(p2.matches(s))
      }

      if (p2.matchesAny) assert(diff == Nil)

      // the law we wish we had:
      //if (p2.matches(s) && p1.matches(s)) assert(!diffmatch)
    }
  }

  test("p + q match (s + t) if p.matches(s) && q.matches(t)") {
    forAll(genPattern, genPattern, genSeq, genSeq) { (p: Pattern, q: Pattern, s: S, t: S) =>
      if (p.matches(s) && q.matches(t)) {
        assert((p + q).matches(catSeq(s, t)))
      }
    }
  }


  def namedMatchesPatternLaw(n: Named, str: S) = {
    val p = n.unname

    assert(n.matches(str).isDefined == p.matches(str), s"${p.show}")
  }

  test("Named.matches agrees with Pattern.matches") {
    forAll(genNamed, genSeq)(namedMatchesPatternLaw(_, _))
  }

/*
  test("if x - y is empty, (x + z) - (y + z) is empty") {
    forAll { (x0: Pattern, y0: Pattern, z0: Pattern) =>
      val x = Pattern.fromList(x0.toList.take(3))
      val y = Pattern.fromList(y0.toList.take(3))
      val z = Pattern.fromList(z0.toList.take(3))
      if (x.difference(y).isEmpty) {
        assert((x + z).difference(y + z) == Nil)
      }
    }
  }
*/
}


class StringSeqPatternSetLaws extends SetOpsLaws[StringSeqPattern.Pattern] {
  import StringSeqPattern._

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 5)

  // if there are too many wildcards the intersections will blow up
  def genItem: Gen[Pattern] = StringSeqPatternGen.genPat.map { p =>
    Pattern.fromList(p.toList.take(4)).normalize
  }

  def eqUnion: Gen[Eq[List[Pattern]]] =
    Gen.listOfN(100, StringSeqPatternGen.genBitString).map { tests =>
      new Eq[List[Pattern]] {
        def eqv(a: List[Pattern], b: List[Pattern]) =
          tests.forall { s =>
            a.exists(_.matches(s)) == b.exists(_.matches(s))
          }
      }
    }

  val setOps: SetOps[Pattern] = Pattern.patternSetOps
}

class SeqPatternTest extends SeqPatternLaws[Char, String] {

  import StringSeqPattern._
  import Pattern.patternSetOps._

  lazy val seqPattern = StringSeqPattern
  def genPattern = StringSeqPatternGen.genPat
  def genNamed = StringSeqPatternGen.genNamed
  def genSeq = StringSeqPatternGen.genBitString

  import StringSeqPatternGen._

  test("some matching examples") {
    val matches: List[(Pattern, String)] =
      (Pattern.Wild + Pattern.Any + Pattern.Any + toPattern("1"), "111") ::
      (toPattern("1") + Pattern.Any + toPattern("1"), "111") ::
      Nil

    matches.foreach { case (p, s) => assert(p.matches(s), s"$p.matches($s)") }
  }

  test("wildcard on either side is the same as contains") {
    forAll { (ps: String, s: String) =>
      assert(matches(Pattern.Wild + toPattern(ps) + Pattern.Wild, s) == s.contains(ps))
    }
  }
  test("wildcard on front side is the same as endsWith") {
    forAll { (ps: String, s: String) =>
      assert(matches(Pattern.Wild + toPattern(ps), s) == s.endsWith(ps))
    }
  }
  test("wildcard on back side is the same as startsWith") {
    forAll { (ps: String, s: String) =>
      assert(matches(toPattern(ps) + Pattern.Wild, s) == s.startsWith(ps))
    }
  }

  test("intersection(p1, p2).matches(x) == p1.matches(x) && p2.matches(x) regressions") {
    val regressions: List[(Pattern, Pattern, String)] =
      (toPattern("0") + Pattern.Any + Pattern.Wild, Pattern.Any + toPattern("01") + Pattern.Any, "001") ::
      (Pattern.Wild + Pattern.Any + Pattern.Any + toPattern("1"), toPattern("1") + Pattern.Any + toPattern("1"), "111") ::
      Nil

    regressions.foreach { case (p1, p2, s) => intersectionMatchLaw(p1, p2, s) }
  }

  test("subset is consistent with match regressions") {
    assert(subset(toPattern("00") + Pattern.Wild, toPattern("0") + Pattern.Wild))
    assert(subset(toPattern("00") + Pattern.Any + Pattern.Wild, toPattern("0") + Pattern.Any + Pattern.Wild))
  }

  test("if y - x is empty, (yz - xz) for all strings is empty") {
    def law(x0: Pattern, y0: Pattern, str: String) = {
      val x = Pattern.fromList(x0.toList.take(5))
      val y = Pattern.fromList(y0.toList.take(5))
      if (y.difference(x) == Nil) {
        assert((y + toPattern(str)).difference(x + toPattern(str)) == Nil)
      }
    }

    forAll(genPattern, genPattern, genSeq)(law(_, _, _))
  }

  test("if y - x is empty, (zy - zx) for all strings is empty") {
    forAll(genPattern, genPattern, genSeq) { (x0: Pattern, y0: Pattern, str: String) =>
      val x = Pattern.fromList(x0.toList.take(5))
      val y = Pattern.fromList(y0.toList.take(5))
      if (y.difference(x) == Nil) {
        assert((toPattern(str) + y).difference(toPattern(str) + x) == Nil)
      }
    }
  }

  test("Named.matches + render agree") {
    forAll(genNamed, genSeq) { (n: Named, str: String) =>
      n.matches(str).foreach { m =>
        n.render(m)(c => c) match {
          case Some(s0) => assert(s0 == str)
          case None =>
            // this can only happen if we have unnamed Wild/AnyElem
            assert(n.isRenderable == false)
        }
      }
    }
  }

  def named(s: String): Named =
    Named.fromPattern(toPattern(s))

  test("Named.matches agrees with Pattern.matches regressions") {

    import Named._
    val regressions: List[(Named, String)] =
      (NCat(NEmpty,NCat(NPart(Lit('1')),NPart(Wildcard))), "1") ::
      (NCat(NPart(Lit('1')),NPart(Wildcard)), "1") ::
      (NPart(Lit('1')), "1") ::
      Nil

    regressions.foreach { case (n, s) => namedMatchesPatternLaw(n, s) }
  }

  test("Test some examples of Named matching") {
    // foo@("bar" baz@(*"baz"))
    val p1 = (named("bar") + (Named.Wild + named("baz")).name("baz")).name("foo")
    assert(p1.matches("bar and baz") == Some(Map("foo" -> "bar and baz", "baz" -> " and baz")))
  }
}
