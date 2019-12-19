package org.bykn.bosatsu.pattern

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

import SeqPattern.{Cat, Empty}

import cats.implicits._

object StringSeqPatternGen {
  import SeqPart.{Wildcard, AnyElem, Lit}

  type Named = NamedSeqPattern[Char]
  val Named = NamedSeqPattern

  // generate a string of 0s and 1s to make matches more likely
  val genBitString: Gen[String] =
    for {
      sz <- Gen.choose(0, 4)
      g = Gen.oneOf(0, 1)
      list <- Gen.listOfN(sz, g)
    } yield list.mkString

  val genPart: Gen[SeqPart[Char]] = {
    import SeqPart._

    Gen.frequency(
      (15, Gen.oneOf(Lit('0'), Lit('1'))),
      (2, Gen.const(AnyElem)),
      (1, Gen.const(Wildcard)))
  }

  val genPat: Gen[SeqPattern[Char]] = {

    val cat = for {
      h <- genPart
      t <- genPat
    } yield Cat(h, t)

    Gen.frequency(
      (1, Gen.const(Empty)),
      (5, cat))
  }

  implicit val arbPattern: Arbitrary[SeqPattern[Char]] = Arbitrary(genPat)

  implicit lazy val shrinkPat: Shrink[SeqPattern[Char]] =
    Shrink {
      case Empty => Empty #:: Stream.empty
      case Cat(Wildcard, t) =>
        (Cat(AnyElem, t) #:: t #:: Stream.empty).flatMap(shrinkPat.shrink)
      case Cat(_, t) =>
        t #:: shrinkPat.shrink(t)
    }

  def genNamedFn(nextId: Int): Gen[(Int, Named)] = {
    lazy val recur = Gen.lzy(res)

    lazy val genNm: Gen[(Int, Named.Bind[Char])] =
      recur.map { case (i, n) => (i + 1, Named.Bind(i.toString, n)) }

    // expected length = L = p_c * 2 * L + pn + L + pe * 0 + pp
    //
    // L = pp/(1 - (2*pc + pn))
    // so we need 2*pc + pn < 1
    // e.g. pc = 1/3, pn = 1/9 would work
    // L = (4/9) / (1 - (2/3 + 1/9)) = 4 / (9 - 5) = 1
    lazy val res: Gen[(Int, NamedSeqPattern[Char])] =
      Gen.frequency(
        (3, for {
          (i0, n0) <- recur
          (i1, n1) <- genNamedFn(i0)
        } yield (i1, NamedSeqPattern.NCat(n0, n1))),
        (1, genNm),
        (1, Gen.const((nextId, NamedSeqPattern.NEmpty))),
        (4, genPart.map { p => (nextId, NamedSeqPattern.NSeqPart(p)) }))

    res
  }

  val genNamed: Gen[NamedSeqPattern[Char]] =
    genNamedFn(0).map(_._2)

  implicit val arbNamed: Arbitrary[NamedSeqPattern[Char]] = Arbitrary(genNamed)

  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
    if (s1.isEmpty) s2 else if (s2.isEmpty) s1 else {
      s1.head #:: interleave(s2, s1.tail)
    }

  implicit val shrinkNamedSeqPattern: Shrink[NamedSeqPattern[Char]] =
    Shrink {
      case NamedSeqPattern.NEmpty => Stream.Empty
      case NamedSeqPattern.Bind(n, p) =>
        val sp = shrinkNamedSeqPattern.shrink(p)
        val binded = sp.map(NamedSeqPattern.Bind(n, _))
        interleave(sp, binded)
      case NamedSeqPattern.NSeqPart(p) =>
        def tail: Stream[SeqPart[Char]] =
          Lit('0') #:: Lit('1') #:: Stream.Empty

        val sp = p match {
          case Wildcard => AnyElem #:: tail
          case AnyElem => tail
          case Lit(_) => Stream.Empty
        }
        sp.map(NamedSeqPattern.NSeqPart(_))
      case NamedSeqPattern.NCat(fst, snd) =>
        val s1 = shrinkNamedSeqPattern.shrink(fst)
        val s2 = shrinkNamedSeqPattern.shrink(snd)
        interleave(s1, s2).iterator.sliding(2).map {
          case Seq(a, b) => NamedSeqPattern.NCat(a, b)
          case _ => NamedSeqPattern.NEmpty
        }
        .toStream
    }

  def unany[A](p: SeqPattern[A]): SeqPattern[A] =
    p match {
      case Empty => Empty
      case Cat(AnyElem, t) => unany(t)
      case Cat(h, t) => Cat(h, unany(t))
    }

  def unwild[A](p: SeqPattern[A]): SeqPattern[A] =
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

abstract class SeqPatternLaws[E, I, S, R] extends FunSuite {
  import SeqPart.{Wildcard, AnyElem, Lit}

  type Pattern = SeqPattern[E]
  val Pattern = SeqPattern
  type Named = NamedSeqPattern[E]
  val Named = NamedSeqPattern

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 5)

  def genPattern: Gen[Pattern]
  def genNamed: Gen[Named]
  def genSeq: Gen[S]
  def splitter: Splitter[E, I, S, R]
  def setOps: SetOps[Pattern]

  test("reverse is idempotent") {
    forAll(genPattern) { p: Pattern =>
      assert(p.reverse.reverse == p)
    }
  }

  test("cat.reverse == cat.reverseCat") {
    forAll(genPattern) { p =>
      p match {
        case Empty => assert(p.reverse == Empty)
        case c@Cat(_, _) => assert(c.reverseCat == c.reverse)
      }
    }
  }

  def matches(p: Pattern, s: S): Boolean
  def namedMatches(p: Named, s: S): Boolean

  test("reverse matches the reverse string") {
    forAll(genPattern, genSeq) { (p: Pattern, str: S) =>
      val rstr = splitter.fromList(splitter.toList(str).reverse)
      assert(matches(p, str) == matches(p.reverse, rstr), s"p.reverse = ${p.reverse}")
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
    val rawintr = setOps.intersection(p1, p2)
    val intersect = rawintr.map(_.normalize).distinct
    val sep = matches(p1, x) && matches(p2, x)
    val together = intersect.exists(matches(_, x))

    assert(together == sep, s"n1: $n1, n2: $n2, intersection: $intersect")
    //if (together != sep) sys.error(s"n1: $n1, n2: $n2, intersection: $intersect")
    //else succeed
  }

  test("intersection(p1, p2).matches(x) == p1.matches(x) && p2.matches(x)") {
    forAll(genPattern, genPattern, genSeq)(intersectionMatchLaw(_, _, _))
  }

  test("matches any does match any") {
    forAll(genPattern, genSeq) { (p: Pattern, s: S) =>
      if (p.matchesAny) assert(matches(p, s))
    }
  }

  test("matchesEmpty is right") {
    forAll(genPattern) { p: Pattern =>
      if (p.matchesEmpty) assert(matches(p, splitter.emptySeq))
    }
  }

  test("toList/fromList match the same") {
    forAll(genPattern, genSeq) { (p, s) =>
      val p1 = Pattern.fromList(p.toList)
      assert(matches(p1, s) == matches(p, s), s"p1 = $p1")
    }
  }

  test("isEmpty only matches empty") {
    forAll(genPattern, genSeq) { (p: Pattern, s: S) =>
      if (p.isEmpty) {
        assert(matches(p, splitter.emptySeq))
        if (s != splitter.emptySeq) {
          assert(!matches(p, s))
        }
      }
    }
  }

  test("unify union preserves matching") {
    forAll(Gen.listOf(genPattern), genSeq) { (ps: List[Pattern], s: S) =>
      val u1 = ps.exists(matches(_, s))
      val unified = setOps.unifyUnion(ps)
      val u2 = unified.exists(matches(_, s))

      assert(u2 == u1, s"unified = $unified")
    }
  }

  def subsetConsistentWithMatchLaw(a: Pattern, b: Pattern, s: S) = {
    if (matches(a, s)) {
      if (setOps.subset(a, b)) {
        assert(matches(b, s))
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
      val diff = setOps.difference(p1, p2)
      val diffmatch = diff.exists(matches(_, s))

      if (diffmatch) assert(matches(p1, s), s"diff = $diff")

      if (matches(p1, s) && !matches(p2, s)) {
        assert(diffmatch, s"diff = $diff")
      }

      if (diff.isEmpty && matches(p1, s)) {
        assert(matches(p2, s))
      }

      if (p2.matchesAny) assert(diff == Nil)

      // the law we wish we had:
      //if (p2.matches(s) && p1.matches(s)) assert(!diffmatch)
    }
  }

  test("p + q match (s + t) if p.matches(s) && q.matches(t)") {
    forAll(genPattern, genPattern, genSeq, genSeq) { (p: Pattern, q: Pattern, s: S, t: S) =>
      if (matches(p, s) && matches(q, t)) {
        assert(matches(p + q, splitter.catSeqs(s :: t :: Nil)))
      }
    }
  }


  def namedMatchesPatternLaw(n: Named, str: S) = {
    val p = n.unname

    assert(namedMatches(n, str) == matches(p, str), s"${p.show}")
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


class SeqPatternTest extends SeqPatternLaws[Char, Char, String, Unit] {
  import SeqPart._

  def genPattern = StringSeqPatternGen.genPat
  def genNamed = StringSeqPatternGen.genNamed
  def genSeq = StringSeqPatternGen.genBitString

  def splitter: Splitter[Char, Char, String, Unit] =
    Splitter.stringSplitter(_ => ())

  val pmatcher = Pattern.matcher(splitter)

  def matches(p: Pattern, s: String): Boolean = pmatcher(p)(s).isDefined
  def namedMatches(p: Named, s: String): Boolean =
    NamedSeqPattern.matcher(splitter)(p)(s).isDefined

  def namedMatch(p: Named, s: String): Option[Map[String, String]] =
    NamedSeqPattern.matcher(Splitter.stringSplitter(_.toString))(p)(s).map(_._2)

  implicit val setOpsChar: SetOps[Char] = SetOps.distinct[Char]
  def setOps: SetOps[Pattern] = Pattern.seqPatternSetOps[Char]

  import StringSeqPatternGen._

  def toPattern(s: String): Pattern =
    s.toList.foldRight(Pattern.Empty: Pattern) { (c, r) =>
      Pattern.Cat(Lit(c), r)
    }

  test("some matching examples") {
   val ms: List[(Pattern, String)] =
      (Pattern.Wild + Pattern.Any + Pattern.Any + toPattern("1"), "111") ::
      (toPattern("1") + Pattern.Any + toPattern("1"), "111") ::
      Nil

    ms.foreach { case (p, s) => assert(matches(p, s), s"matches($p, $s)") }
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
    assert(setOps.subset(toPattern("00") + Pattern.Wild, toPattern("0") + Pattern.Wild))
    assert(setOps.subset(toPattern("00") + Pattern.Any + Pattern.Wild, toPattern("0") + Pattern.Any + Pattern.Wild))
  }

  test("if y - x is empty, (yz - xz) for all strings is empty") {
    def law(x0: Pattern, y0: Pattern, str: String) = {
      val x = Pattern.fromList(x0.toList.take(5))
      val y = Pattern.fromList(y0.toList.take(5))
      if (setOps.difference(y, x) == Nil) {
        val left = y + toPattern(str)
        assert(setOps.difference(left, x + toPattern(str)) == Nil)
      }
    }

    forAll(genPattern, genPattern, genSeq)(law(_, _, _))
  }

  test("if y - x is empty, (zy - zx) for all strings is empty") {
    forAll(genPattern, genPattern, genSeq) { (x0: Pattern, y0: Pattern, str: String) =>
      val x = Pattern.fromList(x0.toList.take(5))
      val y = Pattern.fromList(y0.toList.take(5))
      if (setOps.difference(y, x) == Nil) {
        val left = toPattern(str) + y
        assert(setOps.difference(left, toPattern(str) + x) == Nil)
      }
    }
  }

  test("Named.matches + render agree") {
    def law(n: Named, str: String) = {
      namedMatch(n, str).foreach { m =>
        n.render(m)(_.toString) match {
          case Some(s0) => assert(s0 == str, s"m = $m")
          case None =>
            // this can only happen if we have unnamed Wild/AnyElem
            assert(n.isRenderable == false, s"m = $m")
        }
      }
    }

    forAll(genNamed, genSeq)(law(_, _))

    import NamedSeqPattern.{NCat, Bind, NSeqPart}
    import SeqPart.Wildcard

    val regressions: List[(Named, String)] =
      (NCat(Bind("0",NSeqPart(Wildcard)),Bind("1",NSeqPart(Wildcard))), "") ::
      Nil

    regressions.foreach { case (n, s) => law(n, s) }
  }

  def named(s: String): Named =
    Named.fromPattern(toPattern(s))

  test("Named.matches agrees with Pattern.matches regressions") {

    import Named._
    val regressions: List[(Named, String)] =
      (NCat(NEmpty,NCat(NSeqPart(Lit('1')),NSeqPart(Wildcard))), "1") ::
      (NCat(NSeqPart(Lit('1')),NSeqPart(Wildcard)), "1") ::
      (NSeqPart(Lit('1')), "1") ::
      Nil

    regressions.foreach { case (n, s) => namedMatchesPatternLaw(n, s) }
  }

  test("Test some examples of Named matching") {
    // foo@("bar" baz@(*"baz"))
    val p1 = (named("bar") + (Named.Wild + named("baz")).name("baz")).name("foo")
    assert(namedMatch(p1, "bar and baz") == Some(Map("foo" -> "bar and baz", "baz" -> " and baz")))
  }
}
