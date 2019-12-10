package org.bykn.bosatsu.pattern

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

class SeqPatternTest extends FunSuite {
  import StringSeqPattern._

  import Pattern.patternSetOps._

  // generate a string of 0s and 1s to make matches more likely
  val genBitString: Gen[String] =
    for {
      sz <- Gen.choose(0, 4)
      g = Gen.oneOf(0, 1)
      list <- Gen.listOfN(sz, g)
    } yield list.mkString

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 500)
    //PropertyCheckConfiguration(minSuccessful = 5)

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

  def unlit(p: Pattern): Pattern =
    p match {
      case Empty => Empty
      case Cat(Lit(_) | AnyElem, t) => unlit(t)
      case Cat(h, t) => Cat(h, unlit(t))
    }

  implicit val arbString: Arbitrary[String] = Arbitrary(genBitString)
  implicit val shrinkString: Shrink[String] =
    Shrink { str =>
      for {
        i <- (0 until str.length).toStream
        j <- (i until str.length).toStream
      } yield str.substring(i, j)
    }

  test("some matching examples") {
    val matches: List[(Pattern, String)] =
      (Pattern.Wild + Pattern.Any + Pattern.Any + Pattern("1"), "111") ::
      (Pattern("1") + Pattern.Any + Pattern("1"), "111") ::
      Nil

    matches.foreach { case (p, s) => assert(p.matches(s), s"$p.matches($s)") }
  }

  test("reverse is idempotent") {
    forAll { p: Pattern =>
      assert(p.reverse.reverse == p)
    }
  }

  test("reverse matches the reverse string") {
    forAll { (p: Pattern, str: String) =>
      assert(p.matches(str) == p.reverse.matches(str.reverse), s"p.reverse = ${p.reverse}")
    }
  }

  test("unlit patterns match everything") {
    forAll { (p0: Pattern, str: String) =>
      val p = unlit(p0)
      assert(matches(p, str) || (p == Empty))
    }
  }

  test("wildcard on either side is the same as contains") {
    forAll { (ps: String, s: String) =>
      assert(matches(Pattern.Wild + Pattern(ps) + Pattern.Wild, s) == s.contains(ps))
    }
  }
  test("wildcard on front side is the same as endsWith") {
    forAll { (ps: String, s: String) =>
      assert(matches(Pattern.Wild + Pattern(ps), s) == s.endsWith(ps))
    }
  }
  test("wildcard on back side is the same as startsWith") {
    forAll { (ps: String, s: String) =>
      assert(matches(Pattern(ps) + Pattern.Wild, s) == s.startsWith(ps))
    }
  }
  test("normalized patterns have the same matches as unnormalized ones") {
    forAll { (p0: Pattern, s: String) =>
      val pnorm = p0.normalize
      assert(matches(pnorm, s) == matches(p0, s))
    }
  }

  test("normalized patterns don't have adjacent Wildcard") {
    forAll { p0: Pattern =>
      val list = p0.normalize.toList
      list.sliding(2).foreach {
        case bad@Seq(Wildcard, Wildcard) =>
          fail(s"saw adjacent: $bad in ${p0.normalize}")
        case _ => ()
      }
    }
  }

  test("intersection(p1, p2).matches(x) == p1.matches(x) && p2.matches(x)") {
    def law(p10: Pattern, p20: Pattern, x: String) = {
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

    forAll(law(_, _, _))
    val regressions: List[(Pattern, Pattern, String)] =
      (Pattern("0") + Pattern.Any + Pattern.Wild, Pattern.Any + Pattern("01") + Pattern.Any, "001") ::
      (Pattern.Wild + Pattern.Any + Pattern.Any + Pattern("1"), Pattern("1") + Pattern.Any + Pattern("1"), "111") ::
      Nil

    regressions.foreach { case (p1, p2, s) => law(p1, p2, s) }
  }

  test("intersection is commutative") {
    def law(p1: Pattern, p2: Pattern, x: String) =
      assert(p1.intersection(p2).exists(_.matches(x)) ==
        p2.intersection(p1).exists(_.matches(x)))

    forAll(law(_: Pattern, _: Pattern, _: String))
  }

  test("intersection is associative") {
    def law(p1u: Pattern, p2u: Pattern, p3u: Pattern, x: String) = {
      // this can get really huge and take forever
      val max = 3
      val p1 = Pattern.fromList(p1u.normalize.toList.take(max))
      val p2 = Pattern.fromList(p2u.normalize.toList.take(max))
      val p3 = Pattern.fromList(p3u.normalize.toList.take(max))

      val leftI = for {
        i1 <- p1.intersection(p2).iterator
        i2 <- i1.intersection(p3).iterator
      } yield i2

      val left = leftI.exists(_.matches(x))

      val rightI = for {
        i1 <- p2.intersection(p3).iterator
        i2 <- p1.intersection(i1).iterator
      } yield i2

      val right = rightI.exists(_.matches(x))

      //assert(left == right, s"\n\n$leftI\n\n$rightI")
      assert(left == right)
    }

    forAll(law(_, _, _, _))

    val regressions: List[(Pattern, Pattern, Pattern, String)] =
      Nil

    regressions.foreach { case (p1, p2, p3, s) => law(p1, p2, p3, s) }
  }

  test("matches any does match any") {
    forAll { (p: Pattern, s: String) =>
      if (p.matchesAny) assert(p.matches(s))
    }
  }

  test("matchesEmpty is right") {
    forAll { p: Pattern =>
      if (p.matchesEmpty) assert(p.matches(""))
    }
  }

  test("toList/fromList match the same") {
    forAll { (p: Pattern, s: String) =>
      val p1 = Pattern.fromList(p.toList)
      assert(p1.matches(s) == p.matches(s), s"p1 = $p1")
    }
  }


  test("isEmpty only matches empty") {
    forAll { (p: Pattern, s: String) =>
      if (p.isEmpty) {
        assert(p.matches(""))
        if (s != "") {
          assert(!p.matches(s))
        }
      }
    }
  }

  test("unify union preserves matching") {
    forAll { (ps: List[Pattern], s: String) =>
      val u1 = ps.exists(_.matches(s))
      val unified = unifyUnion(ps)
      val u2 = unified.exists(_.matches(s))

      assert(u2 == u1, s"unified = $unified")
    }
  }

  test("unify union makes size <= input") {
    forAll { (ps: List[Pattern], s: String) =>
      val unified = unifyUnion(ps)

      assert(ps.size >= unified.size, s"unified = $unified")
    }
  }

  test("difference is an upper bound") {
    forAll { (p10: Pattern, p20: Pattern, s: String) =>
      // intersections get really slow if they get too big
      val p1 = Pattern.fromList(p10.toList.take(5))
      val p2 = Pattern.fromList(p20.toList.take(5))
      // true difference is <= diff
      val diff = difference(p1, p2)
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

  test("difference is idempotent: (a - b) = c, c - b == c") {
    forAll { (a0: Pattern, b0: Pattern) =>
      val a = Pattern.fromList(a0.toList.take(4))
      val b = Pattern.fromList(b0.toList.take(4))
      val c = unifyUnion(a.difference(b))
      val c1 = unifyUnion(c.flatMap(_.difference(b)))
      assert(c.map(_.show) == c1.map(_.show))
    }
  }

  test("subset is consistent with match") {
    forAll { (a: Pattern, b: Pattern, s: String) =>
      if (a.matches(s)) {
        if (subset(a, b)) {
          assert(b.matches(s))
        }
      }
    }

    assert(subset(Pattern("00") + Pattern.Wild, Pattern("0") + Pattern.Wild))
    assert(subset(Pattern("00") + Pattern.Any + Pattern.Wild, Pattern("0") + Pattern.Any + Pattern.Wild))
  }

  test("a n a == a") {
    forAll { (a: Pattern) =>
      if (a.normalize == a) {
        // normal patterns matche themselves exactly
        assert(a.intersection(a) == List(a))
      }
      else {
        assert(a.intersection(a).map(_.normalize).distinct == List(a.normalize))
      }
    }
  }

  test("a - a == 0") {
    def law(p1: Pattern, p2: Pattern) = {
      if (p1.normalize == p2.normalize) {
        assert(p1.difference(p2) == Nil)
      }
      assert(p1.difference(p1) == Nil)
      assert(p2.difference(p2) == Nil)
    }
    forAll(law(_, _))

    val regressions: List[(Pattern, Pattern)] =
      Nil

    regressions.foreach { case (p1, p2) => law(p1, p2) }


  }

  test("if y - x is empty, (yz - xz) for all strings is empty") {
    def law(x0: Pattern, y0: Pattern, str: String) = {
      val x = Pattern.fromList(x0.toList.take(5))
      val y = Pattern.fromList(y0.toList.take(5))
      if (y.difference(x) == Nil) {
        assert(y.append(str).difference(x.append(str)) == Nil)
      }
    }

    forAll(law(_, _, _))
  }

  test("if y - x is empty, (zy - zx) for all strings is empty") {
    forAll { (x0: Pattern, y0: Pattern, str: String) =>
      val x = Pattern.fromList(x0.toList.take(5))
      val y = Pattern.fromList(y0.toList.take(5))
      if (y.difference(x) == Nil) {
        assert((Pattern(str) + y).difference(Pattern(str) + x) == Nil)
      }
    }
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
  test("p + q match (s + t) if p.matches(s) && q.matches(t)") {
    forAll { (p: Pattern, q: Pattern, s: String, t: String) =>
      if (p.matches(s) && q.matches(t)) {
        assert((p + q).matches(s + t))
      }
    }
  }

  test("x - wild = 0") {
    assert(Pattern.Wild.matchesAny)

    forAll { (x: Pattern, y: Pattern) =>
      if (y.matchesAny) assert(x.difference(y).isEmpty)

      assert(x.difference(Pattern.Wild) == Nil)
      assert(y.difference(Pattern.Wild) == Nil)
    }
  }

  test("if a n b = 0 then a - b = a") {
    def law(p1: Pattern, p2: Pattern) = {
      val inter = p1.intersection(p2)
      val diff = p1.difference(p2)

      if (inter.isEmpty) {
        assert(diff.map(_.normalize) == p1.normalize :: Nil)
      }

      // difference is an upper bound, so this is not true
      // although we wish it were
      /*
      if (diff.map(_.normalize).distinct == p1.normalize :: Nil) {
        // intersection is 0
        assert(inter == Nil)
      }
      */
    }

    forAll(law(_, _))

    val regressions: List[(Pattern, Pattern)] =
      Nil

    regressions.foreach { case (p1, p2) => law(p1, p2) }
  }

  test("x - y = z, then x - y - z = 0") {
    val max = 3
    forAll { (x0: Pattern, y0: Pattern) =>
      val x = Pattern.fromList(x0.toList.take(max))
      val y = Pattern.fromList(y0.toList.take(max))
      val z = x.difference(y)
      val z1 = unifyUnion(differenceAll(z, z))

      assert(z1.map(_.show) == Nil, s"z = ${z.map(_.show)}")
    }
  }

  // test("subset consistency: a n b == a <=> a - b = 0") {
  //   def isSubsetIntr(a: Pattern, b: Pattern) =
  //     intersection(a.normalize, b.normalize).map(_.normalize).distinct == List(a.normalize)

  //   def isSubsetDiff(a: Pattern, b: Pattern) =
  //     difference(a, b).isEmpty

  //   def law(a: Pattern, b: Pattern) = {
  //     assert(isSubsetIntr(a, b) == isSubsetDiff(a, b))
  //   }

  //   forAll(law(_, _))

  //   val regressions: List[(Pattern, Pattern)] = Nil

  //   regressions.foreach { case (a, b) => law(a, b) }
  // }

  // test("no missing/unused paradox") {
  //   /*
  //    * We don't want to produce a list of missing branches, but then add them
  //    * and find we have unused branches
  //    */
  //   val smallList: Gen[List[Pattern]] =
  //     for {
  //       cnt <- Gen.choose(1, 2)
  //       list <- Gen.listOfN(cnt, genPat)
  //     } yield list

  //   def diff(as: List[Pattern], bs: List[Pattern]): List[Pattern] =
  //     for {
  //       a <- as
  //       b <- bs
  //       c <- difference(a, b)
  //     } yield c

  //   def intr(as: List[Pattern], bs: List[Pattern]): List[Pattern] =
  //     for {
  //       a <- as
  //       b <- bs
  //       c <- intersection(a, b)
  //     } yield c

  //   forAll(genPat, smallList) { (h, t) =>
  //     val pats = h :: t
  //     val missing = diff(List(Pattern.Wild), pats)
  //     if (missing.nonEmpty) {
  //       // this cannot be a subset of pats
  //       val isSubSet = diff(missing, pats)
  //       assert(isSubSet != Nil)
  //     }
  //   }
  // }

  test("Named.matches agrees with Pattern.matches") {
    def law(n: Named, str: String) = {
      val p = n.unname

      assert(n.matches(str).isDefined == p.matches(str), s"${p.show}")
    }

    forAll(law(_, _))

    import Named._
    val regressions: List[(Named, String)] =
      (NCat(NEmpty,NCat(NPart(Lit('1')),NPart(Wildcard))), "1") ::
      (NCat(NPart(Lit('1')),NPart(Wildcard)), "1") ::
      (NPart(Lit('1')), "1") ::
      Nil

    regressions.foreach { case (n, s) => law(n, s) }
  }

  test("Named.matches + render agree") {
    forAll { (n: Named, str: String) =>
      n.matches(str).foreach { m =>
        n.render(m) match {
          case Some(s0) => assert(s0 == str)
          case None =>
            // this can only happen if we have unnamed Wild/AnyElem
            assert(n.isRenderable == false)
        }
      }
    }
  }

  test("Test some examples of Named matching") {
    // foo@("bar" baz@(*"baz"))
    val p1 = (Named("bar") + (Named.Wild + Named("baz")).name("baz")).name("foo")
    assert(p1.matches("bar and baz") == Some(Map("foo" -> "bar and baz", "baz" -> " and baz")))
  }
}
