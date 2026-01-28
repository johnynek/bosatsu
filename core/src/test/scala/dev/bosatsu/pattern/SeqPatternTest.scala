package dev.bosatsu.pattern

import dev.bosatsu.set.{Rel, SetOps}
import dev.bosatsu.StringUtil
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Prop.forAll

import SeqPattern.{Cat, Empty}
import SeqPart.{Wildcard, AnyElem, Lit}

import cats.implicits._

object StringSeqPatternGen {

  type Named = NamedSeqPattern[Int]
  val Named = NamedSeqPattern

  def lit(c: Char): SeqPart[Int] = Lit(c.toInt)

  // generate a string of 0s and 1s to make matches more likely
  val genBitString: Gen[String] =
    for {
      sz <- Gen.geometric(4.0)
      g = Gen.oneOf(0, 1)
      list <- Gen.listOfN(sz, g)
    } yield list.mkString

  val genPart: Gen[SeqPart[Int]] = {
    import SeqPart._

    Gen.frequency(
      (15, Gen.oneOf(lit('0'), lit('1'))),
      (2, Gen.const(AnyElem)),
      (1, Gen.const(Wildcard))
    )
  }

  val genPat: Gen[SeqPattern[Int]] = {

    val cat = for {
      h <- genPart
      t <- genPat
    } yield Cat(h, t)

    Gen.frequency((1, Gen.const(Empty)), (5, cat))
  }

  implicit val arbPattern: Arbitrary[SeqPattern[Int]] = Arbitrary(genPat)

  implicit lazy val shrinkPat: Shrink[SeqPattern[Int]] =
    Shrink.withLazyList {
      case Empty            => Empty #:: LazyList.empty
      case Cat(Wildcard, t) =>
        (Cat(AnyElem, t) #:: t #:: LazyList.empty).flatMap(shrinkPat.shrink)
      case Cat(_, t) =>
        t #:: LazyList.from(shrinkPat.shrink(t))
    }

  def genNamedFn[A](
      gp: Gen[SeqPart[A]],
      nextId: Int
  ): Gen[(Int, NamedSeqPattern[A])] = {
    lazy val recur = Gen.lzy(res)

    lazy val genNm: Gen[(Int, Named.Bind[A])] =
      recur.map { case (i, n) => (i + 1, Named.Bind(i.toString, n)) }

    // expected length = L = p_c * 2 * L + pn + L + pe * 0 + pp
    //
    // L = pp/(1 - (2*pc + pn))
    // so we need 2*pc + pn < 1
    // e.g. pc = 1/3, pn = 1/9 would work
    // L = (4/9) / (1 - (2/3 + 1/9)) = 4 / (9 - 5) = 1
    lazy val res: Gen[(Int, NamedSeqPattern[A])] =
      Gen.frequency(
        (
          3,
          for {
            (i0, n0) <- recur
            (i1, n1) <- genNamedFn(gp, i0)
          } yield (i1, NamedSeqPattern.NCat(n0, n1))
        ),
        (1, genNm),
        (1, Gen.const((nextId, NamedSeqPattern.NEmpty))),
        (4, gp.map(p => (nextId, NamedSeqPattern.NSeqPart(p))))
      )

    res
  }

  val genNamed: Gen[NamedSeqPattern[Int]] =
    genNamedFn(genPart, 0).map(_._2)

  implicit val arbNamed: Arbitrary[NamedSeqPattern[Int]] = Arbitrary(genNamed)

  def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    if (s1.isEmpty) s2
    else if (s2.isEmpty) s1
    else {
      s1.head #:: interleave(s2, s1.tail)
    }

  implicit val shrinkNamedSeqPattern: Shrink[NamedSeqPattern[Int]] =
    Shrink.withLazyList {
      case NamedSeqPattern.NEmpty     => LazyList.empty
      case NamedSeqPattern.Bind(n, p) =>
        val sp = LazyList.from(shrinkNamedSeqPattern.shrink(p))
        val binded = sp.map(NamedSeqPattern.Bind(n, _))
        interleave(sp, binded)
      case NamedSeqPattern.NSeqPart(p) =>
        def tail: LazyList[SeqPart[Int]] =
          lit('0') #:: lit('1') #:: LazyList.empty

        val sp = p match {
          case Wildcard => AnyElem #:: tail
          case AnyElem  => tail
          case Lit(_)   => LazyList.empty
        }
        sp.map(NamedSeqPattern.NSeqPart(_))
      case NamedSeqPattern.NCat(fst, snd) =>
        val s1 = LazyList.from(shrinkNamedSeqPattern.shrink(fst))
        val s2 = LazyList.from(shrinkNamedSeqPattern.shrink(snd))
        interleave(s1, s2).iterator
          .sliding(2)
          .map {
            case Seq(a, b) => NamedSeqPattern.NCat(a, b)
            case _         => NamedSeqPattern.NEmpty
          }
          .to(LazyList)
    }

  def unany[A](p: SeqPattern[A]): SeqPattern[A] =
    p match {
      case Empty           => Empty
      case Cat(AnyElem, t) => unany(t)
      case Cat(h, t)       => Cat(h, unany(t))
    }

  def unwild[A](p: SeqPattern[A]): SeqPattern[A] =
    p match {
      case Empty            => Empty
      case Cat(Wildcard, t) => unwild(t)
      case Cat(h, t)        => Cat(h, unwild(t))
    }

  implicit val arbString: Arbitrary[String] = Arbitrary(genBitString)
  implicit val shrinkString: Shrink[String] =
    Shrink.withLazyList { str =>
      for {
        i <- (0 until str.length).to(LazyList)
        j <- (i until str.length).to(LazyList)
      } yield str.substring(i, j)
    }

}

abstract class SeqPatternLaws[E, I, S, R] extends munit.ScalaCheckSuite {

  type Pattern = SeqPattern[E]
  val Pattern = SeqPattern
  type Named = NamedSeqPattern[E]
  val Named = NamedSeqPattern

  protected def minSuccessfulTests: Int =
    // PropertyCheckConfiguration(minSuccessful = 50000)
    5000
  // PropertyCheckConfiguration(minSuccessful = 50)

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(minSuccessfulTests)

  def genPattern: Gen[Pattern]
  def genNamed: Gen[Named]
  def genSeq: Gen[S]
  def splitter: Splitter[E, I, S, R]
  def setOps: SetOps[Pattern]

  def matches(p: Pattern, s: S): Boolean
  def namedMatches(p: Named, s: S): Boolean

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

    assertEquals(together, sep, s"n1: $n1, n2: $n2, intersection: $intersect")
    // if (together != sep) sys.error(s"n1: $n1, n2: $n2, intersection: $intersect")
    // else succeed
  }

  def namedMatchesPatternLaw(n: Named, str: S) = {
    val p = n.unname

    assertEquals(namedMatches(n, str), matches(p, str), s"${p.show}")
  }

  def differenceUBLaw(p10: Pattern, p20: Pattern, s: S) = {
    // intersections get really slow if they get too big
    val max = 8
    val p1 = Pattern.fromList(p10.toList.take(max))
    val p2 = Pattern.fromList(p20.toList.take(max))
    // true difference is <= diff
    val diff = setOps.difference(p1, p2)
    val diffmatch = diff.exists(matches(_, s))

    if (diffmatch) assert(matches(p1, s), s"diff = $diff")

    if (matches(p1, s) && !matches(p2, s)) {
      assert(diffmatch, s"diff = $diff")
    }

    // diff.isEmpty implies p1 <= p2,
    // so everything matched by p1 must be matched by p2
    if (diff.isEmpty) {
      if (matches(p1, s)) assert(matches(p2, s))
    }

    if (p2.matchesAny) assertEquals(diff, Nil)

    // the law we wish we had:
    // if (p2.matches(s) && p1.matches(s)) assert(!diffmatch)
  }

  test("reverse is idempotent") {
    forAll(genPattern) { (p: Pattern) =>
      assertEquals(p.reverse.reverse, p)
    }
  }

  test("cat.reverse == cat.reverseCat") {
    forAll(genPattern) { p =>
      p match {
        case Empty         => assertEquals(p.reverse, Empty)
        case c @ Cat(_, _) => assertEquals(c.reverseCat, c.reverse)
      }
    }
  }

  test("reverse matches the reverse string") {
    forAll(genPattern, genSeq) { (p: Pattern, str: S) =>
      val rstr = splitter.fromList(splitter.toList(str).reverse)
      assertEquals(matches(p, str), matches(p.reverse, rstr), s"p.reverse = ${p.reverse}")
    }
  }

  test("unlit patterns match everything") {
    def unlit(p: Pattern): Pattern =
      p match {
        case Empty                    => Empty
        case Cat(Lit(_) | AnyElem, t) => unlit(t)
        case Cat(h, t)                => Cat(h, unlit(t))
      }

    forAll(genPattern, genSeq) { (p0, str) =>
      val p = unlit(p0)
      assert(matches(p, str) || (p == Empty))
    }
  }

  test("normalized patterns have the same matches as unnormalized ones") {
    forAll(genPattern, genSeq) { (p0, s) =>
      val pnorm = p0.normalize
      assertEquals(matches(pnorm, s), matches(p0, s))
    }
  }

  test("normalized patterns don't have adjacent Wildcard") {
    forAll(genPattern) { p0 =>
      val list = p0.normalize.toList
      list.sliding(2).foreach {
        case bad @ Seq(Wildcard, Wildcard) =>
          fail(s"saw adjacent: $bad in ${p0.normalize}")
        case _ => ()
      }
    }
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
    forAll(genPattern) { (p: Pattern) =>
      if (p.matchesEmpty) assert(matches(p, splitter.emptySeq))
    }
  }

  test("toList/fromList match the same") {
    forAll(genPattern, genSeq) { (p, s) =>
      val p1 = Pattern.fromList(p.toList)
      assertEquals(matches(p1, s), matches(p, s), s"p1 = $p1")
    }
  }

  test("isEmpty only matches empty") {
    forAll(genPattern, genSeq) { (p: Pattern, s: S) =>
      if (p.isEmpty) {
        assert(matches(p, splitter.emptySeq))
        val seqEq = cats.Eq.fromUniversalEquals[S]
        if (!seqEq.eqv(s, splitter.emptySeq)) {
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

      assertEquals(u2, u1, s"unified = $unified")
    }
  }

  def subsetConsistentWithMatchLaw(a: Pattern, b: Pattern, s: S) =
    if (matches(a, s)) {
      if (setOps.subset(a, b)) {
        assert(matches(b, s))
      }
    }

  test("if subset(a, b) then matching a implies matching b") {
    forAll(genPattern, genPattern, genSeq)(
      subsetConsistentWithMatchLaw(_, _, _)
    )
  }

  def diffUBRegressions: List[(Pattern, Pattern, S)] = Nil

  test("difference is an upper bound") {
    val prop = forAll(genPattern, genPattern, genSeq) { case (p1, p2, s) =>
      differenceUBLaw(p1, p2, s)
    }

    diffUBRegressions.foreach { case (p1, p2, s) =>
      differenceUBLaw(p1, p2, s)
    }
    prop
  }

  test("p + q match (s + t) if p.matches(s) && q.matches(t)") {
    forAll(genPattern, genPattern, genSeq, genSeq) {
      (p: Pattern, q: Pattern, s: S, t: S) =>
        if (matches(p, s) && matches(q, t)) {
          assert(matches(p + q, splitter.catSeqs(s :: t :: Nil)))
        }
    }
  }

  test("Named.matches agrees with Pattern.matches") {
    forAll(genNamed, genSeq)(namedMatchesPatternLaw(_, _))
  }

  test("* - [] - [_, *] == empty") {
    val diff1 = setOps.difference(Cat(Wildcard, Empty), Empty)
    assertEquals(diff1.flatMap(
        setOps.difference(_, Cat(AnyElem, Cat(Wildcard, Empty)))
      ), Nil)
    assertEquals(diff1.flatMap(
        setOps.difference(_, Cat(Wildcard, Cat(AnyElem, Empty)))
      ), Nil)
  }

  test("[*, _] n [*, _, *] commutes and is [_, *]") {
    val p1 = Cat(Wildcard, Cat(AnyElem, Empty))
    val p2 = Cat(Wildcard, Cat(AnyElem, Cat(Wildcard, Empty)))
    val int1 = setOps.intersection(p1, p2)
    val int2 = setOps.intersection(p2, p1)
    assertEquals(int1, int2)
    assertEquals(int1, List(Cat(AnyElem, Cat(Wildcard, Empty))))
  }

  test("[*] n [*, *] commutes") {
    val p1 = Cat(Wildcard, Empty)
    val p2 = Cat(Wildcard, p1)
    val int1 = setOps.intersection(p1, p2)
    val int2 = setOps.intersection(p2, p1)
    assertEquals(int1, int2)
  }

  test("x n y == y n x") {
    forAll(genPattern, genPattern) { (x: Pattern, y: Pattern) =>
      val i1 = setOps.intersection(x, y)
      val i2 = setOps.intersection(y, x)
      assertEquals(i1, i2, s"${i1.map(_.show)} != ${i2.map(_.show)}")
    }
  }

  test("if x - y is empty, (x + z) - (y + z) is empty") {
    forAll(genPattern, genPattern, genPattern) {
      (x0: Pattern, y0: Pattern, z0: Pattern) =>
        val x = Pattern.fromList(x0.toList.take(3))
        val y = Pattern.fromList(y0.toList.take(3))
        val z = Pattern.fromList(z0.toList.take(3))
        if (setOps.difference(x, y).isEmpty) {
          assertEquals(setOps.differenceAll(x :: z :: Nil, y :: z :: Nil), Nil)
        }
    }
  }
}

class BoolSeqPatternTest
    extends SeqPatternLaws[Set[Boolean], Boolean, List[Boolean], Unit] {

  override protected def minSuccessfulTests: Int =
    // these tests wind up running very long sometimes
    100

  implicit lazy val shrinkPat: Shrink[SeqPattern[Set[Boolean]]] =
    Shrink.withLazyList {
      case Empty            => Empty #:: LazyList.empty
      case Cat(Wildcard, t) =>
        (Cat(AnyElem, t) #:: t #:: LazyList.empty).flatMap(shrinkPat.shrink)
      case Cat(AnyElem, t) =>
        (Cat(Lit(Set(false)), t) #:: Cat(
          Lit(Set(true)),
          t
        ) #:: t #:: LazyList.empty).flatMap(shrinkPat.shrink)
      case Cat(_, t) =>
        t #:: LazyList.from(shrinkPat.shrink(t))
    }

  val genBool: Gen[Boolean] = Gen.oneOf(true, false)

  val genSetBool: Gen[Set[Boolean]] =
    // we don't generate Set.empty, which is a bottom type,
    // which this code does not yet support
    Gen.oneOf(List(Set(true), Set(false), Set(true, false)))

  def genPart: Gen[SeqPart[Set[Boolean]]] = {
    import SeqPart._

    Gen.frequency(
      (15, genSetBool.map(Lit(_))),
      (2, Gen.const(AnyElem)),
      (1, Gen.const(Wildcard))
    )
  }

  val genNamed: Gen[NamedSeqPattern[Set[Boolean]]] =
    StringSeqPatternGen.genNamedFn(genPart, 0).map(_._2)

  lazy val genPattern: Gen[SeqPattern[Set[Boolean]]] = {
    val sp = Gen.frequency(
      (1, SeqPart.Wildcard),
      (2, SeqPart.AnyElem),
      (8, genSetBool.map(SeqPart.Lit(_)))
    )

    Gen.frequency(
      (1, Gen.const(SeqPattern.Empty)),
      (
        4,
        Gen.zip(sp, Gen.lzy(genPattern)).map { case (h, t) =>
          SeqPattern.Cat(h, t)
        }
      )
    )
  }
  def genSeq: Gen[List[Boolean]] =
    Gen.choose(0, 20).flatMap(Gen.listOfN(_, genBool))

  val splitter = Splitter.listSplitter(
    Matcher.fnMatch[Boolean]: Matcher[Set[Boolean], Boolean, Unit]
  )
  val pmatcher = SeqPattern.matcher(splitter)

  def matches(p: SeqPattern[Set[Boolean]], s: List[Boolean]): Boolean =
    pmatcher(p)(s).isDefined
  def namedMatches(
      p: NamedSeqPattern[Set[Boolean]],
      s: List[Boolean]
  ): Boolean =
    NamedSeqPattern.matcher(splitter)(p)(s).isDefined

  implicit val setOpsBool: SetOps[Set[Boolean]] =
    SetOps.fromFinite(List(true, false))
  implicit val ordSet: Ordering[Set[Boolean]] =
    Ordering[List[Boolean]].on((s: Set[Boolean]) => s.toList.sorted)

  val setOps: SetOps[SeqPattern[Set[Boolean]]] =
    SeqPattern.seqPatternSetOps[Set[Boolean]]

  // we can sometimes enumerate a finite LazyList of matches
  def enumerate(sp: SeqPattern[Set[Boolean]]): Option[LazyList[List[Boolean]]] =
    if (sp.toList.exists(_.isWild)) None
    else {
      def loop(sp: SeqPattern[Set[Boolean]]): LazyList[List[Boolean]] =
        sp match {
          case Cat(hsp, rest) =>
            val rests = loop(rest)
            val heads = hsp match {
              case Lit(s) if s.size == 1 => s.head :: Nil
              case _                     =>
                // we assume any because there
                // are no wilds
                List(false, true)
            }
            for {
              tail <- rests
              h <- heads
            } yield h :: tail
          case Empty => LazyList(Nil)
        }

      Some(loop(sp))
    }

  override def diffUBRegressions: List[(Pattern, Pattern, List[Boolean])] =
    List {
      val p1 = Cat(Wildcard, Empty)
      val p2 = Cat(
        Lit(Set(true)),
        Cat(
          Wildcard,
          Cat(Lit(Set(true, false)), Cat(Lit(Set(true)), Cat(Wildcard, Empty)))
        )
      )
      val s = List(true, false, false)

      (p1, p2, s)
    }

  test("check an example [*_, True, *_], [], [False, *_]") {
    val missing = setOps.missingBranches(
      List(SeqPattern.fromList(Wildcard :: Nil)),
      List(
        SeqPattern.fromList(Wildcard :: Lit(Set(true)) :: Wildcard :: Nil),
        SeqPattern.fromList(Lit(Set(false)) :: Wildcard :: Nil),
        SeqPattern.fromList(Nil)
      )
    )

    assertEquals(missing, Nil)
  }

  test("a n b == b n a regressions (bool)") {
    // val a = Cat(AnyElem, Cat(Wildcard, Cat(Lit(Set(true, false)), Empty)))
    // val b = Cat(AnyElem, Cat(Wildcard, Cat(AnyElem, Empty)))
    val a = Cat(AnyElem, Cat(Wildcard, Cat(Lit(Set(true, false)), Empty)))
    val b = Cat(AnyElem, Cat(Wildcard, Cat(AnyElem, Empty)))

    val ab = setOps.intersection(a, b)
    val ba = setOps.intersection(b, a)
    assertEquals(ab, ba)
  }

  test("if we can enumerate p1 and p1 - p2 == 0, then all match p2") {
    def law(p10: Pattern, p20: Pattern) = {
      val max = 7
      val p1 = Pattern.fromList(p10.toList.take(max))
      enumerate(p1) match {
        case None     => ()
        case Some(ms) =>
          val p2 = Pattern.fromList(p20.toList.take(max))
          val diff = setOps.difference(p1, p2)
          if (diff.isEmpty || setOps.subset(p1, p2)) {
            ms.foreach { s =>
              assert(matches(p1, s), s"p1: $s")
              assert(matches(p2, s), s"p2: $s")
            }
          } else {
            // difference is an upper-bound
            // so truediff <= diff
            // if ms does not match diff, then it must
            // match p2
            val diffMisses = ms.filter { s =>
              !diff.exists(matches(_, s))
            }
            diffMisses.foreach { d =>
              assert(matches(p2, d), d)
            }
          }
      }
    }

    forAll(genPattern, genPattern)(law(_, _))
  }

  test("enumerate is correct") {
    forAll(genPattern, genSeq) { (p0, s) =>
      val max = 6
      val p = Pattern.fromList(p0.toList.take(max))
      enumerate(p) match {
        case None     => ()
        case Some(ms) =>
          assert(ms.nonEmpty)
          val mslist = ms.toList
          assertEquals(matches(p, s), ms.exists(_ == s), mslist)
      }
    }
  }

  test("subset == true can be trusted. checked with enumerations") {
    def law(p10: Pattern, p20: Pattern, blist: List[List[Boolean]]) = {
      val max = 10
      val p1 = Pattern.fromList(p10.toList.take(max))
      val p2 = Pattern.fromList(p20.toList.take(max))
      val mOpt = enumerate(p1)
      if (setOps.subset(p1, p2)) {
        // if p1 <= p2, then matching p1 implies matching p2
        blist.filter(matches(p1, _)).foreach { bs =>
          assert(matches(p2, bs))
        }

        mOpt match {
          case Some(ms) =>
            // if p1 is a subset all these match p2
            ms.foreach { s =>
              assert(matches(p1, s), s)
              assert(matches(p2, s), s)
            }
          case None => ()
        }
      }
    }

    val prop = forAll(genPattern, genPattern, Gen.listOf(genSeq))(law(_, _, _))

    val subsets: List[(Pattern, Pattern, Boolean)] =
      List {
        val p0 = Cat(
          Wildcard,
          Cat(
            Lit(Set(false)),
            Cat(Lit(Set(true, false)), Cat(Lit(Set(true, false)), Empty))
          )
        )
        val p1 = Cat(Wildcard, Cat(Lit(Set(true, false)), Empty))
        (p0, p1, true)
      }

    subsets.foreach { case (p1, p2, res) =>
      assertEquals(setOps.subset(p1, p2), res)
    }

    val regressions: List[(Pattern, Pattern, List[List[Boolean]])] =
      List(
        {
          val p0 = Cat(
            Lit(Set(false)),
            Cat(
              Lit(Set(false)),
              Cat(
                Lit(Set(false)),
                Cat(
                  Lit(Set(true)),
                  Cat(
                    AnyElem,
                    Cat(
                      Lit(Set(false)),
                      Cat(
                        Lit(Set(false)),
                        Cat(
                          Lit(Set(false)),
                          Cat(Lit(Set(true, false)), Cat(Lit(Set(true)), Empty))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          val p1 = Cat(
            Wildcard,
            Cat(
              Lit(Set(true, false)),
              Cat(
                Lit(Set(true)),
                Cat(
                  Lit(Set(false)),
                  Cat(
                    Lit(Set(true, false)),
                    Cat(
                      Wildcard,
                      Cat(
                        Lit(Set(true, false)),
                        Cat(Lit(Set(true, false)), Empty)
                      )
                    )
                  )
                )
              )
            )
          )

          val matchp0 = Nil
          (p0, p1, matchp0)
        }, {
          val p0 =
            Cat(Lit(Set(true)), Cat(AnyElem, Cat(Lit(Set(false)), Empty)))
          val p1 = Cat(
            Wildcard,
            Cat(Lit(Set(true)), Cat(Lit(Set(false)), Cat(Wildcard, Empty)))
          )

          val matchp0 = Nil
          (p0, p1, matchp0)
        }
      )
    regressions.foreach { case (p1, p2, bs) =>
      law(p1, p2, bs)
    }
    prop
  }

  test("test some missing branches") {
    assertEquals(setOps.missingBranches(
        Cat(Wildcard, Empty) :: Nil,
        Pattern.fromList(List(Wildcard, AnyElem, Wildcard)) :: Nil
      ), Pattern.fromList(Nil) :: Nil)

    assertEquals(setOps.missingBranches(
        Cat(Wildcard, Empty) :: Nil,
        Pattern.fromList(List(Wildcard, Lit(Set(true)), Wildcard)) :: Nil
      ), Pattern.fromList(Nil) :: Pattern.fromList(
          Lit(Set(false)) :: Wildcard :: Nil
        ) :: Nil)
  }
}

class SeqPatternTest extends SeqPatternLaws[Int, Int, String, Unit] {
  import SeqPart._

  def genPattern = StringSeqPatternGen.genPat
  def genNamed = StringSeqPatternGen.genNamed
  def genSeq = StringSeqPatternGen.genBitString

  def splitter: Splitter[Int, Int, String, Unit] =
    Splitter.stringSplitter(_ => ())

  val pmatcher = Pattern.matcher(splitter)

  def matches(p: Pattern, s: String): Boolean = pmatcher(p)(s).isDefined
  def namedMatches(p: Named, s: String): Boolean =
    NamedSeqPattern.matcher(splitter)(p)(s).isDefined

  def namedMatch(p: Named, s: String): Option[Map[String, String]] =
    NamedSeqPattern.matcher(Splitter.stringSplitter(_.toString))(p)(s).map(_._2)

  implicit val setOpsChar: SetOps[Int] = SetOps.distinct[Int]
  def setOps: SetOps[Pattern] = Pattern.seqPatternSetOps[Int]

  import StringSeqPatternGen._

  def toPattern(s: String): Pattern =
    StringUtil.codePoints(s).foldRight(Pattern.Empty: Pattern) { (c, r) =>
      Pattern.Cat(Lit(c), r)
    }

  override def diffUBRegressions
      : List[(SeqPattern[Int], SeqPattern[Int], String)] =
    List {
      val p1 = Cat(AnyElem, Cat(Wildcard, Empty))
      val p2 = Cat(
        Wildcard,
        Cat(AnyElem, Cat(lit('0'), Cat(lit('1'), Cat(Wildcard, Empty))))
      )
      (p1, p2, "11")
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
      assertEquals(matches(Pattern.Wild + toPattern(ps) + Pattern.Wild, s), s.contains(
          ps
        ))
    }
  }
  test("wildcard on front side is the same as endsWith") {
    forAll { (ps: String, s: String) =>
      assertEquals(matches(Pattern.Wild + toPattern(ps), s), s.endsWith(ps))
    }
  }
  test("wildcard on back side is the same as startsWith") {
    forAll { (ps: String, s: String) =>
      assertEquals(matches(toPattern(ps) + Pattern.Wild, s), s.startsWith(ps))
    }
  }

  test(
    "intersection(p1, p2).matches(x) == p1.matches(x) && p2.matches(x) regressions"
  ) {
    val regressions: List[(Pattern, Pattern, String)] =
      (
        toPattern("0") + Pattern.Any + Pattern.Wild,
        Pattern.Any + toPattern("01") + Pattern.Any,
        "001"
      ) ::
        (
          Pattern.Wild + Pattern.Any + Pattern.Any + toPattern("1"),
          toPattern("1") + Pattern.Any + toPattern("1"),
          "111"
        ) ::
        Nil

    regressions.foreach { case (p1, p2, s) => intersectionMatchLaw(p1, p2, s) }
  }

  test("subset is consistent with match regressions") {
    assert(
      setOps.subset(
        toPattern("00") + Pattern.Wild,
        toPattern("0") + Pattern.Wild
      )
    )
    assert(
      setOps.subset(
        toPattern("00") + Pattern.Any + Pattern.Wild,
        toPattern("0") + Pattern.Any + Pattern.Wild
      )
    )
  }

  test("if y - x is empty, (yz - xz) for all strings is empty") {
    def law(x0: Pattern, y0: Pattern, str: String) = {
      val x = Pattern.fromList(x0.toList.take(5))
      val y = Pattern.fromList(y0.toList.take(5))
      if (setOps.difference(y, x) == Nil) {
        val left = y + toPattern(str)
        assertEquals(setOps.difference(left, x + toPattern(str)), Nil)
      }
    }

    forAll(genPattern, genPattern, genSeq)(law(_, _, _))
  }

  test("if y - x is empty, (zy - zx) for all strings is empty") {
    forAll(genPattern, genPattern, genSeq) {
      (x0: Pattern, y0: Pattern, str: String) =>
        val x = Pattern.fromList(x0.toList.take(5))
        val y = Pattern.fromList(y0.toList.take(5))
        if (setOps.difference(y, x) == Nil) {
          val left = toPattern(str) + y
          assertEquals(setOps.difference(left, toPattern(str) + x), Nil)
        }
    }
  }

  test("Named.matches + render agree") {
    def law(n: Named, str: String) =
      namedMatch(n, str).foreach { m =>
        n.render(m) { codePoint =>
          StringUtil.fromCodePoints(codePoint :: Nil)
        } match {
          case Some(s0) => assertEquals(s0, str, s"m = $m")
          case None     =>
            // this can only happen if we have unnamed Wild/AnyElem
            assertEquals(n.isRenderable, false, s"m = $m")
        }
      }

    val prop = forAll(genNamed, genSeq)(law(_, _))

    import NamedSeqPattern.{NCat, Bind, NSeqPart}
    import SeqPart.Wildcard

    val regressions: List[(Named, String)] =
      (
        NCat(Bind("0", NSeqPart(Wildcard)), Bind("1", NSeqPart(Wildcard))),
        ""
      ) ::
        Nil

    regressions.foreach { case (n, s) => law(n, s) }
    prop
  }

  def named(s: String): Named =
    Named.fromPattern(toPattern(s))

  test("Named.matches agrees with Pattern.matches regressions") {

    import Named._
    val regressions: List[(Named, String)] =
      (NCat(NEmpty, NCat(NSeqPart(lit('1')), NSeqPart(Wildcard))), "1") ::
        (NCat(NSeqPart(lit('1')), NSeqPart(Wildcard)), "1") ::
        (NSeqPart(lit('1')), "1") ::
        Nil

    regressions.foreach { case (n, s) => namedMatchesPatternLaw(n, s) }
  }

  test("Test some examples of Named matching") {
    // foo@("bar" baz@(*"baz"))
    val p1 =
      (named("bar") + (Named.Wild + named("baz")).name("baz")).name("foo")
    assertEquals(namedMatch(p1, "bar and baz"), Some(
        Map("foo" -> "bar and baz", "baz" -> " and baz")
      ))
  }

  test("regression subset example") {
    val p1 = Cat(AnyElem, Cat(Wildcard, Empty))
    val p2 = Cat(Wildcard, Cat(lit('0'), Cat(lit('1'), Empty)))

    assert(setOps.subset(p2, p1))
  }

  test("intersection regression") {
    val p1 = Cat(Wildcard, Cat(lit('0'), Cat(lit('1'), Empty)))
    val p2 = Cat(lit('0'), Cat(lit('0'), Cat(lit('0'), Cat(Wildcard, Empty))))

    assertEquals(setOps.relate(p2, p1), Rel.Intersects)
    assert(setOps.intersection(p1, p2).nonEmpty)
  }
}
