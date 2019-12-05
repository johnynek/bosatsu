package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

class SimpleStringPatternTest extends FunSuite {
  import SimpleStringPattern._

  // generate a string of 0s and 1s to make matches more likely
  val genBitString: Gen[String] =
    for {
      sz <- Gen.choose(0, 4)
      g = Gen.oneOf(0, 1)
      list <- Gen.listOfN(sz, g)
    } yield list.mkString

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 5)

  def genPattern(names: Set[String]): Gen[(Set[String], Pattern)] = {
    val lit: Gen[(Set[String], Pattern1)] =
      genBitString.map { str => (names, Lit(str)) }

    val varP: Gen[(Set[String], Pattern1)] =
      Gen.identifier.filter { s => !names(s) }.map { n => (names + n, Var(n)) }

    val cat = for {
      (n1, ph) <- Gen.oneOf(lit, varP)
      (n2, ptail) <- genPattern(n1)
    } yield (n2, Cat(ph, ptail))

    Gen.frequency(
      (2, lit),
      (1, Gen.const((names, Wildcard))),
      (2, varP),
      (15, cat))
  }

  implicit lazy val shrinkPat: Shrink[Pattern] = {
    val ss = implicitly[Shrink[String]]
    Shrink {
      case Lit(s) =>
        (0 until s.length).toStream.map { t =>
          Lit(s.take(t))
        }
      case Var(x) => Wildcard #:: Stream.empty
      case Wildcard => Lit("") #:: Stream.empty
      case Cat(h, tail) =>
        val sh = shrinkPat.shrink(h).iterator
        val st = shrinkPat.shrink(tail).iterator

        (new Iterator[Pattern] {
          var state: (Iterator[Pattern], Iterator[Pattern]) = (sh, st)
          def hasNext = state._1.hasNext || state._2.hasNext
          def next = {
            if (state._1.hasNext) {
              val res = state._1.next
              state = state.swap
              res
            }
            else {
              val res = state._2.next
              state = state.swap
              res
            }
          }
        }).toStream
    }
  }


  def unvar(p: Pattern): String =
    p match {
      case Lit(str) => str
      case Var(_) | Wildcard => ""
      case Cat(h, tail) => unvar(h) ++ unvar(tail)
    }

  def unwild(p: Pattern): Pattern =
    p match {
      case l@(Lit(_) | Var(_)) => l
      case Wildcard => Lit("")
      case Cat(lv@(Lit(_) | Var(_)), tail) => Cat(lv, unwild(tail))
      case Cat(Wildcard, tail) => unwild(tail)
    }

  def unlit(p: Pattern): Pattern =
    p match {
      case Lit(_) => Wildcard
      case vw@(Wildcard | Var(_)) => vw
      case Cat(Lit(_), tail) => unlit(tail)
      case Cat(vw@(Wildcard | Var(_)), tail) => unlit(tail)
    }

  implicit val arbPattern: Arbitrary[Pattern] =
    Arbitrary(genPattern(Set.empty).map(_._2))

  implicit val arbString: Arbitrary[String] = Arbitrary(genBitString)

  test("matched patterns round trip when rendered") {
    forAll { (p0: Pattern, str: String) =>
      val p = unwild(p0)
      matches(p, str) match {
        case None => ()
        case Some(vars) =>
          // if the unwild matches, the wild must match
          assert(matches(p0, str).isDefined)
          val rendered = p.render(vars)
          val renderWild = p0.render(vars)
          assert(renderWild.isEmpty == (p0 != p))
          assert(rendered == Some(str), s"${rendered.get.length} != ${str.length}, ${rendered.get.toArray.toList.map(_.toInt)} != ${str.toArray.toList.map(_.toInt)}")
      }
    }
  }

  test("if we unvar, we always match") {
    forAll { p0: Pattern =>
      val p = unwild(p0)
      val str = unvar(p)
      matches(p, str) match {
        case None => fail("expected to match")
        case Some(m) => m.foreach { case (k, v) =>
          assert(v == "", s"key: $k, $v")
        }
      }
    }
  }

  test("unlit patterns match everything") {
    forAll { (p0: Pattern, str: String) =>
      val p = unlit(p0)
      assert(matches(p, str).isDefined)
    }
  }

  test("test some examples") {
    val dollar = "$"
    val p1 = Pattern(s"'foo$dollar{x}bar$dollar{y}'")

    assert(matches(p1, "foobar") == Some(Map("x" -> "", "y" -> "")))
    assert(matches(p1, "foobopbarbopbar") == Some(Map("x" -> "bop", "y" -> "bopbar")))
    assert(matches(p1, "foobarbar") == Some(Map("x" -> "", "y" -> "bar")))
    assert(matches(p1, "foobarbarbar") == Some(Map("x" -> "", "y" -> "barbar")))

    val p2 = Pattern(s"'$dollar{front}foo$dollar{middle}bar$dollar{rest}'")

    assert(matches(p2, "0foo1foo2bar3") == Some(Map("front" -> "0", "middle" -> "1foo2", "rest" -> "3")))
  }

  test("wildcard on either side is the same as contains") {
    forAll { (ps: String, s: String) =>
      assert(matches(Cat(Wildcard, Cat(Lit(ps), Wildcard)), s).isDefined == s.contains(ps))
    }
  }
  test("wildcard on front side is the same as endsWith") {
    forAll { (ps: String, s: String) =>
      assert(matches(Cat(Wildcard, Lit(ps)), s).isDefined == s.endsWith(ps))
    }
  }
  test("wildcard on back side is the same as startsWith") {
    forAll { (ps: String, s: String) =>
      assert(matches(Cat(Lit(ps), Wildcard), s).isDefined == s.startsWith(ps))
    }
  }
  test("normalized patterns have the same matches as unnormalized ones") {
    forAll { (p0: Pattern, s: String) =>
      val pnorm = p0.normalize
      val nm = matches(pnorm, s)
      val m = matches(p0, s)
      assert(m.isDefined == nm.isDefined)
      if (m.isDefined) {
        m.get.keys.foreach { k =>
          val mv = m.get(k)
          val nv = nm.get.getOrElse(k, "")
          assert(mv == nv, s"key: $k $m, $nm")
        }
      }
    }
  }

  test("normalized patterns don't have adjacent Var/Wildcard or adjacent Lit") {
    forAll { p0: Pattern =>
      val list = p0.normalize.toList
      list.sliding(2).foreach {
        case bad@Seq(Var(_) | Wildcard, Var(_) | Wildcard) =>
          fail(s"saw adjacent: $bad in ${p0.normalize}")
        case bad@Seq(Lit(_), Lit(_)) =>
          fail(s"saw adjacent: $bad in ${p0.normalize}")
        case _ => ()
      }
    }
  }

  test("toList never emits empty Lit") {
    forAll { p: Pattern =>
      assert(!p.toList.exists {
        case Lit("") => true
        case _ => false
      })
    }

    assert(Cat(Lit("0100"),Cat(Wildcard,Lit(""))).normalize == Cat(Lit("0100"), Wildcard))
  }

  test("onlyMatchesEmpty works") {
    forAll { (p0: Pattern, s: String) =>
      if (p0.onlyMatchesEmpty) {
        assert(matches(p0, "").isDefined)
        if (s != "") assert(matches(p0, s).isEmpty)
      }
    }
  }

  test("unname matches the same strings") {
    forAll { (p0: Pattern, s: String) =>
      val pnorm = p0.unname
      assert(matches(p0, s).isDefined == matches(pnorm, s).isDefined)
    }
  }

  test("intersection(p1, p2).matches(x) == p1.matches(x) && p2.matches(x)") {
    def law(p10: Pattern, p20: Pattern, x: String) = {
      // intersections get really slow if they get too big
      val p1 = Pattern.fromList(p10.toList.take(4))
      val p2 = Pattern.fromList(p20.toList.take(4))
      val n1 = p1.normalize.unname
      val n2 = p2.normalize.unname
      val rawintr = p1.intersection(p2)
      val intersect = rawintr.map(_.normalize).distinct
      val sep = p1.doesMatch(x) && p2.doesMatch(x)
      val together = intersect.exists(_.doesMatch(x))

      assert(together == sep, s"n1: $n1, n2: $n2, intersection: $intersect")
      //if (together != sep) sys.error(s"n1: $n1, n2: $n2, intersection: $intersect")
      //else succeed
    }

    forAll(law(_, _, _))
    val regressions: List[(Pattern, Pattern, String)] =
      List((Cat(Lit("111"), Wildcard), Cat(Lit("1"), Wildcard), "111"))

    regressions.foreach { case (p1, p2, s) => law(p1, p2, s) }
  }

  test("intersection is commutative") {
    def law(p1: Pattern, p2: Pattern, x: String) =
      assert(p1.intersection(p2).exists(_.doesMatch(x)) ==
        p2.intersection(p1).exists(_.doesMatch(x)))

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

      val left = leftI.exists(_.doesMatch(x))

      val rightI = for {
        i1 <- p2.intersection(p3).iterator
        i2 <- p1.intersection(i1).iterator
      } yield i2

      val right = rightI.exists(_.doesMatch(x))

      //assert(left == right, s"\n\n$leftI\n\n$rightI")
      assert(left == right)
    }

    forAll(law(_, _, _, _))

    val regressions: List[(Pattern, Pattern, Pattern, String)] =
      List(
        (Cat(Lit("11"),Cat(Lit(""),Lit("001"))), Var("r"), Wildcard, "")
      )

    regressions.foreach { case (p1, p2, p3, s) => law(p1, p2, p3, s) }
  }

  test("matches any does match any") {
    forAll { (p: Pattern, s: String) =>
      if (p.matchesAny) assert(p.doesMatch(s))
    }
  }

  test("difference is an upper bound") {
    forAll { (p1: Pattern, p2: Pattern, s: String) =>
      // true difference is <= diff
      val diff = difference(p1, p2)
      val diffmatch = diff.exists(_.doesMatch(s))

      if (diffmatch) assert(p1.doesMatch(s))

      if (p1.doesMatch(s) && !p2.doesMatch(s)) {
        assert(diffmatch)
      }

      if (diff.isEmpty && p1.doesMatch(s)) {
        assert(p2.doesMatch(s))
      }

      if (p2.matchesAny) assert(diff == Nil)

      // the law we wish we had:
      //if (p2.doesMatch(s) && p1.doesMatch(s)) assert(!diffmatch)
    }
  }

  test("difference is idempotent: (a - b) = c, c - b == c") {
    forAll { (a: Pattern, b: Pattern) =>
      val c = a.difference(b)
      val c1 = c.flatMap(_.difference(b))
      assert(c == c1)
    }
  }

  test("a n a == a") {
    forAll { (a: Pattern) =>
      assert(a.intersection(a).map(_.normalize.unname).distinct == List(a.normalize.unname))
    }
  }

  test("a - a == 0") {
    def law(p1: Pattern, p2: Pattern) = {
      if (p1.normalize.unname == p2.normalize.unname) {
        assert(p1.difference(p2) == Nil)
      }
      assert(p1.difference(p1) == Nil)
      assert(p2.difference(p2) == Nil)
    }
    forAll(law(_, _))

    val regressions: List[(Pattern, Pattern)] =
      List(
        (
          Pattern.fromList(List(Lit("0"), Lit("1"), Var("x"), Var("y"), Var("z"))),
          Pattern.fromList(List(Lit("01"), Var("x"), Wildcard))))

    regressions.foreach { case (p1, p2) => law(p1, p2) }

  }

  test("if x matches any, (y'z' - x'z') is empty") {
    forAll { (x: Pattern, y: Pattern, str: String) =>
      if (x.matchesAny) {
        assert(y.difference(x) == Nil)
        assert(y.appendString(str).difference(x.appendString(str)) == Nil)
      }
    }
  }

  test("if a n b = 0 then a - b = a") {
    def law(p1: Pattern, p2: Pattern) = {
      val inter = p1.intersection(p2)
      val diff = p1.difference(p2)

      if (inter.isEmpty) {
        assert(diff.map(_.normalize) == p1.normalize :: Nil)
      }
    }

    forAll(law(_, _))

    val regressions: List[(Pattern, Pattern)] =
      List((Cat(Lit("10"),Lit("00")), Cat(Lit("1000"),Lit("110"))))

    regressions.foreach { case (p1, p2) => law(p1, p2) }
  }
}
