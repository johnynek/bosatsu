package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

class SimpleStringPatternTest extends FunSuite {
  import SimpleStringPattern._

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 5)

  def genPattern(names: Set[String]): Gen[(Set[String], Pattern)] = {
    val lit: Gen[(Set[String], Pattern1)] =
      Gen.identifier.map { str => (names, Lit(str)) }

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

  test("matched patterns round trip when rendered") {
    forAll(arbPattern.arbitrary, Gen.identifier) { (p0: Pattern, str: String) =>
      val p = unwild(p0)
      matches(p, str) match {
        case None => ()
        case Some(vars) =>
          // if the unwild matches, the wild must match
          assert(matches(p0, str).isDefined)
          val rendered = render(p, vars)
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
    val p1 = Pattern("'foo${x}bar${y}'")

    assert(matches(p1, "foobar") == Some(Map("x" -> "", "y" -> "")))
    assert(matches(p1, "foobopbarbopbar") == Some(Map("x" -> "bop", "y" -> "bopbar")))
    assert(matches(p1, "foobarbar") == Some(Map("x" -> "", "y" -> "bar")))
    assert(matches(p1, "foobarbarbar") == Some(Map("x" -> "", "y" -> "barbar")))

    val p2 = Pattern("'${front}foo${middle}bar${rest}'")

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
      val pnorm = normalize(p0)
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

  test("onlyMatchesEmpty works") {
    forAll { (p0: Pattern, s: String) =>
      if (p0.onlyMatchesEmpty) {
        assert(matches(p0, "").isDefined)
        if (s != "") assert(matches(p0, s).isEmpty)
      }
    }
  }
}
