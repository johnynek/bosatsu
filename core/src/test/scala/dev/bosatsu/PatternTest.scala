package dev.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop.forAll

class PatternTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(5000)

  val patGen = Gen.choose(0, 5).flatMap(Generators.genPattern(_))

  def pat(s: String): Pattern.Parsed =
    Parser.unsafeParse(Pattern.bindParser, s)

  test("Pattern.unbind is the same as filterVars(Set.empty)") {
    forAll(patGen) { p =>
      assertEquals(p.unbind, p.filterVars(Set.empty))
    }
  }

  test("filtering for names not in a pattern is unbind") {
    forAll(patGen, Gen.listOf(Gen.identifier)) { (p, ids0) =>
      val ids = ids0.map(Identifier.unsafe(_))
      assertEquals(
        p.unbind,
        p.filterVars(ids.toSet.filterNot(p.names.toSet[Identifier]))
      )
    }
  }

  test("filtering and keeping all names is identity") {
    forAll(patGen) { p =>
      assertEquals(p.filterVars(p.names.toSet), p)
    }
  }

  test("substructures is a subset of names") {
    forAll(patGen) { p =>
      p.substructures.toSet.subsetOf(p.names.toSet)
    }
  }

  test("topNames is a subset of names") {
    forAll(patGen) { p =>
      p.topNames.toSet.subsetOf(p.names.toSet)
    }
  }

  test("topNames and substructures are disjoint") {
    forAll(patGen) { p =>
      if (p.collisionBinds.isEmpty)
        p.topNames.toSet.intersect(p.substructures.toSet).isEmpty
      else true
    }
  }

  test("singlynamed implies there is exacly one name") {
    def law(p: Pattern.Parsed) =
      p match {
        case Pattern.SinglyNamed(n) =>
          assertEquals(p.topNames, (n :: Nil))
          assertEquals(p.names, (n :: Nil))
          // we can name with the same name, and still be singly named
          assertEquals(
            Pattern.SinglyNamed.unapply(Pattern.Named(n, p)),
            Some(n)
          )
          // we can annotate and not lose singly named-ness
          assertEquals(
            Pattern.SinglyNamed.unapply(Pattern.Annotation(p, null)),
            Some(n)
          )
          // we can make a union and not lose singly named-ness
          assertEquals(
            Pattern.SinglyNamed.unapply(
              Pattern.union(Pattern.Var(n), p :: Nil)
            ),
            Some(n)
          )
        case _ =>
      }

    forAll(patGen)(p => law(p))
    law(
      Pattern.Named(
        Identifier.Name("x"),
        Pattern.Named(Identifier.Name("x"), Pattern.WildCard)
      )
    )
  }

  test("test some examples for singly named") {
    def check(str: String, nm: String) =
      pat(str) match {
        case Pattern.SinglyNamed(n) => assertEquals(n, Identifier.unsafe(nm))
        case other                  => fail(s"expected singlynamed: $other")
      }

    def checkNot(str: String) =
      pat(str) match {
        case Pattern.SinglyNamed(n) => fail(s"unexpected singlynamed: $n")
        case _                      => ()
      }

    check("foo", "foo")
    check("foo as foo", "foo")
    check("(foo as foo) as foo", "foo")
    check("Foo(_, _)as foo", "foo")
    check("foo: T", "foo")
    check("(_: T) as foo", "foo")
    check("(_, _) as foo", "foo")
    check("foo | foo", "foo")
    check("[*_] as foo", "foo")
    check("(X as foo) | (Y as foo)", "foo")

    checkNot("foo | bar")
    checkNot("Foo(x, y)as foo")
    checkNot("(x, y)as foo")
    checkNot("_")
    checkNot("[foo]")
    checkNot("Foo(x, y)")
    checkNot("bar as foo")
    checkNot("Foo(foo)")
  }

  test("substructures don't include total matches") {
    val foo = Identifier.Name("foo")
    val bar = Identifier.Name("bar")
    assert(Pattern.Var(foo).substructures.isEmpty)
    assert(Pattern.Annotation(Pattern.Var(foo), "Type").substructures.isEmpty)
    assert(
      Pattern
        .Union(Pattern.Var(foo), NonEmptyList.of(Pattern.Var(bar)))
        .substructures
        .isEmpty
    )
  }

  test("unions with total matches work correctly") {
    val foo = Identifier.Name("foo")
    val inner = Pattern.Var(foo)
    val struct = Pattern.PositionalStruct("Foo", inner :: Nil)
    // Note, foo can't be substructural because on the right it is total
    assert(Pattern.Union(struct, NonEmptyList.of(inner)).substructures.isEmpty)
  }

  test("StrPat.isTotal works") {
    forAll(Generators.genStrPat, Arbitrary.arbitrary[String]) { (pat, str) =>
      pat match {
        case sp @ Pattern.StrPat(_) if sp.isTotal =>
          (0 until str.length).foreach { len =>
            assert(sp.matcher(str.take(len)).isDefined)
          }
        case _ => ()
      }
    }
  }

  test("substitute identity is identity") {
    forAll(patGen, Gen.listOf(Generators.bindIdentGen)) { (p, list) =>
      assertEquals(p.substitute(list.map(b => (b, b)).toMap), p)
    }
  }

  test("substitute names homomorphism") {
    import Identifier._

    def law[A, B](p: Pattern[A, B], map: Map[Bindable, Bindable]) = {
      val subsP = p.substitute(map)
      assertEquals(
        subsP.names.distinct,
        p.names.map(n => map.getOrElse(n, n)).distinct,
        s"got $subsP"
      )
    }

    def b(s: String) = Identifier.Name(s)

    {
      import Pattern._
      import StrPart._
      import Lit.Str

      val p = Union(
        Var(Name("a")),
        NonEmptyList(
          StrPat(
            NonEmptyList(NamedStr(Name("k")), List(LitStr("wrk"), WildChar))
          ),
          List(
            Named(Name("hqZ9aeuAood"), WildCard),
            Literal(Str("q5VgEdksu")),
            WildCard
          )
        )
      )

      law(p, Map(b("k") -> b("a")))
    }

    forAll(
      patGen,
      Gen.mapOf(Gen.zip(Generators.bindIdentGen, Generators.bindIdentGen))
    ) { (p, map) =>
      law(p, map)
    }
  }
}
