package dev.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop.forAll

class PatternTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 5000 else 800
    )

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

    val patLawProp = forAll(patGen)(p => law(p))
    law(
      Pattern.Named(
        Identifier.Name("x"),
        Pattern.Named(Identifier.Name("x"), Pattern.WildCard)
      )
    )
    patLawProp
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

  test("substructures is not exponential in the number of unions") {
    // The old algorithm re-walked the rest of the pattern once per union
    // branch, so this took about 2^40 steps.
    val names = (0 until 40).map(i => Identifier.Name(s"x$i")).toList
    val args = names.map { n =>
      Pattern.Union(
        Pattern.Var(n),
        NonEmptyList.one(Pattern.Named(n, Pattern.WildCard))
      )
    }
    val pat = Pattern.PositionalStruct("Foo", args)
    assertEquals(pat.substructures, names)
  }

  // The previous implementation, kept as an oracle. It forks the rest of the
  // walk at every union, so only use it on patterns with few union branches.
  private def substructuresReference(
      pat: Pattern.Parsed
  ): List[Identifier.Bindable] = {
    import Pattern.{ListPart, StrPart}
    type B = Identifier.Bindable

    def loop(
        stack: List[(Pattern.Parsed, Boolean)],
        seen: Set[B],
        acc: List[B]
    ): List[B] =
      stack match {
        case Nil => acc.reverse
        case ((Pattern.WildCard, _) | (Pattern.Literal(_), _)) :: tail =>
          loop(tail, seen, acc)
        case (Pattern.Var(v), isTop) :: tail =>
          if (seen(v) || isTop) loop(tail, seen, acc)
          else loop(tail, seen + v, v :: acc)
        case (Pattern.Named(v, p), isTop) :: tail =>
          if (seen(v) || isTop) loop((p, isTop) :: tail, seen, acc)
          else loop((p, isTop) :: tail, seen + v, v :: acc)
        case (
              Pattern.StrPat(NonEmptyList(StrPart.NamedStr(_), Nil)),
              true
            ) :: tail =>
          loop(tail, seen, acc)
        case (Pattern.StrPat(items), _) :: tail =>
          val globs = items
            .collect { case StrPart.NamedStr(glob) => glob }
            .filterNot(seen)
          loop(tail, seen ++ globs, globs reverse_::: acc)
        case (Pattern.ListPat(ListPart.NamedList(_) :: Nil), true) :: tail =>
          loop(tail, seen, acc)
        case (Pattern.ListPat(items), _) :: tail =>
          val globs = items
            .collect { case ListPart.NamedList(glob) => glob }
            .filterNot(seen)
          val next = items.collect { case ListPart.Item(inner) =>
            (inner, false)
          }
          loop(next ::: tail, seen ++ globs, globs reverse_::: acc)
        case (Pattern.Annotation(p, _), isTop) :: tail =>
          loop((p, isTop) :: tail, seen, acc)
        case (Pattern.PositionalStruct(_, params), _) :: tail =>
          loop(params.map((_, false)) ::: tail, seen, acc)
        case (Pattern.Union(h, t), isTop) :: tail =>
          val all =
            (h :: t.toList).map(p => loop((p, isTop) :: tail, seen, acc))
          val intr = all.map(_.toSet).reduce(_.intersect(_))
          all.flatMap(_.filter(intr)).distinct
      }

    loop((pat, true) :: Nil, Set.empty, Nil).distinct
  }

  // An upper bound on how many walks substructuresReference does.
  private def unionForks(pat: Pattern.Parsed): Long =
    pat match {
      case Pattern.WildCard | Pattern.Literal(_) | Pattern.Var(_) |
          Pattern.StrPat(_) =>
        1L
      case Pattern.Named(_, p)             => unionForks(p)
      case Pattern.Annotation(p, _)        => unionForks(p)
      case Pattern.PositionalStruct(_, ps) =>
        ps.foldLeft(1L)((acc, p) => (acc * unionForks(p)).min(1000000L))
      case Pattern.ListPat(items) =>
        items.foldLeft(1L) {
          case (acc, Pattern.ListPart.Item(p)) =>
            (acc * unionForks(p)).min(1000000L)
          case (acc, _) => acc
        }
      case Pattern.Union(h, t) =>
        (h :: t.toList).foldLeft(0L)((acc, p) => acc + unionForks(p))
    }

  test("substructures matches the previous implementation") {
    forAll(patGen) { p =>
      if (unionForks(p) <= 1000L) {
        assertEquals(p.substructures, substructuresReference(p))
      }
    }
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

  test("normalizeStrPatParts re-compacts literals after duplicate removal") {
    val e = Identifier.Name("e")
    val input = NonEmptyList.of(
      Pattern.StrPart.LitStr("v"),
      Pattern.StrPart.NamedStr(e),
      Pattern.StrPart.LitStr("jptmabf"),
      Pattern.StrPart.NamedStr(e)
    )
    val expected = NonEmptyList.of(
      Pattern.StrPart.LitStr("vjptmabf"),
      Pattern.StrPart.NamedStr(e)
    )

    assertEquals(Generators.normalizeStrPatParts(input), expected)
  }

  test("definitelyTotal detects obvious total patterns") {
    val n = Identifier.Name("n")

    assert(Pattern.Var(n).definitelyTotal)
    assert(Pattern.WildCard.definitelyTotal)
    assert(Pattern.Named(n, Pattern.WildCard).definitelyTotal)
    assert(Pattern.Annotation(Pattern.Var(n), "Type").definitelyTotal)
    assert(
      Pattern.ListPat(List(Pattern.ListPart.NamedList(n))).definitelyTotal
    )
    assert(
      Pattern
        .StrPat(NonEmptyList.one(Pattern.StrPart.NamedStr(n)))
        .definitelyTotal
    )
    assert(
      Pattern
        .union(Pattern.Literal(Lit.fromInt(1)), Pattern.Var(n) :: Nil)
        .definitelyTotal
    )

    assert(!Pattern.PositionalStruct("Foo", Nil).definitelyTotal)
    assert(
      !Pattern
        .ListPat(List(Pattern.ListPart.Item(Pattern.WildCard)))
        .definitelyTotal
    )
    assert(
      !Pattern
        .StrPat(NonEmptyList.one(Pattern.StrPart.WildChar))
        .definitelyTotal
    )
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

  test("searchInfo distinguishes search patterns from ambiguous bindings") {
    def bindable(name: String): Identifier.Bindable =
      Identifier.Name(name)

    def yesSearch(str: String): Unit =
      assert(
        pat(str).isSearchPattern,
        s"expected search pattern: $str"
      )

    def noSearch(str: String): Unit =
      assert(
        !pat(str).isSearchPattern,
        s"did not expect search pattern: $str"
      )

    def bindings(str: String): Set[Identifier.Bindable] =
      pat(str).ambiguousBindings

    def expectBindings(str: String, names: Set[String]): Unit =
      assertEquals(
        bindings(str),
        names.map(bindable),
        s"unexpected ambiguous bindings for: $str"
      )

    noSearch("Foo(x)")
    noSearch("[x, y]")
    noSearch("[*xs]")
    noSearch("[x, *y]")
    noSearch("[x, y, *zs]")
    noSearch("Foo([x, *y])")

    yesSearch("[*_, x]")
    yesSearch("[x, *mid, y]")
    yesSearch("Foo([*_, x])")
    yesSearch(""""ab${middle}cd"""")
    yesSearch(""""${prefix}$.{ch}z"""")

    expectBindings("[*xs]", Set.empty)
    expectBindings("[x, *y]", Set.empty)
    expectBindings("[x, y, *zs]", Set.empty)
    expectBindings("Foo([x, *y])", Set.empty)
    expectBindings("[*_, x]", Set.empty)
    expectBindings("[x, *mid, y]", Set.empty)
    expectBindings("Foo([*_, x])", Set.empty)
    expectBindings(""""ab${middle}cd"""", Set.empty)
    expectBindings(""""${prefix}$.{ch}z"""", Set.empty)

    expectBindings("[x, *_, y, *_]", Set("y"))
    expectBindings("[*_, x, *_]", Set("x"))
    expectBindings("[*pre, x, *post]", Set("pre", "x", "post"))
    expectBindings("Foo([*_, x, *_])", Set("x"))
    expectBindings(""""${left}$.{ch}${right}"""", Set("left", "ch", "right"))
    expectBindings(""""$.{x}${_}${_}$.{y}"""", Set.empty)
  }
}
