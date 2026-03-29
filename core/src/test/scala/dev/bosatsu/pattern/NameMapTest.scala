package dev.bosatsu.pattern

import dev.bosatsu.{Generators, Identifier, Pattern}

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class NameMapTest extends munit.ScalaCheckSuite {
  import NameMap.Rename

  private type TestPattern = Pattern[String, Unit]

  private def b(name: String): Identifier.Bindable =
    Identifier.Name(name)

  private val genPatternNoUnion: Gen[Pattern.Parsed] =
    Generators.genPattern(depth = 4, useUnion = false)

  private val genPatternPairNoUnion: Gen[(Pattern.Parsed, Pattern.Parsed)] =
    Gen.zip(genPatternNoUnion, genPatternNoUnion)

  private def normalizedRename(
      rename: Rename,
      subst: Map[Identifier.Bindable, Identifier.Bindable]
  ): Rename =
    rename match {
      case Rename.Same     => Rename.Same
      case Rename.Removed  => Rename.Removed
      case Rename.To(name) => Rename.To(subst.getOrElse(name, name))
    }

  private def renameMapUnderSubst(
      nameMap: NameMap,
      subst: Map[Identifier.Bindable, Identifier.Bindable]
  ): NameMap = {
    val superNames1 = nameMap.superNames.iterator.map { case (from, rename) =>
      val from1 = subst.getOrElse(from, from)
      from1 -> normalizedRename(rename, subst)
    }.toMap

    val addedInSub1 = nameMap.addedInSub.iterator.map(n => subst.getOrElse(n, n)).toSet

    NameMap(superNames1, addedInSub1)
  }

  private def injectiveSubstFor(
      names: Set[Identifier.Bindable]
  ): Map[Identifier.Bindable, Identifier.Bindable] =
    names.toList.sorted.zipWithIndex.map { case (name, idx) =>
      name -> Identifier.synthetic(s"pnm_sub_$idx")
    }.toMap

  private def assertCoherent[N, T](
      superPattern: Pattern[N, T],
      subPattern: Pattern[N, T],
      nameMap: NameMap
  ): Unit = {
    assertEquals(nameMap.superNames.keySet, superPattern.names.toSet)

    val mappedTargets = nameMap.superNames.iterator.flatMap {
      case (from, Rename.Same)    => Some(from)
      case (_, Rename.To(name))   => Some(name)
      case (_, Rename.Removed)    => None
    }.toSet

    assert(mappedTargets.subsetOf(subPattern.names.toSet))
    assertEquals(nameMap.addedInSub, subPattern.names.toSet -- mappedTargets)

    nameMap.superNames.foreach { case (from, rename) =>
      val expected =
        rename match {
          case Rename.Same    => Some(from)
          case Rename.To(name) => Some(name)
          case Rename.Removed => None
        }
      assertEquals(nameMap.superToSub(from), expected)
    }
  }

  test("alignSubsumedPatternNames records rename and removal cases") {
    val superPattern: TestPattern =
      Pattern.PositionalStruct("Pair", List(Pattern.Var(b("a")), Pattern.Var(b("b"))))
    val subPattern: TestPattern =
      Pattern.PositionalStruct("Pair", List(Pattern.Var(b("x")), Pattern.WildCard))

    val actual = NameMap.alignSubsumedPatternNames(superPattern, subPattern)
    val expected = NameMap(
      Map(
        b("a") -> Rename.To(b("x")),
        b("b") -> Rename.Removed
      ),
      Set.empty
    )

    assertEquals(actual, Some(expected))
  }

  test("alignSubsumedPatternNames captures added names in sub pattern") {
    val superPattern: TestPattern =
      Pattern.PositionalStruct("Pair", List(Pattern.Var(b("a")), Pattern.WildCard))
    val subPattern: TestPattern =
      Pattern.PositionalStruct("Pair", List(Pattern.WildCard, Pattern.Var(b("c"))))

    val actual = NameMap.alignSubsumedPatternNames(superPattern, subPattern)
    val expected = NameMap(
      Map(b("a") -> Rename.Removed),
      Set(b("c"))
    )

    assertEquals(actual, Some(expected))
  }

  test("alignSubsumedPatternNames fails for mismatched constructors") {
    val left: TestPattern = Pattern.PositionalStruct("Left", Nil)
    val right: TestPattern = Pattern.PositionalStruct("Right", Nil)

    assertEquals(NameMap.alignSubsumedPatternNames(left, right), None)
  }

  test("alignSubsumedPatternNames aligns through a subsuming union branch") {
    val superPattern: TestPattern =
      Pattern.union(
        Pattern.PositionalStruct("Left", List(Pattern.Var(b("a")))),
        Pattern.PositionalStruct("Right", List(Pattern.Var(b("a")))) :: Nil
      )
    val subPattern: TestPattern =
      Pattern.PositionalStruct("Right", List(Pattern.Var(b("b"))))

    val actual = NameMap.alignSubsumedPatternNames(superPattern, subPattern)
    val expected = NameMap(
      Map(b("a") -> Rename.To(b("b"))),
      Set.empty
    )

    assertEquals(actual, Some(expected))
  }

  test("alignSubsumedPatternNames keeps disjunctive alternatives for union conflicts") {
    val superPattern: TestPattern =
      Pattern.union(
        Pattern.PositionalStruct("Pair", List(Pattern.Var(b("a")), Pattern.WildCard)),
        Pattern.PositionalStruct("Pair", List(Pattern.WildCard, Pattern.Var(b("a")))) :: Nil
      )
    val subPattern: TestPattern =
      Pattern.PositionalStruct("Pair", List(Pattern.Var(b("x")), Pattern.Var(b("y"))))

    val actual = NameMap.alignSubsumedPatternNames(superPattern, subPattern)
    assert(actual.nonEmpty)

    val alternatives =
      actual.get.substitutionAlternatives(Set(b("a"))).toSet

    assertEquals(
      alternatives,
      Set(
        Map(b("a") -> b("x")),
        Map(b("a") -> b("y"))
      )
    )
  }

  test("maybeOr preserves both deterministic alternatives") {
    val left = NameMap(Map(b("a") -> Rename.To(b("x"))), Set.empty)
    val right = NameMap(Map(b("a") -> Rename.To(b("y"))), Set.empty)

    val merged = NameMap.maybeOr(left, right)

    val alternatives = merged.substitutionAlternatives(Set(b("a"))).toSet
    assertEquals(
      alternatives,
      Set(
        Map(b("a") -> b("x")),
        Map(b("a") -> b("y"))
      )
    )
  }

  test("alignSubsumedPatternNames returns coherent maps for generated aligned pairs") {
    forAll(genPatternPairNoUnion) { case (superPattern, subPattern) =>
      NameMap.alignSubsumedPatternNames(superPattern, subPattern) match {
        case Some(nameMap) =>
          assertCoherent(superPattern, subPattern, nameMap)
        case None =>
          ()
      }
    }
  }

  test("generated patterns align against their unbound form") {
    forAll(genPatternNoUnion) { superPattern =>
      val subPattern = superPattern.unbind
      NameMap.alignSubsumedPatternNames(superPattern, subPattern) match {
        case Some(nameMap) =>
          assertCoherent(superPattern, subPattern, nameMap)
          assert(nameMap.superNames.valuesIterator.forall(_ == Rename.Removed))
          assertEquals(nameMap.addedInSub, Set.empty)
        case None =>
          fail(s"expected alignment success for super=$superPattern sub=$subPattern")
      }
    }
  }

  test("alignSubsumedPatternNames can become defined after unbind") {
    val superPattern: Pattern.Parsed =
      Pattern.ListPat(
        List(Pattern.ListPart.NamedList(Identifier.Name("lzjqp9ilw")))
      )
    val subPattern: Pattern.Parsed =
      Pattern.Named(
        Identifier.Operator("&"),
        Pattern.ListPat(
          List(Pattern.ListPart.NamedList(Identifier.Name("mmoedu4mrp4")))
        )
      )

    val d0 = NameMap.alignSubsumedPatternNames(superPattern, subPattern).isDefined
    val d1 = NameMap
      .alignSubsumedPatternNames(superPattern.unbind, subPattern)
      .isDefined
    val d2 = NameMap
      .alignSubsumedPatternNames(superPattern, subPattern.unbind)
      .isDefined
    val d3 = NameMap
      .alignSubsumedPatternNames(superPattern.unbind, subPattern.unbind)
      .isDefined

    assertEquals(d0, false)
    assertEquals(d1, true)
    assertEquals(d2, true)
    assertEquals(d3, true)
  }

  test("successful namemap is stable under simultaneous injective substitute") {
    forAll(genPatternPairNoUnion) { case (superPattern, subPattern) =>
      NameMap.alignSubsumedPatternNames(superPattern, subPattern) match {
        case Some(nameMap) =>
          val allNames = superPattern.names.toSet ++ subPattern.names.toSet
          val subst = injectiveSubstFor(allNames)

          val super1 = superPattern.substitute(subst)
          val sub1 = subPattern.substitute(subst)

          val remapped = renameMapUnderSubst(nameMap, subst)

          val next = NameMap.alignSubsumedPatternNames(super1, sub1)
          assertEquals(next, Some(remapped))
          assertCoherent(super1, sub1, remapped)
        case None =>
          ()
      }
    }
  }
}
