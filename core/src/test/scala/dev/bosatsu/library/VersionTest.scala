package dev.bosatsu.library

import dev.bosatsu.{OrderingLaws, Platform}
import org.scalacheck.{Gen, Prop}

import cats.syntax.all._

object VersionGenerator {

  // Helper Generator: Non-negative Int
  private val nonNegativeIntGen: Gen[Long] = Gen.choose(0L, Long.MaxValue)

  // Helper Generator: Non-numeric Identifier (must contain at least one non-digit)
  private val nonNumericIdentifierGen: Gen[String] = for {
    length <- Gen.choose(1, 10) // Limit to 10 characters
    chars <- Gen
      .listOfN(
        length,
        Gen.oneOf(
          ('A' to 'Z') ++
            ('a' to 'z') ++
            ('-' :: ('0' to '9').toList)
        )
      )
      .suchThat(_.exists(c => c.isLetter || c == '-'))
  } yield chars.mkString

  // Helper Generator: Numeric Identifier (no leading zeros unless zero)
  private val numericIdentifierGen: Gen[String] = Gen.oneOf(
    Gen.const("0"),
    for {
      first <- Gen.choose('1', '9')
      rest <- Gen.listOf(Gen.choose('0', '9'))
    } yield (first :: rest).mkString
  )

  // Helper Generator: General Identifier (Numeric or Non-Numeric)
  private val identifierGen: Gen[String] = Gen.oneOf(
    numericIdentifierGen,
    nonNumericIdentifierGen
  )

  // Pre-release Generator: Optional
  val preReleaseGen: Gen[Option[Version.PreRelease]] = Gen.option {
    for {
      identifiers <- Gen.nonEmptyListOf(identifierGen)
      preRelease = identifiers.mkString(".")
    } yield Version.PreRelease(preRelease)
  }

  // Build Metadata Generator: Optional
  val buildMetadataGen: Gen[Option[Version.Build]] = Gen.option {
    for {
      identifiers <- Gen.nonEmptyListOf(identifierGen)
      build = identifiers.mkString(".")
    } yield Version.Build(build)
  }

  // Complete Version Generator
  val versionGen: Gen[Version] = for {
    major <- nonNegativeIntGen
    minor <- nonNegativeIntGen
    patch <- nonNegativeIntGen
    preRelease <- preReleaseGen
    build <- buildMetadataGen
  } yield Version(major, minor, patch, preRelease, build)
}

class VersionTest extends munit.ScalaCheckSuite {

  import VersionGenerator.{preReleaseGen, buildMetadataGen, versionGen}

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 1000 else 20)
      .withMaxDiscardRatio(10)

  def rt(str: String)(implicit loc: munit.Location): Version =
    Version.parser.parseAll(str) match {
      case Right(v1) =>
        assertEquals(v1.toString, str)
        v1
      case Left(err) => fail(show"$err")
    }

  test("parse spec examples") {
    rt("1.0.0")
    rt("1.11.0")
    rt("2.0.0")
    rt("3.1.0")
    rt("4.0.0")
    // pre-release
    rt("1.0.0-alpha")
    rt("1.0.0-0.3.7")
    rt("1.0.0-alpha.1")
    rt("1.0.0-x.7.z.92")
    // with build identifiers
    rt("1.0.0-alpha+001")
    rt("1.0.0+20130313144700")
    rt("1.0.0-beta+exp.sha.5114f85")
    assertEquals(
      rt("1.0.0+21AF26D3----117B344092BD").build,
      Some(Version.Build("21AF26D3----117B344092BD"))
    )
  }

  property("we can always round trip") {
    Prop.forAll(versionGen) { v =>
      assertEquals(rt(v.toString), v)
    }
  }

  property("the ordering is lawful") {
    Prop.forAll(versionGen, versionGen, versionGen) { (a, b, c) =>
      OrderingLaws.law(a, b, c)
    }
  }

  property("versions are only equal when the same") {
    Prop.forAll(versionGen, versionGen) { (a, b) =>
      if (Ordering[Version].equiv(a, b)) {
        assertEquals(a, b)
      }
    }
  }

  def genShuffle[A](items: List[A]): Gen[List[A]] = {
    val len = items.length
    if (len < 2) Gen.const(items)
    else {
      Gen
        .listOfN(len, Gen.double)
        .map { doubles =>
          items.zip(doubles).sortBy(_._2).map(_._1)
        }
    }
  }

  test("test an example from the spec on ordering") {
    // from the spec:
    // 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
    val expectedSortedStrings = List(
      "1.0.0-alpha",
      "1.0.0-alpha.1",
      "1.0.0-alpha.beta",
      "1.0.0-beta",
      "1.0.0-beta.2",
      "1.0.0-beta.11",
      "1.0.0-rc.1",
      "1.0.0"
    )

    Prop.forAll(genShuffle(expectedSortedStrings)) { shuffled =>
      assertEquals(
        shuffled.map(rt(_)).sorted.map(_.toString),
        expectedSortedStrings
      )
    }
  }

  test("1.0.0-alpha < 1.0.0") {
    assert(Version.unsafe("1.0.0-alpha") < Version.unsafe("1.0.0"))
  }

  test("easy orderings") {
    val expectedSortedStrings = List("1.0.0", "2.0.0", "2.1.0", "2.1.1")

    assertEquals(
      expectedSortedStrings.map(rt(_)).sorted.map(_.toString),
      expectedSortedStrings
    )

    val genPosInt = Gen.choose(0, Int.MaxValue)
    Prop.forAll(Gen.listOf(Gen.zip(genPosInt, genPosInt, genPosInt))) { vs =>
      val sorted = vs.sorted
      assertEquals(
        vs.map { case (ma, mi, p) => Version(ma, mi, p) }.sorted,
        sorted.map { case (ma, mi, p) => Version(ma, mi, p) }
      )
    }
  }

  test("build numbers are compared") {
    assert(Version.unsafe("1.0.0+100") < Version.unsafe("1.0.0+101"))
  }

  property("nexts are greater") {
    val v1 = Version.unsafe("1.0.0")
    assertEquals(v1.nextPatch, Version.unsafe("1.0.1"))
    assertEquals(v1.nextMinor, Version.unsafe("1.1.0"))
    assertEquals(v1.nextMajor, Version.unsafe("2.0.0"))

    Prop.forAll(versionGen) { v =>
      assert(Ordering[Version].lt(v, v.nextPatch))
      assert(Ordering[Version].lt(v.nextPatch, v.nextMinor))
      assert(Ordering[Version].lt(v.nextMinor, v.nextMajor))

      assert(v.justBefore(v.nextPatch))
      assert(v.justBefore(v.nextMinor))
      assert(v.justBefore(v.nextMajor))
      if (v.preRelease.isDefined) {
        assert(v.justBefore(v.nextPreRelease.get))
      }
    }
  }

  property("clearBuild has no build defined") {
    Prop.forAll(versionGen) { v =>
      assert(v.clearBuild.build.isEmpty)
    }
  }

  property("clearPreRelease has no preRelease defined") {
    Prop.forAll(versionGen) { v =>
      assert(v.clearPreRelease.preRelease.isEmpty)
    }
  }

  property("version compat partial ordering only compares when major matches") {
    Prop.forAll(versionGen, versionGen) { (a, b) =>
      if (a.major != b.major)
        assertEquals(
          Version.versionCompatiblePartialOrdering.tryCompare(a, b),
          None
        )
      else {
        assertEquals(
          Version.versionCompatiblePartialOrdering.tryCompare(a, b),
          Some(Version.versionOrder.compare(a, b))
        )
      }
    }
  }

  property("all valid versions ifValid") {
    Prop.forAll(versionGen) { v =>
      Version.ifValid(v.major, v.minor, v.patch, v.preRelease, v.build) match {
        case Some(v1) => assertEquals(v1, v)
        case None     => fail(show"expected to be able to validate $v")
      }
    }
  }

  property("a negative version number isn't valid") {
    Prop.forAll(versionGen) { v =>
      assertEquals(
        Version.ifValid(-v.major, v.minor, v.patch, v.preRelease, v.build),
        None
      )
      assertEquals(
        Version.ifValid(v.major, -v.minor, v.patch, v.preRelease, v.build),
        None
      )
      assertEquals(
        Version.ifValid(v.major, v.minor, -v.patch, v.preRelease, v.build),
        None
      )
    }
  }

  property("version ifValid if prerelease is valid") {
    Prop.forAll(
      versionGen,
      Gen.oneOf(
        preReleaseGen,
        Gen.option(Gen.asciiStr.map(Version.PreRelease(_)))
      )
    ) { (v, p) =>
      assertEquals(
        Version.ifValid(v.major, v.minor, v.patch, p, v.build).isDefined,
        p.fold(true)(p =>
          Version.PreRelease.parser.parseAll(p.asString).isRight
        )
      )
    }
  }

  property("version isValid if build is valid") {
    Prop.forAll(
      versionGen,
      Gen
        .oneOf(buildMetadataGen, Gen.option(Gen.asciiStr.map(Version.Build(_))))
    ) { (v, b) =>
      assertEquals(
        Version.ifValid(v.major, v.minor, v.patch, v.preRelease, b).isDefined,
        b.fold(true)(b => Version.Build.parser.parseAll(b.asString).isRight)
      )
    }
  }

  test("00 is a valid Build but not a prerelease, which has to be numeric") {
    assert(Version.Build.parser.parseAll("00").isRight)
    assert(Version.PreRelease.parser.parseAll("00").isLeft)
  }

  property("show == render") {
    Prop.forAll(versionGen) { v =>
      assertEquals(show"$v", v.render)
    }
  }

  property("we match the given regular expressions") {
    val re = java.util.regex.Pattern.compile(
      """^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$"""
    )
    val genStr = Gen.oneOf(
      versionGen.map(_.render),
      Gen.listOf(Gen.asciiStr).map(_.mkString(".")),
      Gen.asciiPrintableStr
    )

    Prop.forAll(genStr) { s =>
      val m = re.matcher(s)
      Version.parser.parseAll(s) match {
        case Right(v) =>
          assert(m.matches())
          assertEquals(m.group(1), v.major.toString)
          assertEquals(m.group(2), v.minor.toString)
          assertEquals(m.group(3), v.patch.toString)
          assertEquals(
            m.group(4),
            v.preRelease.fold(null: String | Null)(_.asString)
          )
          assertEquals(
            m.group(5),
            v.build.fold(null: String | Null)(_.asString)
          )
        case Left(_) =>
          assert(!m.matches())
      }
    }
  }

  property("next prelease works") {
    lazy val somePr: Gen[Version.PreRelease] =
      preReleaseGen.flatMap {
        case None     => somePr
        case Some(pr) => Gen.const(pr)
      }

    assertEquals(
      Version.unsafe("1.0.0-pre.0").nextPreRelease,
      Some(Version.unsafe("1.0.0-pre.1"))
    )

    assertEquals(
      Version.unsafe("1.0.0-rc.1").nextPreRelease,
      Some(Version.unsafe("1.0.0-rc.2"))
    )

    Prop.forAll(versionGen, somePr) { (v, pr) =>
      val pr0 = v.preRelease match {
        case None     => pr
        case Some(pr) => pr
      }
      val v1 = v.copy(preRelease = Some(pr0))

      val v2 = v.copy(preRelease = Some(pr0.next))
      assert(v1 < v2)
      if (v.preRelease.isEmpty) {
        // prereleases come before final releases
        assert(v1 < v)
      }

      v.nextPreRelease match {
        case None     => assert(v.preRelease.isEmpty)
        case Some(v1) => assert(v < v1)
      }
    }
  }
}
