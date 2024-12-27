package org.bykn.bosatsu.library

import org.bykn.bosatsu.{OrderingLaws, Platform}
import org.scalacheck.{Gen, Prop}

import cats.syntax.all._

object VersionGenerator {

  // Helper Generator: Non-negative Int
  private val nonNegativeIntGen: Gen[Int] = Gen.choose(0, Int.MaxValue)

  // Helper Generator: Non-numeric Identifier (must contain at least one non-digit)
  private val nonNumericIdentifierGen: Gen[String] = for {
    length <- Gen.choose(1, 10) // Limit to 10 characters
    chars <- Gen.listOfN(length, Gen.oneOf(
      ('A' to 'Z') ++
      ('a' to 'z') ++
      ('-' :: ('0' to '9').toList)
    )).suchThat(_.exists(c => c.isLetter || c == '-'))
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
  private val preReleaseGen: Gen[Option[String]] = Gen.option {
    for {
      identifiers <- Gen.nonEmptyListOf(identifierGen)
      preRelease = identifiers.mkString(".")
    } yield preRelease
  }

  // Build Metadata Generator: Optional
  private val buildMetadataGen: Gen[Option[String]] = Gen.option {
    for {
      identifiers <- Gen.nonEmptyListOf(identifierGen)
      build = identifiers.mkString(".")
    } yield build
  }

  // Complete Version Generator
  val versionGen: Gen[Version] = for {
    major      <- nonNegativeIntGen
    minor      <- nonNegativeIntGen
    patch      <- nonNegativeIntGen
    preRelease <- preReleaseGen
    build      <- buildMetadataGen
  } yield Version(major, minor, patch, preRelease, build)
}

class VersionTest extends munit.ScalaCheckSuite {

  import VersionGenerator.versionGen

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 1000 else 20)
      .withMaxDiscardRatio(10)


  def rt(str: String)(implicit loc: munit.Location): Version = {
    Version.parser.parseAll(str) match {
      case Right(v1) =>
        assertEquals(v1.toString, str)
        v1
      case Left(err) => fail(show"$err")
    }
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
    assertEquals(rt("1.0.0+21AF26D3----117B344092BD").build, Some("21AF26D3----117B344092BD")) 
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

    assertEquals(expectedSortedStrings.map(rt(_)).sorted.map(_.toString), expectedSortedStrings)
  }

  test("1.0.0-alpha < 1.0.0") {
    assert(Version.unsafe("1.0.0-alpha") < Version.unsafe("1.0.0"))
  }

  test("easy orderings") {
    val expectedSortedStrings = List("1.0.0", "2.0.0", "2.1.0", "2.1.1")
    assertEquals(expectedSortedStrings.map(rt(_)).sorted.map(_.toString), expectedSortedStrings)
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
    }
  }
}