package org.bykn.bosatsu.library

import cats.Order
import cats.parse.Parser
import cats.parse.Rfc5234.{digit, alpha}
import cats.parse.Numbers.nonZeroDigit
import cats.syntax.all._

case class Version(
  major: Int,
  minor: Int,
  patch: Int,
  preRelease: Option[String],
  build: Option[String]
) {
  override def toString(): String = render

  lazy val render: String = {
    val pre = preRelease.fold("")(p => s"-$p")
    val bld = build.fold("")(b => s"+$b")

    s"${major}.${minor}.${patch}$pre$bld"
  }

  def nextPatch: Version =
    copy(patch = patch + 1, preRelease = None, build = None)

  def nextMinor: Version =
    copy(minor = minor + 1, patch = 0, preRelease = None, build = None)

  def nextMajor: Version =
    copy(major = major + 1, minor = 0, patch = 0, preRelease = None, build = None)
}

object Version {

    /**
    * Splits the pre-release string into identifiers.
    * Example: "alpha.1" -> List("alpha", "1")
    */
  private def splitPreRelease(pre: String): List[String] = pre.split('.').toList

  /**
    * Attempts to parse a string as an integer.
    * Returns Some(Int) if successful, None otherwise.
    */
  private def parseNumeric(id: String): Option[Int] =
    scala.util.Try(id.toInt).toOption

  /**
    * Compares two pre-release identifiers according to SemVer rules.
    *
    * @param a First identifier
    * @param b Second identifier
    * @return Ordering result: -1 if a < b, 0 if a == b, 1 if a > b
    */
  private def compareIdentifiers(a: String, b: String): Int = {
    (parseNumeric(a), parseNumeric(b)) match {
      case (Some(numA), Some(numB)) =>
        numA.compareTo(numB)
      case (Some(_), None) =>
        // Numeric identifiers have lower precedence than non-numeric
        -1
      case (None, Some(_)) =>
        1
      case (None, None) =>
        a.compareTo(b)
    }
  }

  /**
    * Compares two lists of pre-release identifiers.
    *
    * @param a List of identifiers from first version
    * @param b List of identifiers from second version
    * @return Ordering result
    */
  private def comparePreRelease(a: List[String], b: List[String]): Int = {
    val zipped = a.iterator.zipAll(b, "", "")
    while (zipped.hasNext) {
      val (idA, idB) = zipped.next()
      if (idA.isEmpty) return -1
      if (idB.isEmpty) return 1

      if (idA == idB) {
        // Continue to next identifier
      } else {
        return compareIdentifiers(idA, idB)
      }
    }
    // All compared identifiers are equal; longer list has higher precedence
    a.length.compareTo(b.length)
  }
  implicit val versionOrder: Order[Version] = new Order[Version] {
    val preRelOrd: Order[String] = new Order[String] {
      def compare(pre1: String, pre2: String): Int = {
        val pre1Ids = splitPreRelease(pre1)
        val pre2Ids = splitPreRelease(pre2)
        comparePreRelease(pre1Ids, pre2Ids)
      }
    }

    val optPre: Order[Option[String]] =
      new Order[Option[String]] {
        def compare(x: Option[String], y: Option[String]): Int =
          x match {
            case None =>
              y match {
                case None => 0
                case Some(_) => 1
              }
            case Some(xvalue) =>
              y match {
                case None => -1
                case Some(yvalue) =>
                  preRelOrd.compare(xvalue, yvalue)
              }
          }
      }

    override def compare(v1: Version, v2: Version): Int = {
      // 1. Compare major versions
      val majorCompare = v1.major.compareTo(v2.major)
      if (majorCompare != 0) return majorCompare

      // 2. Compare minor versions
      val minorCompare = v1.minor.compareTo(v2.minor)
      if (minorCompare != 0) return minorCompare

      // 3. Compare patch versions
      val patchCompare = v1.patch.compareTo(v2.patch)
      if (patchCompare != 0) return patchCompare

      // 4. Compare pre-release versions
      val preCompare = optPre.compare(v1.preRelease, v2.preRelease)
      if (preCompare != 0) return preCompare
      // (this violates the spec, but we want equality only when exactly the same)
      optPre.compare(v1.build, v2.build)
    }
  }

  /**
    * Defines the Ordering for Version according to SemVer 2.0.0
    */
  implicit val versionOrdering: Ordering[Version] = versionOrder.toOrdering

  // Helper parser for numeric identifiers (no leading zeros unless the number is zero)
  private val numericIdentifierString: Parser[String] =
    (Parser.char('0'))
      .orElse((nonZeroDigit *> digit.rep0).void)
      .string

  private val numericIdentifier: Parser[Int] =
      numericIdentifierString.flatMap { str =>
        if (BigInt(str).isValidInt) Parser.pure(str.toInt)
        else Parser.failWith(s"$str would overflow Int")
      }

  // Parser for the dot '.'
  private val dot: Parser[Unit] = Parser.char('.')

  private val nonDigit = alpha | Parser.char('-').as('-')
  private val digits = digit.rep.string
  private val identChar = digit | nonDigit
  private val identChars = identChar.rep.string

  // at least one non-digit
  private val alphaNumIdent: Parser[String] =
    // non-digit is first
    ((nonDigit *> identChars.?) |
    // digits first then non-digit, then maybe more idents
    (digits *> nonDigit *> identChars.?)).string

  private val buildIdent: Parser[String] =
    (digit | nonDigit).rep.string

  private val preReleaseIdentifier = alphaNumIdent.backtrack | numericIdentifierString

  private val build = Parser.char('+') *> buildIdent.repSep(dot).string
  private val prerelease = Parser.char('-') *> preReleaseIdentifier.repSep(dot).string

  // Combine all parts to form the complete version parser
  val parser: Parser[Version] =
    ((numericIdentifier <* dot,
      numericIdentifier <* dot,
      numericIdentifier).tupled ~ (prerelease.?, build.?).tupled
    ).map { case ((maj, min, patch), (p, b)) => Version(maj, min, patch, p, b) }


  def unsafe(str: String): Version =
    parser.parseAll(str) match {
      case Right(value) => value
      case Left(value) => sys.error(show"failed to parse: $str: $value")
    }
}