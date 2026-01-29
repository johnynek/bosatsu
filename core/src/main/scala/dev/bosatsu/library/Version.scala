package dev.bosatsu.library

import cats.parse.Numbers.{bigInt, nonNegativeIntString}
import cats.parse.Parser
import cats.parse.Rfc5234.{digit, alpha}
import cats.syntax.all._
import cats.{Order, PartialOrder, Show}
import com.monovore.decline.Argument
import scala.math.PartialOrdering
import _root_.bosatsu.{TypedAst => proto}

import dev.bosatsu.Json
import dev.bosatsu.Json.JString

case class Version(
    major: Long,
    minor: Long,
    patch: Long,
    preRelease: Option[Version.PreRelease],
    build: Option[Version.Build]
) {

  // This expresses the valid next versions from a given version
  def justBefore(next: Version): Boolean =
    if (major == next.major) {
      if (minor == next.minor) {
        if (patch == next.patch) {
          // we are bumping pre-release
          nextPreRelease.exists { v1 =>
            // the next has to be <= to we have
            Ordering[Version].lteq(v1, next)
          }
        } else {
          (patch + 1L) == next.patch
        }
      } else {
        ((minor + 1L) == next.minor) &&
        (next.patch == 0L)
      }
    } else {
      ((major + 1L) == next.major) &&
      (next.minor == 0L) &&
      (next.patch == 0L)
    }

  def toProto: proto.Version =
    proto.Version(
      major = major,
      minor = minor,
      patch = patch,
      preRelease = preRelease.fold("")(_.asString),
      build = build.fold("")(_.asString)
    )

  override def toString(): String = render

  lazy val render: String = {
    val pre = preRelease match {
      case None    => ""
      case Some(p) => "-" + p.asString
    }
    val bld = build match {
      case None    => ""
      case Some(b) => "+" + b.asString
    }

    s"${major}.${minor}.${patch}$pre$bld"
  }

  def clearBuild: Version =
    if (build.isEmpty) this
    else copy(build = None)

  def clearPreRelease: Version =
    if (preRelease.isEmpty) this
    else copy(preRelease = None)

  def nextPatch: Version =
    copy(patch = patch + 1, preRelease = None, build = None)

  def nextMinor: Version =
    copy(minor = minor + 1, patch = 0, preRelease = None, build = None)

  def nextMajor: Version =
    copy(
      major = major + 1,
      minor = 0,
      patch = 0,
      preRelease = None,
      build = None
    )

  def next(dk: Version.DiffKind): Version =
    dk match {
      case Version.DiffKind.Major => nextMajor
      case Version.DiffKind.Minor => nextMinor
      case Version.DiffKind.Patch => nextPatch
    }

  /** If the pre-release is set, increment so we have the next valid pre-release
    * if it is not set, you should set with something like: copy(preRelease =
    * Some(PreRelease("pre"))) or "rc", or "alpha", etc..
    */
  def nextPreRelease: Option[Version] =
    preRelease.map { pr =>
      copy(preRelease = Some(pr.next))
    }

  def diffKindTo(that: Version): Version.DiffKind =
    if (major == that.major) {
      if (minor == that.minor) Version.DiffKind.Patch
      else Version.DiffKind.Minor
    } else {
      Version.DiffKind.Major
    }
}

object Version {
  case class Build(asString: String)
  case class PreRelease(asString: String) {
    def next: PreRelease = {
      val ary = PreRelease.splitPreRelease(this)
      PreRelease.parseNumeric(ary(ary.length - 1)) match {
        case Some(n) =>
          ary(ary.length - 1) = (n + 1).toString
          PreRelease(ary.mkString("."))
        case None =>
          // there is no trailing number, just append 0
          PreRelease(asString + ".0")
      }
    }
  }

  val zero: Version = Version(0L, 0L, 0L)

  // Actually the spec doesn't limit the version numbers, but it says you shouldn't use
  // really long version strings (e.g. < 255 almost certainly). Since Long can hold all
  // decimals of 18 characters (and some 19), this seems like a good trade-off for
  // ergonomics of the common case, and spec conforming.
  // Since using 32 bit timestamps as patch numbers is plausible, I wanted to support
  // more than just Int, which will soon overflow.
  private val numericIdentifier: Parser[Long] =
    nonNegativeIntString.flatMap { str =>
      val bi = BigInt(str)
      if (bi.isValidLong) Parser.pure(bi.toLong)
      else Parser.failWith(s"$str would overflow Long")
    }

  private val dot: Parser[Unit] = Parser.char('.')
  private val nonDigit = alpha | Parser.char('-').as('-')
  private val identChar = digit | nonDigit
  private val identChars = identChar.rep.string

  object PreRelease {

    /** Splits the pre-release string into identifiers. Example: "alpha.1" ->
      * List("alpha", "1")
      */
    private def splitPreRelease(pre: PreRelease): Array[String] =
      pre.asString.split('.')

    /** Attempts to parse a string as an integer. Returns Some(Int) if
      * successful, None otherwise.
      */
    private def parseNumeric(id: String): Option[BigInt] =
      bigInt.parseAll(id).toOption

    /** Compares two pre-release identifiers according to SemVer rules.
      *
      * @param a
      *   First identifier
      * @param b
      *   Second identifier
      * @return
      *   Ordering result: -1 if a < b, 0 if a == b, 1 if a > b
      */
    private def compareIdentifiers(a: String, b: String): Int =
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

    /** Compares two lists of pre-release identifiers.
      *
      * @param a
      *   List of identifiers from first version
      * @param b
      *   List of identifiers from second version
      * @return
      *   Ordering result
      */
    private def comparePreRelease(a: Array[String], b: Array[String]): Int = {
      var idx = 0
      while ((idx < a.length) && (idx < b.length)) {
        val idA = a(idx)
        val idB = b(idx)
        if (idA == idB) {
          // Continue to next identifier
        } else {
          return compareIdentifiers(idA, idB)
        }
        idx += 1
      }
      // All compared identifiers are equal; longer list has higher precedence
      a.length.compareTo(b.length)
    }

    implicit val preReleaseOrdering: Ordering[PreRelease] =
      new Ordering[PreRelease] {
        def compare(x: PreRelease, y: PreRelease): Int =
          comparePreRelease(splitPreRelease(x), splitPreRelease(y))
      }

    private val digits = digit.rep.string
    // at least one non-digit
    private val alphaNumIdent: Parser[String] =
      // non-digit is first
      ((nonDigit *> identChars.?) |
        // digits first then non-digit, then maybe more idents
        // this needs soft because we need to backtrack if we don't find
        // any nonDigit, because some sequences of digits only are invalid
        // we need prerelease numbers to not have leading 0
        (digits.soft *> nonDigit *> identChars.?)).string

    // this looks like maybe identChars, but note that 00 wouldn't be a valid
    // prerelease identifier, but would be a valid build identifier
    private val preReleaseIdentifier = alphaNumIdent | nonNegativeIntString

    val parser: Parser[PreRelease] =
      preReleaseIdentifier.repSep(dot).string.map(PreRelease(_))
  }

  object Build {
    // note this allows any sequence of digits, which includes 00000...
    private val buildIdent: Parser[String] = identChars

    val parser: Parser[Build] = buildIdent.repSep(dot).string.map(Build(_))
  }

  // Combine all parts to form the complete version parser
  val parser: Parser[Version] = {
    val numDot = numericIdentifier <* dot
    val build = Parser.char('+') *> Build.parser
    val prerelease = Parser.char('-') *> PreRelease.parser
    (
      (numDot, numDot, numericIdentifier).tupled ~ (
        prerelease.?,
        build.?
      ).tupled
    ).map { case ((maj, min, patch), (p, b)) => Version(maj, min, patch, p, b) }
  }

  def unsafe(str: String): Version =
    parser.parseAll(str) match {
      case Right(value) => value
      case Left(value)  => sys.error(show"failed to parse: $str: $value")
    }

  def apply(major: Long, minor: Long, patch: Long): Version =
    Version(major, minor, patch, None, None)

  def apply(major: Int, minor: Int, patch: Int): Version =
    apply(major, minor, patch, None, None)

  def apply(
      major: Int,
      minor: Int,
      patch: Int,
      preRelease: Option[PreRelease],
      build: Option[Build]
  ): Version =
    Version(major.toLong, minor.toLong, patch.toLong, preRelease, build)

  def fromProto(pv: proto.Version): Version =
    Version(
      major = pv.major,
      minor = pv.minor,
      patch = pv.patch,
      preRelease = Option(pv.preRelease).flatMap {
        case "" => None
        case pr => Some(PreRelease(pr))
      },
      build = Option(pv.build).flatMap {
        case "" => None
        case b  => Some(Build(b))
      }
    )

  def ifValid(
      major: Long,
      minor: Long,
      patch: Long,
      preRelease: Option[PreRelease],
      build: Option[Build]
  ): Option[Version] =
    if ((major >= 0) && (minor >= 0) && (patch >= 0)) {
      (preRelease, build) match {
        case (None, None) =>
          Some(Version(major, minor, patch, None, None))
        case (None, Some(b)) =>
          Build.parser.parseAll(b.asString).toOption.map { b =>
            Version(major, minor, patch, None, Some(b))
          }
        case (Some(p), None) =>
          PreRelease.parser.parseAll(p.asString).toOption.map { p =>
            Version(major, minor, patch, Some(p), None)
          }
        case (Some(p), Some(b)) =>
          (
            PreRelease.parser.parseAll(p.asString).toOption,
            Build.parser.parseAll(b.asString).toOption
          ).mapN { (p, b) =>
            Version(major, minor, patch, Some(p), Some(b))
          }
      }
    } else None

  implicit val versionOrder: Order[Version] = new Order[Version] {
    def noneLast[A](ord: Ordering[A]): Order[Option[A]] =
      new Order[Option[A]] {
        def compare(x: Option[A], y: Option[A]): Int =
          x match {
            case None =>
              y match {
                case None    => 0
                case Some(_) => 1
              }
            case Some(xvalue) =>
              y match {
                case None         => -1
                case Some(yvalue) =>
                  ord.compare(xvalue, yvalue)
              }
          }
      }

    val preOrd = noneLast(PreRelease.preReleaseOrdering)
    val buildOrd =
      preOrd.contramap[Option[Build]](_.map(b => PreRelease(b.asString)))

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
      val preCompare = preOrd.compare(v1.preRelease, v2.preRelease)
      if (preCompare != 0) return preCompare
      // (this violates the spec, but we want equality only when exactly the same)
      buildOrd.compare(v1.build, v2.build)
    }
  }

  /** Defines the Ordering for Version according to SemVer 2.0.0
    */
  implicit val versionOrdering: Ordering[Version] = versionOrder.toOrdering

  /** if one version can replace another, it is gteq. If the major versions
    * disagree, we can't compare
    */
  val versionCompatiblePartialOrder: PartialOrder[Version] =
    new PartialOrder[Version] {
      def partialCompare(x: Version, y: Version): Double =
        if (x.major == y.major) {
          versionOrder.compare(x, y).toDouble
        } else java.lang.Double.NaN
    }

  val versionCompatiblePartialOrdering: PartialOrdering[Version] =
    PartialOrder.catsKernelPartialOrderingForPartialOrder(
      using versionCompatiblePartialOrder
    )

  implicit val versionShow: Show[Version] =
    new Show[Version] {
      def show(t: Version): String = t.render
    }

  implicit val versionWriter: Json.Writer[Version] =
    Json.Writer.from[Version](v => Json.JString(v.render))

  implicit val versionReader: Json.Reader[Version] =
    new Json.Reader[Version] {
      def describe: String = "Version"
      def read(
          path: Json.Path,
          j: Json
      ): Either[(String, Json, Json.Path), Version] =
        j match {
          case JString(str) =>
            parser.parseAll(str) match {
              case right @ Right(_) => right.leftCast
              case Left(value)      =>
                Left((show"couldn't parse semver: $value", j, path))
            }
          case notStr =>
            Left(("expected string", notStr, path))
        }
    }

  implicit val versionArg: Argument[Version] =
    dev.bosatsu.Parser.argFromParser(
      Version.parser,
      "semver",
      "Version",
      "Expects a val semver string."
    )

  sealed abstract class DiffKind(val name: String) derives CanEqual {
    def isMajor: Boolean = this == DiffKind.Major
    def isMinor: Boolean = this == DiffKind.Minor
    def isPatch: Boolean = this == DiffKind.Patch
  }

  object DiffKind {
    case object Patch extends DiffKind("patch")
    case object Minor extends DiffKind("minor")
    case object Major extends DiffKind("major")

    implicit val orderDiffKind: Order[DiffKind] =
      Order.by {
        case Patch => 0
        case Minor => 1
        case Major => 2
      }

    implicit val orderingDiffKind: Ordering[DiffKind] =
      Ordering.by {
        case Patch => 0
        case Minor => 1
        case Major => 2
      }
  }
}
