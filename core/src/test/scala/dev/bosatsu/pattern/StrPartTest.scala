package dev.bosatsu.pattern

import dev.bosatsu.Generators.{genStrPat, genValidUtf}
import dev.bosatsu.StringUtil
import dev.bosatsu.Pattern
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

class StrPartTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(100000)
      .withMaxDiscardRatio(10)

  val nonUnicode: Gen[String] =
    Gen.oneOf(
      Gen.asciiStr,
      Gen.identifier
    )

  val genStr: Gen[String] =
    Gen.oneOf(
      Gen.asciiStr,
      Gen.identifier,
      genValidUtf
    )

  private val genCompactPart: Gen[StrPart] =
    Gen.frequency(
      6 -> Gen.oneOf(Gen.const(""), genStr).map(StrPart.LitStr(_)),
      2 -> Gen.const(StrPart.WildStr),
      2 -> Gen.const(StrPart.IndexStr),
      2 -> Gen.const(StrPart.WildChar),
      2 -> Gen.const(StrPart.IndexChar)
    )

  private val genCompactParts: Gen[List[StrPart]] =
    Gen.listOf(genCompactPart)

  private def hasAdjacentLitStr(parts: List[StrPart]): Boolean =
    parts match {
      case StrPart.LitStr(_) :: StrPart.LitStr(_) :: _ =>
        true
      case _ :: tail =>
        hasAdjacentLitStr(tail)
      case Nil =>
        false
    }

  property("StringUtil codePoints work (used internally by matching)") {
    forAll(genStr) { str =>
      val cp = StringUtil.codePoints(str)
      val s1 = StringUtil.fromCodePoints(cp)
      assertEquals(s1, str, s"codepoints = $cp")
    }
  }

  property("pat.matcher works for non-unicode strings") {
    forAll(nonUnicode, genStrPat) { (str, pat) =>
      val sp = pat.toSeqPattern
      StrPart.matchPattern(str, pat) match {
        case Some(binds) =>
          val justNames = binds.map(_._1)
          assertEquals(justNames.distinct, justNames)
          assertEquals(pat.names, justNames)
          // this should agree with the matches method
          assert(
            pat.matcher(str).isDefined,
            s"seqPattern = $sp, named = ${pat.toNamedSeqPattern}"
          )
        case None =>
          assert(
            pat.matcher(str).isEmpty,
            s"seqPattern = $sp, named = ${pat.toNamedSeqPattern}"
          )
      }
    }
  }

  property("matches finds all the bindings in order (unicode)") {
    forAll(genStr, genStrPat) { (str, pat) =>
      StrPart.matchPattern(str, pat) match {
        case Some(binds) =>
          val justNames = binds.map(_._1)
          assertEquals(justNames.distinct, justNames)
          assertEquals(pat.names, justNames)
          // this should agree with the matches method
          assert(pat.matches(str))
        case None =>
          assert(!pat.matches(str))
      }
    }
  }

  property("matchPattern agrees with NamedPattern.matcher (unicode)") {
    val nm = NamedSeqPattern.matcher(Splitter.stringUnit)
    forAll(genStr, genStrPat) { (str, pat) =>
      val namedMatcher = nm(pat.toNamedSeqPattern)

      val res1 = StrPart.matchPattern(str, pat).map { pairs =>
        pairs.map { case (b, sr) =>
          (b.sourceCodeRepr, sr.asStr)
        }.toMap
      }
      val matchRes = namedMatcher(str).map(_._2)
      assertEquals(matchRes, res1)
    }
  }

  property("matches agrees with toRegex") {
    forAll(genStr, genStrPat) { (str, pat) =>
      val re = pat.toRegex
      val matcher = re.matcher(str)

      StrPart.matchPattern(str, pat) match {
        case Some(binds) =>
          assert(matcher.matches(), s"binds = $binds, re = $re")
          val reMatches = (1 to matcher.groupCount).map { idx =>
            matcher.group(idx)
          }.toList

          // TODO: this fails
          assertEquals(
            pat.names.zip(reMatches),
            binds.map { case (k, v) => (k, v.asStr) }
          )
        case None =>
          assert(!matcher.matches())
      }
    }
  }

  property("compact never leaves adjacent LitStr") {
    forAll(genCompactParts) { parts =>
      val compacted = StrPart.compact(parts)
      assert(
        !hasAdjacentLitStr(compacted),
        s"input=$parts compacted=$compacted"
      )
    }
  }

  property("compact is idempotent") {
    forAll(genCompactParts) { parts =>
      val once = StrPart.compact(parts)
      val twice = StrPart.compact(once)
      assertEquals(twice, once, s"input=$parts")
    }
  }

  property("matchString agrees before and after compact") {
    forAll(genStr, genStrPat) { (str, pat) =>
      val parts = pat.parts.toList.map {
        case Pattern.StrPart.NamedStr(_)  => StrPart.IndexStr
        case Pattern.StrPart.NamedChar(_) => StrPart.IndexChar
        case Pattern.StrPart.WildStr      => StrPart.WildStr
        case Pattern.StrPart.WildChar     => StrPart.WildChar
        case Pattern.StrPart.LitStr(s)    => StrPart.LitStr(s)
      }
      val binds = pat.names.size

      val before =
        Option(StrPart.matchString(str, parts, binds)).map(_.toList)
      val after =
        Option(StrPart.matchString(str, StrPart.compact(parts), binds))
          .map(_.toList)

      assertEquals(after, before, s"str=$str, parts=$parts")
    }
  }
}
