package org.bykn.bosatsu.pattern

import org.bykn.bosatsu.Generators.{genStrPat, genValidUtf}
import org.bykn.bosatsu.StringUtil
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
      Gen.identifier,
    )

  val genStr: Gen[String] =
    Gen.oneOf(
      Gen.asciiStr,
      Gen.identifier,
      genValidUtf
    )

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
          assert(pat.matcher(str).isDefined, s"seqPattern = $sp, named = ${pat.toNamedSeqPattern}")
        case None =>
          assert(pat.matcher(str).isEmpty, s"seqPattern = $sp, named = ${pat.toNamedSeqPattern}")
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
        }
        .toMap
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
          val reMatches = (1 to matcher.groupCount)
            .map { idx =>
              matcher.group(idx)  
            }
            .toList

          // TODO: this fails
          assertEquals(pat.names.zip(reMatches), binds.map { case (k, v) => (k, v.asStr)})
        case None =>
          assert(!matcher.matches())
      }
    }
  }
}