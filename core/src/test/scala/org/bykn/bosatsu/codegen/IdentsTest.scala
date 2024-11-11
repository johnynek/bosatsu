package org.bykn.bosatsu.codegen
import org.scalacheck.Prop.forAll

class IdentsTest extends munit.ScalaCheckSuite {
  property("Idents.escape/unescape") {
    forAll { (prefix: String, content: String) =>
      val escaped = Idents.escape(prefix, content)  
      val stringNums = content.map(_.toInt).toList
      Idents.unescape(prefix, escaped) match {
        case Some(c1) => assertEquals(c1, content, s"escaped = $escaped, stringNums = $stringNums")
        case None => fail(s"expected to unescape: $escaped, stringNums = $stringNums")
      }
    }
  }
}