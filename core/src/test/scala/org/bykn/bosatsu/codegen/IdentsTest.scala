package org.bykn.bosatsu.codegen
import org.scalacheck.Prop.forAll

class IdentsTest extends munit.ScalaCheckSuite {
  val validIdentChars =
    (('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')).toSet + '_'

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

  test("allSimpleIdents escape to identity") {
    Idents.allSimpleIdents.take(10000).foreach { str =>
      assertEquals(Idents.escape("", str), str)  
    }
  }

  test("allSimpleIdents are distinct") {
    assertEquals(Idents.allSimpleIdents.take(10000).toSet.size, 10000)
  }

  property("escape starts with prefix") {
    forAll { (prefix: String, content: String) =>
      assert(Idents.escape(prefix, content).startsWith(prefix))
    }
  }

  property("escape creates validIdentChars") {
    forAll { (prefix: String, content: String) =>
      val escaped = Idents.escape(prefix, content)
      assert(escaped.drop(prefix.length).forall(validIdentChars))
    }
  }

  property("valid strings are escaped with identity") {
    forAll { (prefix: String, content: String) =>
      val escaped = Idents.escape(prefix, content)
      if (content.forall(validIdentChars)) {
        assertEquals(escaped, prefix + content.flatMap {
          case '_' => "__"
          case a => a.toString
        })
      }
      else {
        assert(escaped.length > (prefix + content).length)
      }
    }
  }

  test("some examples") {
    assertEquals(Idents.escape("foo", "bar_baz"), "foobar__baz")
  }
}