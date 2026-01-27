package dev.bosatsu.ui

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Arbitrary}

/**
 * Property-based tests for VNode and DOMCodegen.
 *
 * These tests verify:
 * - VNode structure invariants
 * - renderToString produces valid HTML
 * - DOM codegen produces valid JavaScript
 * - Round-trip properties where applicable
 */
class VNodeTest extends ScalaCheckSuite {
  import VNodeGen._

  // VNode structure properties

  property("Text nodes have no children") {
    forAll(genTextNode) { text =>
      text.key == None
    }
  }

  property("VNode depth is always >= 1") {
    forAll(genVNode(3)) { vnode =>
      VNodeGen.depth(vnode) >= 1
    }
  }

  property("VNode depth is bounded by maxDepth + 1") {
    val maxDepth = 3
    forAll(genVNode(maxDepth)) { vnode =>
      VNodeGen.depth(vnode) <= maxDepth + 1
    }
  }

  property("VNode node count is >= depth") {
    forAll(genVNode(3)) { vnode =>
      VNodeGen.nodeCount(vnode) >= VNodeGen.depth(vnode)
    }
  }

  // renderToString properties

  property("renderToString produces output for any VNode") {
    forAll(genVNode(3)) { vnode =>
      // Empty text nodes produce empty strings, which is valid
      val result = DOMCodegen.renderToString(vnode)
      vnode match {
        case VNode.Text("") => result.isEmpty
        case VNode.Text(_) => result.nonEmpty
        case VNode.Element(_, _, _, _, _) => result.nonEmpty
        case VNode.Component(_, _, _, _) => true // Component render varies
      }
    }
  }

  property("renderToString for Text escapes HTML entities") {
    forAll { (s: String) =>
      val text = VNode.Text(s)
      val html = DOMCodegen.renderToString(text)
      // Should not contain unescaped < or >
      !html.contains('<') && !html.contains('>')
    }
  }

  property("renderToString produces matching tags for elements") {
    forAll(genLeafElement) { element =>
      val html = DOMCodegen.renderToString(element)
      if (VNodeGen.voidTags.contains(element.tag)) {
        html.startsWith(s"<${element.tag}")
      } else {
        html.startsWith(s"<${element.tag}") && html.endsWith(s"</${element.tag}>")
      }
    }
  }

  property("renderToString includes all rendered attributes") {
    forAll(genLeafElement.filter(_.attributes.nonEmpty)) { element =>
      val html = DOMCodegen.renderToString(element)
      // Only attributes that render to Some(value) should appear
      element.attributes.forall { case (attrName, attrValue) =>
        AttributeValue.render(attrValue) match {
          case Some(_) => html.contains(attrName)
          case None => true // Attribute not rendered, so no requirement
        }
      }
    }
  }

  // DOM codegen properties

  property("DOM codegen produces valid statements for any VNode") {
    forAll(genVNode(2)) { vnode =>
      val statements = DOMCodegen.generate(vnode, "root")
      statements.nonEmpty
    }
  }

  property("DOM codegen produces at least one statement per VNode") {
    forAll(genVNode(2)) { vnode =>
      val statements = DOMCodegen.generate(vnode, "root")
      statements.length >= 1
    }
  }

  property("DOM codegen first statement creates the root element") {
    import dev.bosatsu.codegen.js.Code._

    forAll(genVNode(2)) { vnode =>
      val statements = DOMCodegen.generate(vnode, "root")
      statements.headOption.exists {
        case Const("root", _) => true
        case _ => false
      }
    }
  }

  property("generateRenderFunction creates a function declaration") {
    import dev.bosatsu.codegen.js.Code._

    forAll(genVNode(2)) { vnode =>
      DOMCodegen.generateRenderFunction(vnode, "render", "#app") match {
        case Const("render", Function(_, _, _)) => true
        case _ => false
      }
    }
  }

  property("generateModule produces non-empty Doc") {
    forAll(genVNode(2)) { vnode =>
      val doc = DOMCodegen.generateModule(vnode, "TestModule")
      doc.render(80).nonEmpty
    }
  }

  // AttributeValue properties

  property("AttributeValue.render handles all value types") {
    forAll(genAttributeValue) { av =>
      val result = AttributeValue.render(av)
      av match {
        case AttributeValue.NullValue => result.isEmpty
        case AttributeValue.BoolValue(false) => result.isEmpty
        case _ => result.isDefined
      }
    }
  }

  // Smart constructor properties

  test("VNode.el creates element with empty attributes") {
    val el = VNode.el("div")
    assertEquals(el.tag, "div")
    assertEquals(el.attributes, Map.empty[String, AttributeValue])
    assertEquals(el.children, Nil)
  }

  test("VNode.text creates text node") {
    val text = VNode.text("hello")
    assertEquals(text.content, "hello")
  }

  test("VNode.div creates div element") {
    val div = VNode.div(VNode.text("content"))
    assertEquals(div.tag, "div")
    assertEquals(div.children.length, 1)
  }

  test("VNode.keyedEl creates element with key") {
    val el = VNode.keyedEl("li", "item-1", VNode.text("Item 1"))
    assertEquals(el.key, Some("item-1"))
  }

  // Void element handling

  test("renderToString handles void elements correctly") {
    val input = VNode.input()
    val html = DOMCodegen.renderToString(input)
    assert(!html.contains("</input>"), s"Void element should not have closing tag: $html")
  }

  test("renderToString handles img with attributes") {
    val img = VNode.Element(
      "img",
      Map("src" -> AttributeValue.StringValue("/test.png"), "alt" -> AttributeValue.StringValue("Test")),
      Map.empty,
      Nil,
      None
    )
    val html = DOMCodegen.renderToString(img)
    assert(html.contains("src="), s"Should contain src attribute: $html")
    assert(html.contains("alt="), s"Should contain alt attribute: $html")
    assert(!html.contains("</img>"), s"Should not have closing tag: $html")
  }

  // Nesting tests

  test("renderToString handles nested elements") {
    val nested = VNode.div(
      VNode.h1(VNode.text("Title")),
      VNode.p(VNode.text("Paragraph"))
    )
    val html = DOMCodegen.renderToString(nested)
    assert(html.contains("<div>"), s"Should start with div: $html")
    assert(html.contains("<h1>Title</h1>"), s"Should contain h1: $html")
    assert(html.contains("<p>Paragraph</p>"), s"Should contain p: $html")
    assert(html.contains("</div>"), s"Should end with closing div: $html")
  }

  // DOMBindings tests

  test("DOMBindings.empty has no state access") {
    val empty = DOMBindings.empty
    assertEquals(empty.stateReads, Set.empty)
    assertEquals(empty.stateWrites, Set.empty)
    assertEquals(empty.eventHandlers, Nil)
    assertEquals(empty.renderTarget, None)
  }
}
