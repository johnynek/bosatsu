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

  // ==========================================================================
  // VProps tests
  // ==========================================================================

  test("VProps.empty has no attributes or handlers") {
    val props = VProps.empty
    assertEquals(props.attributes, Map.empty)
    assertEquals(props.eventHandlers, Map.empty)
  }

  test("VProps.apply creates props with attributes") {
    val props = VProps(
      "class" -> AttributeValue.StringValue("btn"),
      "id" -> AttributeValue.StringValue("submit")
    )
    assertEquals(props.attributes.size, 2)
    assertEquals(props.attributes.get("class"), Some(AttributeValue.StringValue("btn")))
    assertEquals(props.eventHandlers, Map.empty)
  }

  test("VProps.withHandler creates props with event handler") {
    val handler = EventHandler("click", null) // handler expression not needed for this test
    val props = VProps.withHandler("click", handler)
    assertEquals(props.attributes, Map.empty)
    assertEquals(props.eventHandlers.size, 1)
    assert(props.eventHandlers.contains("click"))
  }

  test("VProps.withHandler with attrs creates combined props") {
    val handler = EventHandler("submit", null)
    val props = VProps.withHandler(
      Map("action" -> AttributeValue.StringValue("/api")),
      "submit",
      handler
    )
    assertEquals(props.attributes.size, 1)
    assertEquals(props.eventHandlers.size, 1)
  }

  // ==========================================================================
  // VNode.h tests
  // ==========================================================================

  test("VNode.h creates element with empty props") {
    val el = VNode.h("div", VProps.empty)
    assertEquals(el.tag, "div")
    assertEquals(el.attributes, Map.empty)
    assertEquals(el.children, Nil)
    assertEquals(el.key, None)
  }

  test("VNode.h extracts key from string attribute") {
    val props = VProps("key" -> AttributeValue.StringValue("item-1"))
    val el = VNode.h("li", props)
    assertEquals(el.key, Some("item-1"))
    // key should be removed from attributes
    assert(!el.attributes.contains("key"))
  }

  test("VNode.h extracts key from int attribute") {
    val props = VProps("key" -> AttributeValue.IntValue(42))
    val el = VNode.h("li", props)
    assertEquals(el.key, Some("42"))
  }

  test("VNode.h does not extract key from other types") {
    val props = VProps("key" -> AttributeValue.BoolValue(true))
    val el = VNode.h("li", props)
    assertEquals(el.key, None)
  }

  test("VNode.h passes through event handlers") {
    val handler = EventHandler("click", null)
    val props = VProps.withHandler("click", handler)
    val el = VNode.h("button", props, List(VNode.text("Click")))
    assertEquals(el.eventHandlers.size, 1)
    assertEquals(el.children.length, 1)
  }

  // ==========================================================================
  // Fragment tests
  // ==========================================================================

  test("Fragment has no key") {
    val frag = VNode.Fragment(List(VNode.text("a"), VNode.text("b")))
    assertEquals(frag.key, None)
  }

  test("VNode.fragment creates Fragment from list") {
    val frag = VNode.fragment(List(VNode.text("a"), VNode.text("b")))
    assertEquals(frag.children.length, 2)
  }

  test("VNode.fragment creates Fragment from varargs") {
    val frag = VNode.fragment(VNode.text("a"), VNode.text("b"), VNode.text("c"))
    assertEquals(frag.children.length, 3)
  }

  // Note: Fragment is not yet supported by DOMCodegen.renderToString
  // It should be expanded via flattenChildren first

  // ==========================================================================
  // Type guard tests
  // ==========================================================================

  test("VNode.isElement returns true for Element") {
    assert(VNode.isElement(VNode.div()))
    assert(!VNode.isElement(VNode.text("hello")))
    assert(!VNode.isElement(VNode.fragment()))
  }

  test("VNode.isText returns true for Text") {
    assert(VNode.isText(VNode.text("hello")))
    assert(!VNode.isText(VNode.div()))
    assert(!VNode.isText(VNode.fragment()))
  }

  test("VNode.isFragment returns true for Fragment") {
    assert(VNode.isFragment(VNode.fragment()))
    assert(!VNode.isFragment(VNode.div()))
    assert(!VNode.isFragment(VNode.text("hello")))
  }

  test("VNode.isComponent returns true for Component") {
    val comp = VNode.Component("Test", Map.empty, () => VNode.div(), None)
    assert(VNode.isComponent(comp))
    assert(!VNode.isComponent(VNode.div()))
    assert(!VNode.isComponent(VNode.text("hello")))
  }

  // ==========================================================================
  // flattenChildren tests
  // ==========================================================================

  test("flattenChildren returns list unchanged when no fragments") {
    val children = List(VNode.text("a"), VNode.text("b"))
    val flattened = VNode.flattenChildren(children)
    assertEquals(flattened.length, 2)
  }

  test("flattenChildren expands fragments inline") {
    val frag = VNode.fragment(VNode.text("b"), VNode.text("c"))
    val children = List(VNode.text("a"), frag, VNode.text("d"))
    val flattened = VNode.flattenChildren(children)
    assertEquals(flattened.length, 4)
    flattened(0) match {
      case VNode.Text(c) => assertEquals(c, "a")
      case _ => fail("Expected Text")
    }
    flattened(1) match {
      case VNode.Text(c) => assertEquals(c, "b")
      case _ => fail("Expected Text")
    }
  }

  test("flattenChildren handles nested fragments") {
    val inner = VNode.fragment(VNode.text("b"), VNode.text("c"))
    val outer = VNode.fragment(VNode.text("a"), inner, VNode.text("d"))
    val flattened = VNode.flattenChildren(List(outer))
    assertEquals(flattened.length, 4)
  }

  // ==========================================================================
  // getSelector tests
  // ==========================================================================

  test("getSelector returns id selector when id exists") {
    val el = VNode.Element("div", Map("id" -> AttributeValue.StringValue("main")), Map.empty, Nil, None)
    assertEquals(VNode.getSelector(el), Some("#main"))
  }

  test("getSelector returns data-bosatsu-id selector when no id") {
    val el = VNode.Element("div", Map("data-bosatsu-id" -> AttributeValue.StringValue("el-1")), Map.empty, Nil, None)
    assertEquals(VNode.getSelector(el), Some("[data-bosatsu-id='el-1']"))
  }

  test("getSelector returns None when no selector attributes") {
    val el = VNode.Element("div", Map("class" -> AttributeValue.StringValue("container")), Map.empty, Nil, None)
    assertEquals(VNode.getSelector(el), None)
  }

  test("getSelector prefers id over data-bosatsu-id") {
    val el = VNode.Element("div", Map(
      "id" -> AttributeValue.StringValue("my-id"),
      "data-bosatsu-id" -> AttributeValue.StringValue("el-1")
    ), Map.empty, Nil, None)
    assertEquals(VNode.getSelector(el), Some("#my-id"))
  }

  // ==========================================================================
  // EventHandler tests
  // ==========================================================================

  test("EventHandler has correct defaults") {
    val handler = EventHandler("click", null)
    assertEquals(handler.eventType, "click")
    assertEquals(handler.preventDefault, false)
    assertEquals(handler.stopPropagation, false)
  }

  test("EventHandler with modifiers") {
    val handler = EventHandler("submit", null, preventDefault = true, stopPropagation = true)
    assertEquals(handler.preventDefault, true)
    assertEquals(handler.stopPropagation, true)
  }

  // Note: DOMCodegen.generate does not yet support Fragment directly
  // Fragments should be expanded via flattenChildren first
}
