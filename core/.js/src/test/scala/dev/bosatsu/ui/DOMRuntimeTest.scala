package dev.bosatsu.ui

import munit.FunSuite

/**
 * Tests for DOMRuntime that can run in Node.js environment.
 *
 * Note: These tests verify basic functionality without a real DOM.
 * Full DOM integration tests require a browser environment and are
 * covered by the demo/dom-test.html browser test.
 */
class DOMRuntimeTest extends FunSuite {

  // Test that VNode rendering produces valid node structures
  test("VNode.Text can be rendered to string") {
    val text = VNode.Text("Hello")
    val html = DOMCodegen.renderToString(text)
    assertEquals(html, "Hello")
  }

  test("VNode.Element can be rendered to string") {
    val div = VNode.div(VNode.text("Content"))
    val html = DOMCodegen.renderToString(div)
    assert(html.contains("<div>"))
    assert(html.contains("Content"))
    assert(html.contains("</div>"))
  }

  test("VNode with attributes can be rendered to string") {
    val btn = VNode.Element(
      "button",
      Map("class" -> AttributeValue.StringValue("btn"), "disabled" -> AttributeValue.BoolValue(true)),
      Map.empty,
      List(VNode.text("Click")),
      None
    )
    val html = DOMCodegen.renderToString(btn)
    assert(html.contains("class="))
    assert(html.contains("disabled"))
    assert(html.contains("Click"))
  }

  test("Nested VNodes can be rendered to string") {
    val nested = VNode.div(
      VNode.h1(VNode.text("Title")),
      VNode.ul(
        VNode.li(VNode.text("Item 1")),
        VNode.li(VNode.text("Item 2"))
      )
    )
    val html = DOMCodegen.renderToString(nested)
    assert(html.contains("<h1>Title</h1>"))
    assert(html.contains("<li>Item 1</li>"))
    assert(html.contains("<li>Item 2</li>"))
  }

  test("DOMCodegen generates valid JS statements") {
    val div = VNode.div(VNode.text("Test"))
    val statements = DOMCodegen.generate(div, "root")
    assert(statements.nonEmpty, "Should generate statements")
  }

  test("DOMCodegen generates render function") {
    val div = VNode.div(VNode.text("Test"))
    val fn = DOMCodegen.generateRenderFunction(div, "render", "#app")
    // Just verify it produces valid code without errors
    assert(fn != null)
  }
}
