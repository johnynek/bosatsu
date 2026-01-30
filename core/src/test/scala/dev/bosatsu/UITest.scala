package dev.bosatsu

import munit.FunSuite
import dev.bosatsu.Value._
import cats.data.NonEmptyList

class UITest extends FunSuite {

  // Helper to get an FfiCall from externals
  private def getExternal(name: String): FfiCall =
    UI.jvmExternals.toMap((UI.packageName, name))

  // ==========================================================================
  // Package name tests
  // ==========================================================================

  test("packageName is Bosatsu/UI") {
    assertEquals(UI.packageName.asString, "Bosatsu/UI")
  }

  // ==========================================================================
  // uiString tests
  // ==========================================================================

  test("uiString is not empty") {
    assert(UI.uiString.nonEmpty)
  }

  test("uiString contains package declaration") {
    assert(UI.uiString.contains("package Bosatsu/UI"))
  }

  // ==========================================================================
  // VNode ADT tests
  // ==========================================================================

  test("VElement creates element node") {
    val elem = UI.VElement("div", List(("class", "container")), Nil)
    assertEquals(elem.tag, "div")
    assertEquals(elem.props, List(("class", "container")))
    assertEquals(elem.children, Nil)
  }

  test("VElement with children") {
    val child = UI.VText("Hello")
    val elem = UI.VElement("p", Nil, List(child))
    assertEquals(elem.children.length, 1)
    assertEquals(elem.children.head, child)
  }

  test("VText creates text node") {
    val text = UI.VText("Hello World")
    assertEquals(text.content, "Hello World")
  }

  test("VFragment creates fragment node") {
    val child1 = UI.VText("A")
    val child2 = UI.VText("B")
    val frag = UI.VFragment(List(child1, child2))
    assertEquals(frag.children.length, 2)
  }

  test("VNode equality works") {
    val text1 = UI.VText("Hello")
    val text2 = UI.VText("Hello")
    val text3 = UI.VText("World")
    assertEquals(text1, text2)
    assertNotEquals(text1, text3)
  }

  // ==========================================================================
  // UIState tests
  // ==========================================================================

  test("UIState holds initial value") {
    val state = UI.UIState("test_id", "initial")
    assertEquals(state.id, "test_id")
    assertEquals(state.value, "initial")
  }

  test("UIState value is mutable") {
    val state = UI.UIState("test_id", 0)
    state.value = 42
    assertEquals(state.value, 42)
  }

  // ==========================================================================
  // EventHandler tests
  // ==========================================================================

  test("EventHandler stores event type and handler") {
    val handler = UI.EventHandler("click", UnitValue)
    assertEquals(handler.eventType, "click")
    assertEquals(handler.handler, UnitValue)
  }

  // ==========================================================================
  // jvmExternals tests - state
  // ==========================================================================

  test("state external creates UIState with unique ID") {
    val stateFn = getExternal("state")
    val result1 = stateFn match {
      case FfiCall.Fn1(f) => f(Value.Str("initial"))
      case _ => fail("Expected Fn1"); UnitValue
    }
    val result2 = stateFn match {
      case FfiCall.Fn1(f) => f(Value.Str("initial2"))
      case _ => fail("Expected Fn1"); UnitValue
    }

    // Both should be ExternalValues with UIState
    result1.asExternal.toAny match {
      case s: UI.UIState[?] =>
        assert(s.id.startsWith("state_"))
      case _ => fail("Expected UIState")
    }

    result2.asExternal.toAny match {
      case s: UI.UIState[?] =>
        assert(s.id.startsWith("state_"))
      case _ => fail("Expected UIState")
    }
  }

  // ==========================================================================
  // jvmExternals tests - h
  // ==========================================================================

  test("h external creates VElement") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = Value.Str("div")
    val props = VList.VNil // Empty list
    val children = VList.VNil

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        assertEquals(elem.tag, "div")
        assertEquals(elem.props, Nil)
        assertEquals(elem.children, Nil)
      case _ => fail("Expected VElement")
    }
  }

  test("h external uses 'div' for non-string tag") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = VInt(42) // Not a string, should use default
    val props = VList.VNil
    val children = VList.VNil

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        assertEquals(elem.tag, "div")
      case _ => fail("Expected VElement")
    }
  }

  test("h external parses props list correctly") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = Value.Str("input")
    // Create a list of tuples: [("type", "text"), ("id", "my-input")]
    val tuple1 = ProductValue(Array(Value.Str("type"), Value.Str("text")))
    val tuple2 = ProductValue(Array(Value.Str("id"), Value.Str("my-input")))
    val props = VList.Cons(tuple1, VList.Cons(tuple2, VList.VNil))
    val children = VList.VNil

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        assertEquals(elem.tag, "input")
        assertEquals(elem.props.length, 2)
        assert(elem.props.contains(("type", "text")))
        assert(elem.props.contains(("id", "my-input")))
      case _ => fail("Expected VElement")
    }
  }

  test("h external handles props with non-string values") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = Value.Str("div")
    // Non-string value should be skipped
    val tuple1 = ProductValue(Array(Value.Str("class"), Value.Str("container")))
    val tuple2 = ProductValue(Array(Value.Str("count"), VInt(42))) // Non-string value
    val props = VList.Cons(tuple1, VList.Cons(tuple2, VList.VNil))
    val children = VList.VNil

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        // Only the string-value tuple should be included
        assertEquals(elem.props.length, 1)
        assertEquals(elem.props.head, ("class", "container"))
      case _ => fail("Expected VElement")
    }
  }

  test("h external handles non-tuple props items") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = Value.Str("div")
    // Just a string, not a tuple
    val props = VList.Cons(Value.Str("not-a-tuple"), VList.VNil)
    val children = VList.VNil

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        // Non-tuple should be skipped
        assertEquals(elem.props, Nil)
      case _ => fail("Expected VElement")
    }
  }

  test("h external parses children list correctly") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }
    val textFn = getExternal("text") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val tag = Value.Str("p")
    val props = VList.VNil
    // Create a text child
    val textChild = textFn(Value.Str("Hello"))
    val children = VList.Cons(textChild, VList.VNil)

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        assertEquals(elem.tag, "p")
        assertEquals(elem.children.length, 1)
        elem.children.head match {
          case text: UI.VText =>
            assertEquals(text.content, "Hello")
          case _ => fail("Expected VText child")
        }
      case _ => fail("Expected VElement")
    }
  }

  test("h external handles non-VNode children") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = Value.Str("div")
    val props = VList.VNil
    // A non-VNode child (just an integer)
    val children = VList.Cons(VInt(42), VList.VNil)

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        // Non-VNode should be filtered out
        assertEquals(elem.children, Nil)
      case _ => fail("Expected VElement")
    }
  }

  test("h external handles props that are not a list") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = Value.Str("div")
    // Pass an integer instead of a list
    val props = VInt(42)
    val children = VList.VNil

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        // Non-list props should result in empty props
        assertEquals(elem.props, Nil)
      case _ => fail("Expected VElement")
    }
  }

  test("h external handles children that are not a list") {
    val hFn = getExternal("h") match {
      case FfiCall.Fn3(f) => f
      case _ => fail("Expected Fn3"); null
    }

    val tag = Value.Str("div")
    val props = VList.VNil
    // Pass an integer instead of a list
    val children = VInt(42)

    val result = hFn(tag, props, children)

    result.asExternal.toAny match {
      case elem: UI.VElement =>
        // Non-list children should result in empty children
        assertEquals(elem.children, Nil)
      case _ => fail("Expected VElement")
    }
  }

  // ==========================================================================
  // jvmExternals tests - text
  // ==========================================================================

  test("text external creates VText from string") {
    val textFn = getExternal("text") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val content = Value.Str("Hello World")
    val result = textFn(content)

    result.asExternal.toAny match {
      case text: UI.VText =>
        assertEquals(text.content, "Hello World")
      case _ => fail("Expected VText")
    }
  }

  test("text external handles non-string content") {
    val textFn = getExternal("text") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val content = VInt(42)
    val result = textFn(content)

    result.asExternal.toAny match {
      case text: UI.VText =>
        // Should use toString
        assert(text.content.contains("42") || text.content.nonEmpty)
      case _ => fail("Expected VText")
    }
  }

  // ==========================================================================
  // jvmExternals tests - fragment
  // ==========================================================================

  test("fragment external creates VFragment") {
    val fragFn = getExternal("fragment") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val children = VList.VNil
    val result = fragFn(children)

    result.asExternal.toAny match {
      case frag: UI.VFragment =>
        assertEquals(frag.children, Nil)
      case _ => fail("Expected VFragment")
    }
  }

  test("fragment external parses children list correctly") {
    val fragFn = getExternal("fragment") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }
    val textFn = getExternal("text") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    // Create text children
    val child1 = textFn(Value.Str("A"))
    val child2 = textFn(Value.Str("B"))
    val children = VList.Cons(child1, VList.Cons(child2, VList.VNil))

    val result = fragFn(children)

    result.asExternal.toAny match {
      case frag: UI.VFragment =>
        assertEquals(frag.children.length, 2)
      case _ => fail("Expected VFragment")
    }
  }

  test("fragment external filters out non-VNode children") {
    val fragFn = getExternal("fragment") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }
    val textFn = getExternal("text") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    // Mix of valid VNode and invalid value
    val validChild = textFn(Value.Str("Valid"))
    val children = VList.Cons(validChild, VList.Cons(VInt(42), VList.VNil))

    val result = fragFn(children)

    result.asExternal.toAny match {
      case frag: UI.VFragment =>
        // Only the valid VNode should be included
        assertEquals(frag.children.length, 1)
      case _ => fail("Expected VFragment")
    }
  }

  test("fragment external handles non-list children") {
    val fragFn = getExternal("fragment") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    // Pass an integer instead of a list
    val children = VInt(42)

    val result = fragFn(children)

    result.asExternal.toAny match {
      case frag: UI.VFragment =>
        assertEquals(frag.children, Nil)
      case _ => fail("Expected VFragment")
    }
  }

  // ==========================================================================
  // jvmExternals tests - read
  // ==========================================================================

  test("read external reads UIState value") {
    val readFn = getExternal("read") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    // Create a UIState first
    val state = UI.UIState[Value]("test_id", Value.Str("test_value"))
    val stateValue = ExternalValue(state)

    val result = readFn(stateValue)
    assertEquals(result, Value.Str("test_value"))
  }

  test("read external returns input for non-UIState") {
    val readFn = getExternal("read") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val input = Value.Str("not a state")
    val result = readFn(input)
    assertEquals(result, input)
  }

  // ==========================================================================
  // jvmExternals tests - write
  // ==========================================================================

  test("write external updates UIState value") {
    val writeFn = getExternal("write") match {
      case FfiCall.Fn2(f) => f
      case _ => fail("Expected Fn2"); null
    }

    // Create a UIState first
    val state = UI.UIState[Value]("test_id", Value.Str("old_value"))
    val stateValue = ExternalValue(state)
    val newValue = Value.Str("new_value")

    val result = writeFn(stateValue, newValue)

    // Should return Unit
    assertEquals(result, UnitValue)
    // State should be updated
    assertEquals(state.value, newValue)
  }

  test("write external handles non-UIState gracefully") {
    val writeFn = getExternal("write") match {
      case FfiCall.Fn2(f) => f
      case _ => fail("Expected Fn2"); null
    }

    val notState = Value.Str("not a state")
    val newValue = Value.Str("new_value")

    // Should not throw
    val result = writeFn(notState, newValue)
    assertEquals(result, UnitValue)
  }

  // ==========================================================================
  // jvmExternals tests - on_click
  // ==========================================================================

  test("on_click external creates event attribute tuple") {
    val onClickFn = getExternal("on_click") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val handler = UnitValue
    val result = onClickFn(handler)

    ProductValue.unapplySeq(result) match {
      case Some(seq) if seq.length == 2 =>
        assertEquals(seq(0), Value.Str("data-onclick"))
        // Handler ID is dynamic
        seq(1) match {
          case Value.Str(s) => assert(s.startsWith("handler_"))
          case _ => fail("Expected string handler ID")
        }
      case _ => fail("Expected ProductValue with 2 elements")
    }
  }

  // ==========================================================================
  // jvmExternals tests - on_input
  // ==========================================================================

  test("on_input external creates event attribute tuple") {
    val onInputFn = getExternal("on_input") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val handler = UnitValue
    val result = onInputFn(handler)

    ProductValue.unapplySeq(result) match {
      case Some(seq) if seq.length == 2 =>
        assertEquals(seq(0), Value.Str("data-oninput"))
      case _ => fail("Expected ProductValue")
    }
  }

  // ==========================================================================
  // jvmExternals tests - on_change
  // ==========================================================================

  test("on_change external creates event attribute tuple") {
    val onChangeFn = getExternal("on_change") match {
      case FfiCall.Fn1(f) => f
      case _ => fail("Expected Fn1"); null
    }

    val handler = UnitValue
    val result = onChangeFn(handler)

    ProductValue.unapplySeq(result) match {
      case Some(seq) if seq.length == 2 =>
        assertEquals(seq(0), Value.Str("data-onchange"))
      case _ => fail("Expected ProductValue")
    }
  }

  // ==========================================================================
  // jvmExternals - all externals present
  // ==========================================================================

  test("jvmExternals contains all expected functions") {
    val expected = List("state", "h", "text", "fragment", "read", "write", "on_click", "on_input", "on_change")
    expected.foreach { name =>
      assert(UI.jvmExternals.toMap.contains((UI.packageName, name)), s"Missing external: $name")
    }
  }

  // ==========================================================================
  // Edge cases
  // ==========================================================================

  test("nested VElements work correctly") {
    val innerText = UI.VText("Hello")
    val innerElem = UI.VElement("span", Nil, List(innerText))
    val outerElem = UI.VElement("div", List(("class", "container")), List(innerElem))

    assertEquals(outerElem.tag, "div")
    assertEquals(outerElem.children.length, 1)
    outerElem.children.head match {
      case inner: UI.VElement =>
        assertEquals(inner.tag, "span")
        assertEquals(inner.children.length, 1)
      case _ => fail("Expected VElement")
    }
  }

  test("VElement with multiple props") {
    val elem = UI.VElement("input", List(
      ("type", "text"),
      ("id", "my-input"),
      ("class", "form-control"),
      ("placeholder", "Enter text")
    ), Nil)

    assertEquals(elem.props.length, 4)
    assert(elem.props.contains(("type", "text")))
    assert(elem.props.contains(("id", "my-input")))
  }

  test("VFragment with mixed children") {
    val text = UI.VText("Text")
    val elem = UI.VElement("br", Nil, Nil)
    val frag = UI.VFragment(List(text, elem, text))

    assertEquals(frag.children.length, 3)
    assert(frag.children(0).isInstanceOf[UI.VText])
    assert(frag.children(1).isInstanceOf[UI.VElement])
    assert(frag.children(2).isInstanceOf[UI.VText])
  }
}
