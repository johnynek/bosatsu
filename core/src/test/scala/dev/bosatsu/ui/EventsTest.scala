package dev.bosatsu.ui

import munit.FunSuite

class EventsTest extends FunSuite {

  // ==========================================================================
  // EventType tests
  // ==========================================================================

  test("EventType.fromString parses all event types") {
    import Events.EventType._
    assertEquals(fromString("click"), Some(Click))
    assertEquals(fromString("input"), Some(Input))
    assertEquals(fromString("change"), Some(Change))
    assertEquals(fromString("submit"), Some(Submit))
    assertEquals(fromString("focus"), Some(Focus))
    assertEquals(fromString("blur"), Some(Blur))
    assertEquals(fromString("keydown"), Some(KeyDown))
    assertEquals(fromString("keyup"), Some(KeyUp))
    assertEquals(fromString("mouseenter"), Some(MouseEnter))
    assertEquals(fromString("mouseleave"), Some(MouseLeave))
  }

  test("EventType.fromString is case-insensitive") {
    import Events.EventType._
    assertEquals(fromString("CLICK"), Some(Click))
    assertEquals(fromString("Click"), Some(Click))
    assertEquals(fromString("INPUT"), Some(Input))
  }

  test("EventType.fromString returns None for unknown events") {
    assertEquals(Events.EventType.fromString("unknown"), None)
    assertEquals(Events.EventType.fromString("custom"), None)
    assertEquals(Events.EventType.fromString(""), None)
  }

  test("EventType.all contains all event types") {
    import Events.EventType._
    assertEquals(all.length, 10)
    assert(all.contains(Click))
    assert(all.contains(Input))
    assert(all.contains(Change))
    assert(all.contains(Submit))
    assert(all.contains(Focus))
    assert(all.contains(Blur))
    assert(all.contains(KeyDown))
    assert(all.contains(KeyUp))
    assert(all.contains(MouseEnter))
    assert(all.contains(MouseLeave))
  }

  test("EventType.name returns correct event names") {
    import Events.EventType._
    assertEquals(Click.name, "click")
    assertEquals(Input.name, "input")
    assertEquals(Change.name, "change")
    assertEquals(Submit.name, "submit")
    assertEquals(Focus.name, "focus")
    assertEquals(Blur.name, "blur")
    assertEquals(KeyDown.name, "keydown")
    assertEquals(KeyUp.name, "keyup")
    assertEquals(MouseEnter.name, "mouseenter")
    assertEquals(MouseLeave.name, "mouseleave")
  }

  // ==========================================================================
  // Action tests
  // ==========================================================================

  test("Action.NoOp is a singleton") {
    val a = Events.Action.NoOp
    val b = Events.Action.NoOp
    assertEquals(a, b)
  }

  test("Action.Batch contains list of actions") {
    val batch = Events.Action.Batch[Nothing](List(Events.Action.NoOp))
    assertEquals(batch.actions.length, 1)
  }

  // ==========================================================================
  // HandlerConfig tests
  // ==========================================================================

  test("HandlerConfig has correct defaults") {
    val config = Events.HandlerConfig(
      eventType = Events.EventType.Click,
      action = Events.Action.NoOp
    )
    assertEquals(config.preventDefault, false)
    assertEquals(config.stopPropagation, false)
  }

  test("HandlerConfig can have modifiers enabled") {
    val config = Events.HandlerConfig(
      eventType = Events.EventType.Submit,
      action = Events.Action.NoOp,
      preventDefault = true,
      stopPropagation = true
    )
    assert(config.preventDefault)
    assert(config.stopPropagation)
  }

  // ==========================================================================
  // generateHandlerJs tests
  // ==========================================================================

  test("generateHandlerJs generates basic handler") {
    val config = Events.HandlerConfig(
      eventType = Events.EventType.Click,
      action = Events.Action.NoOp
    )
    val js = Events.generateHandlerJs(config, "doSomething()")
    assert(js.contains("function(e)"))
    assert(js.contains("doSomething()"))
    // Should not contain modifiers
    assert(!js.contains("preventDefault"))
    assert(!js.contains("stopPropagation"))
  }

  test("generateHandlerJs adds preventDefault when configured") {
    val config = Events.HandlerConfig(
      eventType = Events.EventType.Submit,
      action = Events.Action.NoOp,
      preventDefault = true
    )
    val js = Events.generateHandlerJs(config, "handleSubmit()")
    assert(js.contains("e.preventDefault()"))
    assert(!js.contains("stopPropagation"))
  }

  test("generateHandlerJs adds stopPropagation when configured") {
    val config = Events.HandlerConfig(
      eventType = Events.EventType.Click,
      action = Events.Action.NoOp,
      stopPropagation = true
    )
    val js = Events.generateHandlerJs(config, "handleClick()")
    assert(js.contains("e.stopPropagation()"))
    assert(!js.contains("preventDefault"))
  }

  test("generateHandlerJs adds both modifiers when configured") {
    val config = Events.HandlerConfig(
      eventType = Events.EventType.Submit,
      action = Events.Action.NoOp,
      preventDefault = true,
      stopPropagation = true
    )
    val js = Events.generateHandlerJs(config, "handleForm()")
    assert(js.contains("e.preventDefault()"))
    assert(js.contains("e.stopPropagation()"))
  }

  // ==========================================================================
  // generateEventSetup tests
  // ==========================================================================

  test("generateEventSetup generates listener setup code") {
    val handlers = Map(
      "btn1" -> List(("click", "handleClick"))
    )
    val js = Events.generateEventSetup(handlers)
    assert(js.contains("addEventListener"))
    assert(js.contains("click"))
    assert(js.contains("handleClick"))
    assert(js.contains("data-bosatsu-id=\"btn1\""))
  }

  test("generateEventSetup handles multiple handlers per element") {
    val handlers = Map(
      "input1" -> List(
        ("input", "handleInput"),
        ("change", "handleChange")
      )
    )
    val js = Events.generateEventSetup(handlers)
    assert(js.contains("input"))
    assert(js.contains("change"))
    assert(js.contains("handleInput"))
    assert(js.contains("handleChange"))
  }

  test("generateEventSetup handles ID selectors") {
    val handlers = Map(
      "#myButton" -> List(("click", "handleClick"))
    )
    val js = Events.generateEventSetup(handlers)
    // Should use selector as-is for ID selectors
    assert(js.contains("#myButton"))
    assert(!js.contains("data-bosatsu-id"))
  }

  test("generateEventSetup handles attribute selectors") {
    val handlers = Map(
      "[name='email']" -> List(("input", "handleEmail"))
    )
    val js = Events.generateEventSetup(handlers)
    // Should use selector as-is for attribute selectors
    assert(js.contains("[name='email']"))
    assert(!js.contains("data-bosatsu-id"))
  }

  test("generateEventSetup includes helper function") {
    val handlers = Map("btn" -> List(("click", "fn")))
    val js = Events.generateEventSetup(handlers)
    assert(js.contains("_findElement"))
    assert(js.contains("document.querySelector"))
  }

  // ==========================================================================
  // extractEventHandlers tests
  // ==========================================================================

  test("extractEventHandlers returns empty for now") {
    // The function is currently a stub that returns Nil
    val expr = dev.bosatsu.TypedExpr.Literal(
      dev.bosatsu.Lit.Integer(42),
      dev.bosatsu.rankn.Type.IntType,
      ()
    )
    val handlers = Events.extractEventHandlers(expr)
    assertEquals(handlers, Nil)
  }
}
