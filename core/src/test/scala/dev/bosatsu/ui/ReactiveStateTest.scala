package dev.bosatsu.ui

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

/**
 * Property-based tests for ReactiveState.
 */
class ReactiveStateTest extends ScalaCheckSuite {
  import ReactiveStateGen._

  // MutableState tests

  property("MutableState.get returns initial value") {
    forAll(genIntState) { initial =>
      val state = new MutableState[Int](initial)
      state.get == initial
    }
  }

  property("MutableState.set updates the value") {
    forAll(genIntState, genIntState) { (initial, newValue) =>
      val state = new MutableState[Int](initial)
      state.set(newValue)
      state.get == newValue
    }
  }

  property("MutableState.update applies function") {
    forAll(genIntState, genIntUpdate) { (initial, f) =>
      val state = new MutableState[Int](initial)
      state.update(f)
      state.get == f(initial)
    }
  }

  property("MutableState subscribers are notified of changes") {
    forAll(genIntState, genIntState) { (initial, newValue) =>
      var notified = false
      var lastValue = initial
      val state = new MutableState[Int](initial)

      state.subscribe { v =>
        notified = true
        lastValue = v
      }

      // Subscriber is called immediately with initial value
      notified && lastValue == initial && {
        notified = false
        state.set(newValue)
        // Only notified if value actually changed
        if (initial == newValue) {
          !notified && lastValue == initial
        } else {
          notified && lastValue == newValue
        }
      }
    }
  }

  property("MutableState does not notify for same value") {
    forAll(genIntState) { value =>
      var notifyCount = 0
      val state = new MutableState[Int](value)

      state.subscribe { _ =>
        notifyCount += 1
      }

      // Initial notification
      notifyCount == 1 && {
        state.set(value) // Same value
        notifyCount == 1 // No additional notification
      }
    }
  }

  property("Subscription can be cancelled") {
    forAll(genIntState, genIntState) { (initial, newValue) =>
      var notifyCount = 0
      val state = new MutableState[Int](initial)

      val sub = state.subscribe { _ =>
        notifyCount += 1
      }

      // Initial notification
      notifyCount == 1 && {
        sub.cancel()
        !sub.isActive && {
          state.set(newValue)
          notifyCount == 1 // No notification after cancel
        }
      }
    }
  }

  // ComputedState tests

  property("ComputedState computes initial value") {
    forAll(genIntState, genIntState) { (a, b) =>
      val stateA = new MutableState[Int](a)
      val stateB = new MutableState[Int](b)
      val computed = new ComputedState[Int](() => stateA.get + stateB.get, Seq(stateA, stateB))

      computed.get == a + b
    }
  }

  property("ComputedState updates when dependencies change") {
    forAll(genIntState, genIntState, genIntState) { (initial, newA, newB) =>
      val stateA = new MutableState[Int](initial)
      val stateB = new MutableState[Int](initial)
      val computed = new ComputedState[Int](() => stateA.get * stateB.get, Seq(stateA, stateB))

      computed.get == initial * initial && {
        stateA.set(newA)
        computed.get == newA * initial && {
          stateB.set(newB)
          computed.get == newA * newB
        }
      }
    }
  }

  property("ComputedState cannot be set directly") {
    forAll(genIntState) { initial =>
      val state = new MutableState[Int](initial)
      val computed = new ComputedState[Int](() => state.get * 2, Seq(state))

      try {
        computed.set(100)
        false
      } catch {
        case _: UnsupportedOperationException => true
      }
    }
  }

  // StateStore tests

  test("StateStore register and get") {
    val store = ReactiveState.store()
    val state = store.register("count", 0)

    assertEquals(state.get, 0)
    assertEquals(store.get[Int]("count").map(_.get), Some(0))
    assertEquals(store.names, Set("count"))
  }

  test("StateStore computed values") {
    val store = ReactiveState.store()
    store.register("a", 10)
    store.register("b", 20)

    val computed = store.computed[Int](
      "sum",
      () => store.get[Int]("a").map(_.get).getOrElse(0) + store.get[Int]("b").map(_.get).getOrElse(0),
      Seq("a", "b")
    )

    assertEquals(computed.get, 30)
  }

  // StateBinding tests

  test("StateBinding generates valid store code") {
    val stateVars = Map("count" -> "0", "name" -> "\"test\"")
    val storeCode = StateBinding.generateStateStore(stateVars)

    assert(storeCode.nonEmpty, "Should generate store code")
    assert(storeCode.length >= 4, "Should generate _state, _listeners, _getState, _setState")
  }

  property("StateBinding text binding generates valid code") {
    forAll(genTextBinding) { binding =>
      val code = StateBinding.generateTextBinding(binding)
      code != null
    }
  }

  property("StateBinding input binding generates valid code") {
    forAll(genInputBinding) { binding =>
      val code = StateBinding.generateInputBinding(binding)
      code != null
    }
  }

  test("StateBinding generates complete module") {
    val module = StateBinding.generateModule(
      "TestApp",
      Map("count" -> "0"),
      List(StateBinding.TextBinding("count", "count-display")),
      List(StateBinding.InputBinding("count-input", "count"))
    )

    val rendered = module.render(80)
    assert(rendered.contains("_state"), "Should contain state object")
    assert(rendered.contains("_subscribe"), "Should contain subscribe function")
    assert(rendered.contains("window.TestApp"), "Should export module")
  }
}
