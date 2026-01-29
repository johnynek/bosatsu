package dev.bosatsu.ui

import scala.collection.mutable

/**
 * Reactive state management for BosatsuUI.
 *
 * Provides a simple reactive state system where:
 * - State is stored as typed values
 * - Subscribers are notified when state changes
 * - State changes can be batched for efficiency
 */

/**
 * A typed state value that can be observed for changes.
 *
 * @tparam A The type of the state value
 */
sealed trait StateValue[A] {
  def get: A
  def set(value: A): Unit
  def update(f: A => A): Unit
  def subscribe(listener: A => Unit): Subscription
}

/**
 * A subscription that can be cancelled to stop receiving updates.
 */
trait Subscription {
  def cancel(): Unit
  def isActive: Boolean
}

/**
 * Implementation of a mutable state value with change notification.
 */
final class MutableState[A](initial: A) extends StateValue[A] {
  private var currentValue: A = initial
  private val listeners = mutable.Set[A => Unit]()

  def get: A = currentValue

  def set(value: A): Unit = {
    if (!value.equals(currentValue)) {
      currentValue = value
      notifyListeners()
    }
  }

  def update(f: A => A): Unit = {
    set(f(currentValue))
  }

  def subscribe(listener: A => Unit): Subscription = {
    listeners += listener
    // Immediately notify with current value
    listener(currentValue)
    new Subscription {
      private var active = true
      def cancel(): Unit = {
        if (active) {
          listeners -= listener
          active = false
        }
      }
      def isActive: Boolean = active
    }
  }

  private def notifyListeners(): Unit = {
    val value = currentValue
    listeners.foreach(_(value))
  }
}

/**
 * A computed value derived from other state values.
 *
 * Automatically updates when dependencies change.
 * Call dispose() when the computed state is no longer needed to prevent memory leaks.
 */
final class ComputedState[A](compute: () => A, deps: Seq[StateValue[_]]) extends StateValue[A] {
  private var cachedValue: A = compute()
  private val listeners = mutable.Set[A => Unit]()
  private var disposed = false
  private val depSubscriptions: Seq[Subscription] = deps.map { dep =>
    dep.asInstanceOf[StateValue[Any]].subscribe(_ => recompute())
  }

  /**
   * Dispose this computed state, cancelling all dependency subscriptions.
   * After disposal, the state will no longer update when dependencies change.
   */
  def dispose(): Unit = {
    if (!disposed) {
      depSubscriptions.foreach(_.cancel())
      listeners.clear()
      disposed = true
    }
  }

  def isDisposed: Boolean = disposed

  def get: A = cachedValue

  def set(value: A): Unit = {
    throw new UnsupportedOperationException("Cannot set a computed value directly")
  }

  def update(f: A => A): Unit = {
    throw new UnsupportedOperationException("Cannot update a computed value directly")
  }

  def subscribe(listener: A => Unit): Subscription = {
    listeners += listener
    listener(cachedValue)
    new Subscription {
      private var active = true
      def cancel(): Unit = {
        if (active) {
          listeners -= listener
          active = false
        }
      }
      def isActive: Boolean = active
    }
  }

  private def recompute(): Unit = {
    val newValue = compute()
    if (!newValue.equals(cachedValue)) {
      cachedValue = newValue
      listeners.foreach(_(cachedValue))
    }
  }
}

/**
 * State store that holds multiple named state values.
 */
final class StateStore {
  private val states = mutable.Map[String, StateValue[_]]()

  def register[A](name: String, initial: A): StateValue[A] = {
    val state = new MutableState[A](initial)
    states(name) = state
    state
  }

  def get[A](name: String): Option[StateValue[A]] = {
    states.get(name).map(_.asInstanceOf[StateValue[A]])
  }

  def computed[A](name: String, compute: () => A, deps: Seq[String]): StateValue[A] = {
    val depStates = deps.map { depName =>
      get[Any](depName).getOrElse(
        throw new IllegalArgumentException(s"Missing dependency '$depName' for computed state '$name'")
      )
    }
    val state = new ComputedState[A](compute, depStates)
    states(name) = state
    state
  }

  def names: Set[String] = states.keySet.toSet
}

object ReactiveState {
  /**
   * Create a new mutable state value.
   */
  def state[A](initial: A): StateValue[A] = new MutableState[A](initial)

  /**
   * Create a computed state value.
   */
  def computed[A](compute: () => A, deps: StateValue[_]*): StateValue[A] =
    new ComputedState[A](compute, deps)

  /**
   * Create a new state store.
   */
  def store(): StateStore = new StateStore()
}
