package dev.bosatsu.ui

import cats.data.NonEmptyList

/**
 * Simulation Applet infrastructure for BosatsuUI.
 *
 * This leverages Bosatsu's type system advantages over TypeScript:
 * - Type-safe provenance: Dependencies tracked at compile time
 * - Exhaustive steps: Sealed traits ensure all simulation phases handled
 * - Total functions: All computations guaranteed to terminate
 * - Matchless IR: Can analyze code structure for "Why?" explanations
 *
 * Unlike React/BurritoScript where dependencies are runtime strings,
 * here we use typed derivation chains that the compiler verifies.
 */
object SimulationApplet {

  /**
   * A typed derivation step showing how a value was computed.
   *
   * @tparam A The type of the value
   */
  sealed trait Derivation[+A] {
    def value: A
    def explain: String
    def dependencies: List[Derivation[?]]
  }

  /**
   * A base assumption - a value provided as input.
   */
  case class Assumption[A](
      name: String,
      value: A,
      description: String = ""
  ) extends Derivation[A] {
    def explain: String = s"$name = $value (assumption)"
    def dependencies: List[Derivation[?]] = Nil
  }

  /**
   * A computed value derived from other derivations.
   *
   * The computation function and its dependencies are tracked,
   * enabling "Why?" explanations and "What if?" toggles.
   */
  case class Computed[A](
      name: String,
      value: A,
      formula: String,
      dependencies: List[Derivation[?]]
  ) extends Derivation[A] {
    def explain: String = s"$name = $formula → $value"
  }

  /**
   * A conditional derivation that depends on a boolean condition.
   */
  case class Conditional[A](
      name: String,
      value: A,
      condition: Derivation[Boolean],
      conditionName: String,
      trueBranch: String,
      falseBranch: String,
      dependencies: List[Derivation[?]]
  ) extends Derivation[A] {
    def explain: String = {
      val branch = if (condition.value) trueBranch else falseBranch
      s"$name = $branch (because $conditionName=${condition.value})"
    }
  }

  /**
   * Build a derivation chain for a value, tracking all dependencies.
   */
  def derive[A](name: String, formula: String, compute: => A)(deps: Derivation[?]*): Derivation[A] =
    Computed(name, compute, formula, deps.toList)

  /**
   * Create an assumption (base value).
   */
  def assume[A](name: String, value: A, description: String = ""): Derivation[A] =
    Assumption(name, value, description)

  /**
   * Get the full derivation chain as a human-readable explanation.
   *
   * This is what the "Why?" button displays.
   */
  def explainDerivation(d: Derivation[?], depth: Int = 0): String = {
    val indent = "  " * depth
    val self = s"$indent${d.explain}"
    val deps = d.dependencies.map(explainDerivation(_, depth + 1))
    (self :: deps).mkString("\n")
  }

  /**
   * Get all assumptions in a derivation chain.
   *
   * These are the values that can be toggled with "What if?" UI.
   */
  def getAssumptions(d: Derivation[?]): List[Assumption[?]] = {
    d match {
      case a: Assumption[?] => List(a)
      case c: Computed[?] => c.dependencies.flatMap(getAssumptions)
      case c: Conditional[?] => getAssumptions(c.condition) ++ c.dependencies.flatMap(getAssumptions)
    }
  }

  /**
   * Simulation state with typed derivations.
   *
   * Unlike untyped state stores, this tracks the derivation
   * chain for each value, enabling provenance queries.
   */
  trait SimulationState {
    def get[A](name: String): Option[Derivation[A]]
    def set[A](d: Derivation[A]): Unit
    def getAssumption[A](name: String): Option[Assumption[A]]
    def updateAssumption[A](name: String, newValue: A): Unit
    def derivations: Map[String, Derivation[?]]
  }

  /**
   * Mutable simulation state implementation.
   */
  final class MutableSimulationState extends SimulationState {
    private val state = scala.collection.mutable.Map[String, Derivation[?]]()
    private val assumptions = scala.collection.mutable.Map[String, Assumption[?]]()
    private var listeners = List[() => Unit]()

    def get[A](name: String): Option[Derivation[A]] =
      state.get(name).map(_.asInstanceOf[Derivation[A]])

    def set[A](d: Derivation[A]): Unit = {
      d match {
        case a: Assumption[?] => assumptions(a.name) = a
        case _ => ()
      }
      state(d match {
        case a: Assumption[?] => a.name
        case c: Computed[?] => c.name
        case c: Conditional[?] => c.name
      }) = d
      notifyListeners()
    }

    def getAssumption[A](name: String): Option[Assumption[A]] =
      assumptions.get(name).map(_.asInstanceOf[Assumption[A]])

    def updateAssumption[A](name: String, newValue: A): Unit = {
      assumptions.get(name).foreach { a =>
        val updated = a.asInstanceOf[Assumption[A]].copy(value = newValue)
        assumptions(name) = updated
        state(name) = updated
        notifyListeners()
      }
    }

    def derivations: Map[String, Derivation[?]] = state.toMap

    def subscribe(listener: () => Unit): Unit = {
      listeners = listener :: listeners
    }

    private def notifyListeners(): Unit = listeners.foreach(_())
  }

  /**
   * Create a new simulation state.
   */
  def simulationState(): MutableSimulationState = new MutableSimulationState()
}
