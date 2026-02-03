package dev.bosatsu.tla

import dev.bosatsu.service.{ServiceOperation, ServiceAnalysis, OperationKind}
import dev.bosatsu.tla.TlaValue._

/**
 * Generates TLA+ specifications from Bosatsu service handlers.
 *
 * This enables formal verification of concurrent handler executions
 * by generating TLA+ that models:
 * - State transitions from handler operations
 * - Concurrent execution interleavings
 * - Invariant checking
 */
object TlaGen {

  /**
   * Generate a TLA+ specification from a service analysis.
   */
  def generate(
    analysis: ServiceAnalysis,
    options: TlaOptions,
    instances: Int = 1
  ): TlaSpec = {
    val moduleName = sanitizeModuleName(analysis.handlerName)

    // Generate variables
    val variables = List(options.stateVariable, "pc") ++
      (if (instances > 1) List("instance") else Nil)

    // Generate initial state
    val init = generateInit(analysis, options, instances)

    // Generate actions from operations
    val actions = generateActions(analysis, options, instances)

    // Generate Next formula
    val next = generateNext(actions)

    // Generate Spec formula
    val spec = s"Spec == Init /\\ [][Next]_vars"

    // Generate invariants
    val invariants = options.invariant.toList

    TlaSpec(
      moduleName = moduleName,
      extends_ = List("Integers", "Sequences", "TLC"),
      variables = variables,
      init = init,
      actions = actions,
      next = next,
      spec = spec,
      invariants = invariants
    )
  }

  /**
   * Generate TLA+ for race condition analysis.
   *
   * Creates a specification with multiple concurrent instances
   * to find race conditions via model checking.
   */
  def generateRaceSpec(
    analysis: ServiceAnalysis,
    instances: Int,
    invariant: String
  ): TlaSpec = {
    val options = TlaOptions(
      invariant = Some(invariant),
      checkDeadlock = true
    )
    generate(analysis, options, instances)
  }

  /**
   * Generate Init predicate.
   */
  private def generateInit(
    analysis: ServiceAnalysis,
    options: TlaOptions,
    instances: Int
  ): String = {
    val stateVar = options.stateVariable
    val initialValues = options.initialState.map { case (k, v) =>
      s"$k |-> ${v.render}"
    }.mkString(", ")

    val stateInit = if (initialValues.nonEmpty) {
      s"$stateVar = [$initialValues]"
    } else {
      s"$stateVar = [x \\in {} |-> 0]"
    }

    val pcInit = if (instances > 1) {
      s"pc = [i \\in 1..$instances |-> \"start\"]"
    } else {
      "pc = \"start\""
    }

    val instanceInit = if (instances > 1) {
      s"instance = 1..$instances"
    } else {
      ""
    }

    val inits = List(stateInit, pcInit) ++
      (if (instanceInit.nonEmpty) List(instanceInit) else Nil)

    s"""Init ==
    ${inits.map(i => s"/\\ $i").mkString("\n    ")}"""
  }

  /**
   * Generate actions from service operations.
   */
  private def generateActions(
    analysis: ServiceAnalysis,
    options: TlaOptions,
    instances: Int
  ): List[TlaAction] = {
    val stateVar = options.stateVariable

    // Group operations by kind for state modeling
    val readOps = analysis.operations.filter(_.kind == OperationKind.Read)
    val writeOps = analysis.operations.filter(_.kind == OperationKind.Write)

    // Generate read actions
    val readActions = readOps.zipWithIndex.map { case (op, i) =>
      val actionName = s"Read_${sanitizeName(op.method)}_$i"
      TlaAction(
        name = actionName,
        guard = "TRUE",
        effect = s"UNCHANGED $stateVar",
        pcFrom = if (i == 0) "start" else s"read_${i-1}",
        pcTo = s"read_$i"
      )
    }

    // Generate write actions
    val writeActions = writeOps.zipWithIndex.map { case (op, i) =>
      val actionName = s"Write_${sanitizeName(op.method)}_$i"
      val readOffset = readOps.size
      TlaAction(
        name = actionName,
        guard = "TRUE",
        effect = s"$stateVar' = $stateVar",  // Placeholder - real impl would model the write
        pcFrom = if (readOps.isEmpty && i == 0) "start"
                 else if (i == 0) s"read_${readOps.size - 1}"
                 else s"write_${i-1}",
        pcTo = if (i == writeOps.size - 1) "done" else s"write_$i"
      )
    }

    // If no operations, add a simple complete action
    if (readActions.isEmpty && writeActions.isEmpty) {
      List(TlaAction(
        name = "Complete",
        guard = "TRUE",
        effect = s"UNCHANGED $stateVar",
        pcFrom = "start",
        pcTo = "done"
      ))
    } else {
      readActions ++ writeActions
    }
  }

  /**
   * Generate Next formula.
   */
  private def generateNext(actions: List[TlaAction]): String = {
    val actionDisjuncts = (actions.map(_.name) :+ "Done").mkString(" \\/ ")
    s"Next == $actionDisjuncts"
  }

  /**
   * Sanitize a name for use as a TLA+ identifier.
   */
  private def sanitizeName(name: String): String = {
    name.replaceAll("[^a-zA-Z0-9_]", "_")
  }

  /**
   * Sanitize a module name for TLA+.
   */
  private def sanitizeModuleName(name: String): String = {
    val sanitized = sanitizeName(name)
    // Module names must start with a letter
    if (sanitized.headOption.exists(_.isLetter)) sanitized
    else s"M_$sanitized"
  }

  /**
   * Generate a TLA+ configuration file for TLC.
   */
  def generateConfig(
    spec: TlaSpec,
    options: TlcOptions
  ): String = {
    val invariantConfig = spec.invariants.zipWithIndex.map { case (_, i) =>
      s"INVARIANT Inv$i"
    }.mkString("\n")

    val deadlockConfig = if (options.checkDeadlock) "" else "CHECK_DEADLOCK FALSE"

    s"""SPECIFICATION Spec
$invariantConfig
$deadlockConfig
"""
  }
}
