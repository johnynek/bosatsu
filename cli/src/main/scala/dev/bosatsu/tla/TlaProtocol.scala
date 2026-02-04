package dev.bosatsu.tla

import dev.bosatsu.service.ServiceOperation

/**
 * Protocol types for TLA+ generation.
 *
 * TLA+ is a formal specification language for describing concurrent
 * and distributed systems. This module generates TLA+ specifications
 * from Bosatsu service handlers to enable formal verification.
 */

/**
 * Options for TLA+ generation.
 */
case class TlaOptions(
  initialState: Map[String, TlaValue] = Map.empty,
  invariant: Option[String] = None,
  stateVariable: String = "state",
  checkDeadlock: Boolean = true
) derives CanEqual

/**
 * TLA+ value representation.
 */
sealed trait TlaValue derives CanEqual {
  def render: String
}

object TlaValue {
  case class TlaInt(value: Int) extends TlaValue {
    def render: String = value.toString
  }

  case class TlaString(value: String) extends TlaValue {
    def render: String = s"\"$value\""
  }

  case class TlaBool(value: Boolean) extends TlaValue {
    def render: String = if (value) "TRUE" else "FALSE"
  }

  case class TlaSeq(values: List[TlaValue]) extends TlaValue {
    def render: String = s"<<${values.map(_.render).mkString(", ")}>>"
  }

  case class TlaSet(values: Set[TlaValue]) extends TlaValue {
    def render: String = s"{${values.map(_.render).mkString(", ")}}"
  }

  case class TlaRecord(fields: Map[String, TlaValue]) extends TlaValue {
    def render: String = {
      val fieldStrs = fields.map { case (k, v) => s"$k |-> ${v.render}" }
      s"[${fieldStrs.mkString(", ")}]"
    }
  }

  case class TlaFunction(domain: String, mapping: String) extends TlaValue {
    def render: String = s"[$domain |-> $mapping]"
  }

  def fromAny(value: Any): TlaValue = value match {
    case i: Int => TlaInt(i)
    case l: Long => TlaInt(l.toInt)
    case s: String => TlaString(s)
    case b: Boolean => TlaBool(b)
    case seq: Seq[?] => TlaSeq(seq.map(fromAny).toList)
    case set: Set[?] => TlaSet(set.map(fromAny))
    case map: Map[?, ?] =>
      TlaRecord(map.map { case (k, v) => k.toString -> fromAny(v) }.toMap)
    case _ => TlaString(value.toString)
  }
}

/**
 * Represents a TLA+ action (state transition).
 */
case class TlaAction(
  name: String,
  guard: String,
  effect: String,
  pcFrom: String,
  pcTo: String,
  multiInstance: Boolean = false  // For multi-instance specs, pc is a function
) derives CanEqual

/**
 * Represents a complete TLA+ specification.
 */
case class TlaSpec(
  moduleName: String,
  extends_ : List[String],
  variables: List[String],
  init: String,
  actions: List[TlaAction],
  next: String,
  spec: String,
  invariants: List[String] = Nil
) derives CanEqual {
  def render: String = {
    val extendsClause = if (extends_.nonEmpty) s"EXTENDS ${extends_.mkString(", ")}" else ""
    val variablesClause = s"VARIABLES ${variables.mkString(", ")}"
    val varsDefn = s"vars == <<${variables.mkString(", ")}>>"

    val actionDefs = actions.map { a =>
      if (a.multiInstance) {
        // Multi-instance: pc is a function, parameterize action by self
        s"""${a.name}(self) ==
    /\\ pc[self] = "${a.pcFrom}"
    /\\ ${a.guard}
    /\\ ${a.effect}
    /\\ pc' = [pc EXCEPT ![self] = "${a.pcTo}"]"""
      } else {
        s"""${a.name} ==
    /\\ pc = "${a.pcFrom}"
    /\\ ${a.guard}
    /\\ ${a.effect}
    /\\ pc' = "${a.pcTo}""""
      }
    }.mkString("\n\n")

    val invariantDefs = invariants.zipWithIndex.map { case (inv, i) =>
      s"Inv$i == $inv"
    }.mkString("\n")

    s"""---- MODULE $moduleName ----

$extendsClause

$variablesClause

$varsDefn

$init

$actionDefs

Done ==
    /\\ pc = "done"
    /\\ UNCHANGED vars

$next

$spec

$invariantDefs

====
"""
  }
}

/**
 * Result of TLC model checking.
 */
case class TlcResult(
  success: Boolean,
  invariantViolation: Boolean = false,
  deadlock: Boolean = false,
  temporalPropertyViolation: Boolean = false,
  syntaxError: Boolean = false,
  errorMessage: Option[String] = None,
  statesGenerated: Option[Int] = None,
  distinctStates: Option[Int] = None,
  errorTrace: List[TlcTraceState] = Nil,
  skipped: Boolean = false,
  skipReason: Option[String] = None,
  rawOutput: Option[String] = None
) derives CanEqual

/**
 * A state in a TLC error trace.
 */
case class TlcTraceState(
  stateNumber: Int,
  action: String,
  variables: String
) derives CanEqual

/**
 * Options for running TLC.
 */
case class TlcOptions(
  workers: Int = 1,
  checkDeadlock: Boolean = true,
  depth: Option[Int] = None,
  timeout: Option[Int] = None,  // milliseconds
  skipIfUnavailable: Boolean = true
) derives CanEqual

/**
 * TLA+ CLI command types.
 */
sealed trait TlaCommand derives CanEqual

object TlaCommand {
  case class Generate(
    file: String,
    output: Option[String] = None,
    instances: Int = 1,
    invariant: Option[String] = None
  ) extends TlaCommand

  case class Check(
    file: String,
    workers: Int = 1,
    depth: Option[Int] = None,
    timeout: Option[Int] = None
  ) extends TlaCommand

  case class Race(
    file: String,
    instances: Int = 2,
    invariant: Option[String] = None
  ) extends TlaCommand
}

/**
 * Result of TLA+ generation.
 */
case class TlaGenResult(
  file: String,
  tlaSpec: String,
  moduleName: String,
  instances: Int,
  invariant: Option[String] = None,
  tlcResult: Option[TlcResult] = None
) derives CanEqual

/**
 * Result of race condition analysis.
 */
case class RaceAnalysisResult(
  file: String,
  handlerName: String,
  instances: Int,
  totalInterleavings: Int,
  violatingInterleavings: Int,
  invariant: String,
  exampleViolation: Option[RaceViolation] = None
) derives CanEqual

case class RaceViolation(
  trace: List[String],
  finalState: Map[String, TlaValue]
) derives CanEqual
