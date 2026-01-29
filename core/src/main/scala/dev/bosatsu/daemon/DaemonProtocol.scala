package dev.bosatsu.daemon

import cats.data.NonEmptyList
import cats.{Eq, Show}
import cats.implicits._
import dev.bosatsu.{Identifier, PackageName, Region}

/**
 * Protocol types for the Bosatsu debugger daemon.
 *
 * The daemon holds a provenance trace in memory and handles exploration
 * commands. This enables stateful debugging across multiple CLI invocations.
 *
 * Architecture:
 * - Daemon listens on a Unix socket
 * - CLI client sends JSON commands, receives JSON responses
 * - State (focus node, trace) persists across commands
 */

/**
 * Node identifier in the provenance trace.
 */
case class NodeId(value: String) extends AnyVal derives CanEqual {
  override def toString: String = value
}

object NodeId {
  implicit val nodeIdEq: Eq[NodeId] = Eq.fromUniversalEquals
  implicit val nodeIdShow: Show[NodeId] = Show.show(_.value)
}

/**
 * Source location for a node.
 */
case class SourceLocation(
    file: String,
    line: Int,
    column: Int
) derives CanEqual

object SourceLocation {
  implicit val sourceLocationEq: Eq[SourceLocation] = Eq.fromUniversalEquals
  implicit val sourceLocationShow: Show[SourceLocation] =
    Show.show(loc => s"${loc.file}:${loc.line}:${loc.column}")
}

/**
 * Source type describing how a value was computed.
 */
sealed trait SourceType derives CanEqual {
  def typeName: String
}

object SourceType {
  case class Pure(expression: Option[String]) extends SourceType {
    def typeName: String = "pure"
  }

  case class Operation(interface: String, method: String) extends SourceType {
    def typeName: String = "operation"
  }

  case class Binding(name: String) extends SourceType {
    def typeName: String = "binding"
  }

  case object Parameter extends SourceType {
    def typeName: String = "parameter"
  }

  case object Literal extends SourceType {
    def typeName: String = "literal"
  }

  implicit val sourceTypeEq: Eq[SourceType] = Eq.fromUniversalEquals
  implicit val sourceTypeShow: Show[SourceType] = Show.show(_.typeName)
}

/**
 * A node in the provenance trace.
 */
case class TraceNode(
    id: NodeId,
    value: String, // JSON-encoded value
    source: SourceType,
    bindingName: Option[String],
    location: Option[SourceLocation],
    dependencies: List[NodeId],
    usedBy: List[NodeId]
)

object TraceNode {
  implicit val traceNodeEq: Eq[TraceNode] = Eq.fromUniversalEquals
}

/**
 * A provenance trace containing all nodes from an execution.
 */
case class ProvenanceTrace(
    nodes: Map[NodeId, TraceNode],
    resultNodeId: NodeId,
    sourceFile: String
) {
  def getNode(id: NodeId): Option[TraceNode] = nodes.get(id)

  def getDependencies(id: NodeId): List[TraceNode] =
    getNode(id).map(_.dependencies.flatMap(nodes.get)).getOrElse(Nil)

  def getUsages(id: NodeId): List[TraceNode] =
    getNode(id).map(_.usedBy.flatMap(nodes.get)).getOrElse(Nil)

  def findByValue(value: String): List[TraceNode] =
    nodes.values.filter(_.value == value).toList

  def nodeCount: Int = nodes.size
}

/**
 * Command types supported by the daemon.
 */
sealed trait DaemonCommand derives CanEqual

object DaemonCommand {
  /** List all nodes in the trace */
  case class List(showValues: Boolean = false) extends DaemonCommand

  /** Explain a node's derivation tree */
  case class Explain(nodeId: Option[NodeId]) extends DaemonCommand

  /** Find nodes by value */
  case class Find(value: String) extends DaemonCommand

  /** Show dependencies of a node */
  case class Deps(nodeId: NodeId) extends DaemonCommand

  /** Show what uses a node */
  case class Usages(nodeId: NodeId) extends DaemonCommand

  /** Set focus to a node */
  case class Focus(nodeId: NodeId) extends DaemonCommand

  /** Clear focus */
  case object Unfocus extends DaemonCommand

  /** Show path from root to node */
  case class Path(nodeId: NodeId) extends DaemonCommand

  /** Show full value of a node */
  case class Value(nodeId: NodeId) extends DaemonCommand

  /** Show source location of a node */
  case class Source(nodeId: NodeId) extends DaemonCommand

  /** Show code snippet with context */
  case class Snippet(nodeId: NodeId, contextLines: Int = 3) extends DaemonCommand

  /** Evaluate an expression with node's scope */
  case class Eval(nodeId: NodeId, expression: String) extends DaemonCommand

  /** Get daemon status */
  case object Status extends DaemonCommand

  /** Shutdown the daemon */
  case object Shutdown extends DaemonCommand

  implicit val daemonCommandEq: Eq[DaemonCommand] = Eq.fromUniversalEquals
}

/**
 * Response from the daemon.
 */
sealed trait DaemonResponse {
  def isSuccess: Boolean
}

object DaemonResponse {
  case class Success(data: ResponseData) extends DaemonResponse {
    def isSuccess: Boolean = true
  }

  case class Error(message: String) extends DaemonResponse {
    def isSuccess: Boolean = false
  }

  implicit val daemonResponseEq: Eq[DaemonResponse] = Eq.fromUniversalEquals
}

/**
 * Response data types for different commands.
 */
sealed trait ResponseData

object ResponseData {
  case class NodeList(nodes: List[NodeSummary]) extends ResponseData

  case class NodeSummary(
      id: NodeId,
      value: Option[String], // Only present if showValues=true
      typeInfo: String,
      source: String,
      bindingName: Option[String],
      location: Option[SourceLocation]
  )

  case class ExplainResult(
      nodeId: NodeId,
      tree: String, // Formatted tree string
      compact: String,
      node: NodeSummary
  ) extends ResponseData

  case class FindResult(nodes: List[NodeSummary]) extends ResponseData

  case class DepsResult(
      nodeId: NodeId,
      dependencies: List[NodeSummary]
  ) extends ResponseData

  case class UsagesResult(
      nodeId: NodeId,
      usages: List[NodeSummary]
  ) extends ResponseData

  case class FocusResult(
      message: String,
      node: NodeSummary
  ) extends ResponseData

  case class UnfocusResult(message: String) extends ResponseData

  case class PathResult(path: List[NodeSummary]) extends ResponseData

  case class ValueResult(
      nodeId: NodeId,
      value: String,
      valueJson: String
  ) extends ResponseData

  case class SourceResult(
      nodeId: NodeId,
      location: Option[SourceLocation]
  ) extends ResponseData

  case class SnippetLine(
      lineNum: Int,
      code: String,
      highlight: Boolean
  )

  case class ScopeEntry(
      name: String,
      value: String,
      nodeId: Option[NodeId]
  )

  case class SnippetResult(
      nodeId: NodeId,
      location: SourceLocation,
      lines: List[SnippetLine],
      scope: List[ScopeEntry],
      result: ScopeEntry
  ) extends ResponseData

  case class EvalResult(
      expression: String,
      result: String,
      scope: Map[String, String]
  ) extends ResponseData

  case class StatusResult(
      running: Boolean,
      traceFile: String,
      nodeCount: Int,
      focusNodeId: Option[NodeId],
      resultNodeId: NodeId,
      resultValue: String
  ) extends ResponseData

  case class ShutdownResult(message: String) extends ResponseData

  implicit val responseDataEq: Eq[ResponseData] = Eq.fromUniversalEquals
}

/**
 * Daemon state held in memory.
 */
case class DaemonState(
    trace: ProvenanceTrace,
    focusNodeId: Option[NodeId],
    traceFile: String
) derives CanEqual {
  def withFocus(nodeId: NodeId): DaemonState =
    copy(focusNodeId = Some(nodeId))

  def clearFocus: DaemonState =
    copy(focusNodeId = None)

  def effectiveNodeId(explicit: Option[NodeId]): NodeId =
    explicit.orElse(focusNodeId).getOrElse(trace.resultNodeId)
}

object DaemonState {
  def initial(trace: ProvenanceTrace, traceFile: String): DaemonState =
    DaemonState(trace, None, traceFile)
}
