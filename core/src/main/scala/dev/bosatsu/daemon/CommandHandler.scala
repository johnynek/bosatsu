package dev.bosatsu.daemon

import cats.implicits._

/**
 * Handles daemon commands and returns responses.
 *
 * This is the core logic for command processing, separated from I/O
 * to enable easy testing.
 */
object CommandHandler {

  /**
   * Result of handling a command.
   */
  case class CommandResult(
      response: DaemonResponse,
      newState: DaemonState,
      shouldShutdown: Boolean = false
  )

  /**
   * Handle a command and return the response along with updated state.
   */
  def handle(command: DaemonCommand, state: DaemonState): CommandResult = {
    import DaemonCommand._
    import DaemonResponse._
    import ResponseData._

    command match {
      case List(showValues) =>
        val summaries = state.trace.nodes.values.toList.sortBy(_.id.value).map { node =>
          toSummary(node, showValues)
        }
        CommandResult(Success(NodeList(summaries)), state)

      case Explain(explicitNodeId) =>
        val nodeId = state.effectiveNodeId(explicitNodeId)
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(node) =>
            val tree = buildExplanationTree(state.trace, nodeId, 0)
            val compact = buildCompactExplanation(state.trace, nodeId)
            CommandResult(
              Success(ExplainResult(nodeId, tree, compact, toSummary(node, showValue = true))),
              state
            )
        }

      case Find(value) =>
        val nodes = state.trace.findByValue(value).map(n => toSummary(n, showValue = true))
        CommandResult(Success(FindResult(nodes)), state)

      case Deps(nodeId) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(_) =>
            val deps = state.trace.getDependencies(nodeId).map(n => toSummary(n, showValue = true))
            CommandResult(Success(DepsResult(nodeId, deps)), state)
        }

      case Usages(nodeId) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(_) =>
            val usages = state.trace.getUsages(nodeId).map(n => toSummary(n, showValue = true))
            CommandResult(Success(UsagesResult(nodeId, usages)), state)
        }

      case Focus(nodeId) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(node) =>
            val newState = state.withFocus(nodeId)
            CommandResult(
              Success(FocusResult(s"Focused on ${nodeId.value}", toSummary(node, showValue = true))),
              newState
            )
        }

      case Unfocus =>
        CommandResult(Success(UnfocusResult("Focus cleared")), state.clearFocus)

      case Path(nodeId) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(_) =>
            val pathNodes = findPath(state.trace, nodeId).map(n => toSummary(n, showValue = true))
            CommandResult(Success(PathResult(pathNodes)), state)
        }

      case Value(nodeId) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(node) =>
            CommandResult(
              Success(ValueResult(nodeId, node.value, formatValueJson(node.value))),
              state
            )
        }

      case Source(nodeId) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(node) =>
            CommandResult(Success(SourceResult(nodeId, node.location)), state)
        }

      case Snippet(nodeId, contextLines) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(node) =>
            node.location match {
              case None =>
                CommandResult(Error(s"No source location for node: ${nodeId.value}"), state)
              case Some(loc) =>
                // Build scope from dependencies
                val scope = state.trace.getDependencies(nodeId).flatMap { dep =>
                  dep.bindingName.map { name =>
                    ScopeEntry(name, dep.value, Some(dep.id))
                  }
                }
                val result = ScopeEntry(
                  node.bindingName.getOrElse("result"),
                  node.value,
                  Some(nodeId)
                )
                // Note: actual line reading would happen in the server
                // Here we just build the structure
                CommandResult(
                  Success(SnippetResult(nodeId, loc, Nil, scope, result)),
                  state
                )
            }
        }

      case Eval(nodeId, expression) =>
        state.trace.getNode(nodeId) match {
          case None =>
            CommandResult(Error(s"Node not found: ${nodeId.value}"), state)
          case Some(node) =>
            // Build scope from dependencies
            val scope = state.trace.getDependencies(nodeId).flatMap { dep =>
              dep.bindingName.map(name => name -> dep.value)
            }.toMap
            // Note: actual evaluation would happen in the server
            // For now, just return the scope
            CommandResult(
              Success(EvalResult(expression, "<eval not implemented>", scope)),
              state
            )
        }

      case Status =>
        val resultNode = state.trace.getNode(state.trace.resultNodeId)
        val resultValue = resultNode.map(_.value).getOrElse("<unknown>")
        CommandResult(
          Success(StatusResult(
            running = true,
            traceFile = state.traceFile,
            nodeCount = state.trace.nodeCount,
            focusNodeId = state.focusNodeId,
            resultNodeId = state.trace.resultNodeId,
            resultValue = formatValueBrief(resultValue)
          )),
          state
        )

      case Shutdown =>
        CommandResult(
          Success(ShutdownResult("Daemon shutting down")),
          state,
          shouldShutdown = true
        )
    }
  }

  /**
   * Convert a TraceNode to a NodeSummary.
   */
  private def toSummary(node: TraceNode, showValue: Boolean): ResponseData.NodeSummary = {
    ResponseData.NodeSummary(
      id = node.id,
      value = if (showValue) Some(node.value) else None,
      typeInfo = inferTypeInfo(node.value),
      source = formatSource(node.source),
      bindingName = node.bindingName,
      location = node.location
    )
  }

  /**
   * Build an explanation tree string.
   */
  private def buildExplanationTree(
      trace: ProvenanceTrace,
      nodeId: NodeId,
      depth: Int,
      visited: Set[NodeId] = Set.empty
  ): String = {
    if (visited.contains(nodeId)) {
      s"${"  " * depth}${nodeId.value}: <circular reference>"
    } else {
      trace.getNode(nodeId) match {
        case None =>
          s"${"  " * depth}${nodeId.value}: <not found>"
        case Some(node) =>
          val indent = "  " * depth
          val binding = node.bindingName.map(n => s"$n = ").getOrElse("")
          val value = formatValueBrief(node.value)
          val src = formatSource(node.source)
          val header = s"$indent${nodeId.value}: $binding$value ← $src"

          if (node.dependencies.isEmpty) {
            header
          } else {
            val newVisited = visited + nodeId
            val children = node.dependencies.map { depId =>
              buildExplanationTree(trace, depId, depth + 1, newVisited)
            }
            (header :: children).mkString("\n")
          }
      }
    }
  }

  /**
   * Build a compact one-line explanation.
   */
  private def buildCompactExplanation(trace: ProvenanceTrace, nodeId: NodeId): String = {
    trace.getNode(nodeId) match {
      case None => s"${nodeId.value}: <not found>"
      case Some(node) =>
        val binding = node.bindingName.map(n => s"$n = ").getOrElse("")
        val value = formatValueBrief(node.value)
        val src = formatSource(node.source)
        s"${nodeId.value}: $binding$value ← $src"
    }
  }

  /**
   * Find path from root to a node.
   */
  private def findPath(trace: ProvenanceTrace, targetId: NodeId): List[TraceNode] = {
    // BFS to find path from result to target
    def bfs(
        queue: List[(NodeId, List[NodeId])],
        visited: Set[NodeId]
    ): List[NodeId] = {
      queue match {
        case Nil => Nil
        case (current, path) :: rest =>
          if (current == targetId) {
            // Path is built from result to target, return in same order (result -> ... -> target)
            path :+ current
          } else if (visited.contains(current)) {
            bfs(rest, visited)
          } else {
            trace.getNode(current) match {
              case None => bfs(rest, visited + current)
              case Some(node) =>
                val newQueue = node.dependencies.map(dep => (dep, path :+ current))
                bfs(rest ++ newQueue, visited + current)
            }
          }
      }
    }

    val pathIds = bfs(List((trace.resultNodeId, Nil)), Set.empty)
    pathIds.flatMap(trace.getNode)
  }

  /**
   * Format source type for display.
   */
  private def formatSource(source: SourceType): String = source match {
    case SourceType.Pure(Some(expr)) =>
      val truncated = if (expr.length > 30) expr.take(30) + "..." else expr
      s"pure($truncated)"
    case SourceType.Pure(None) => "pure"
    case SourceType.Operation(iface, method) => s"$iface.$method()"
    case SourceType.Binding(name) => s"binding($name)"
    case SourceType.Parameter => "parameter"
    case SourceType.Literal => "literal"
  }

  /**
   * Format a value briefly for display.
   */
  private def formatValueBrief(value: String): String = {
    // Try to detect type from the JSON-like value string
    if (value == "null") "null"
    else if (value == "true" || value == "false") value
    else if (value.startsWith("\"")) {
      val content = value.stripPrefix("\"").stripSuffix("\"")
      if (content.length > 20) s"\"${content.take(20)}...\""
      else value
    }
    else if (value.startsWith("[")) {
      // Array - try to count elements
      val content = value.stripPrefix("[").stripSuffix("]").trim
      if (content.isEmpty) "[]"
      else s"[...]"
    }
    else if (value.startsWith("{")) "{...}"
    else if (value.matches("-?\\d+\\.?\\d*")) value
    else value.take(20)
  }

  /**
   * Format value as pretty JSON.
   */
  private def formatValueJson(value: String): String = {
    // Simple indentation for JSON-like strings
    // In production, would use a proper JSON library
    value
  }

  /**
   * Infer type info from value string.
   */
  private def inferTypeInfo(value: String): String = {
    if (value == "null") "<null>"
    else if (value == "true" || value == "false") "<boolean>"
    else if (value.startsWith("\"")) "<string>"
    else if (value.startsWith("[")) "<array>"
    else if (value.startsWith("{")) "<object>"
    else if (value.matches("-?\\d+")) "<int>"
    else if (value.matches("-?\\d+\\.\\d+")) "<number>"
    else "<unknown>"
  }
}
