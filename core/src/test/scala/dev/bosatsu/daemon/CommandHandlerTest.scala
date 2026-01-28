package dev.bosatsu.daemon

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object CommandHandlerTest extends Properties("CommandHandler") {
  import DaemonGen._

  // =========================================================================
  // List command tests
  // =========================================================================

  property("list returns all nodes") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.List(), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.NodeList(nodes)) =>
        nodes.size == state.trace.nodeCount
      case _ => false
    }
  }

  property("list preserves state") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.List(), state)
    result.newState == state && !result.shouldShutdown
  }

  property("list with showValues includes values") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.List(showValues = true), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.NodeList(nodes)) =>
        nodes.forall(_.value.isDefined)
      case _ => false
    }
  }

  property("list without showValues excludes values") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.List(showValues = false), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.NodeList(nodes)) =>
        nodes.forall(_.value.isEmpty)
      case _ => false
    }
  }

  // =========================================================================
  // Explain command tests
  // =========================================================================

  property("explain with valid node succeeds") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Explain(Some(nodeId)), state)
    result.response.isSuccess
  }

  property("explain without nodeId uses effective node (focus or result)") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.Explain(None), state)
    val expectedId = state.focusNodeId.getOrElse(state.trace.resultNodeId)
    result.response match {
      case DaemonResponse.Success(ResponseData.ExplainResult(nodeId, _, _, _)) =>
        nodeId == expectedId
      case _ => false
    }
  }

  property("explain with focus uses focused node") = forAll(daemonState) { state =>
    val focusId = state.trace.nodes.keys.headOption.getOrElse(state.trace.resultNodeId)
    val focusedState = state.withFocus(focusId)
    val result = CommandHandler.handle(DaemonCommand.Explain(None), focusedState)
    result.response match {
      case DaemonResponse.Success(ResponseData.ExplainResult(nodeId, _, _, _)) =>
        nodeId == focusId
      case _ => false
    }
  }

  property("explain with invalid node fails") = forAll(daemonState) { state =>
    val invalidId = NodeId("nonexistent")
    val result = CommandHandler.handle(DaemonCommand.Explain(Some(invalidId)), state)
    result.response match {
      case DaemonResponse.Error(msg) => msg.contains("not found")
      case _ => false
    }
  }

  // =========================================================================
  // Find command tests
  // =========================================================================

  property("find returns nodes with matching value") = forAll(daemonState) { state =>
    // Find the value of the first node
    state.trace.nodes.headOption match {
      case Some((_, node)) =>
        val result = CommandHandler.handle(DaemonCommand.Find(node.value), state)
        result.response match {
          case DaemonResponse.Success(ResponseData.FindResult(nodes)) =>
            nodes.exists(_.id == node.id)
          case _ => false
        }
      case None => true // Empty trace is valid
    }
  }

  property("find returns empty for non-matching value") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.Find("__nonexistent_value__"), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.FindResult(nodes)) =>
        nodes.isEmpty
      case _ => false
    }
  }

  // =========================================================================
  // Deps command tests
  // =========================================================================

  property("deps with valid node succeeds") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Deps(nodeId), state)
    result.response.isSuccess
  }

  property("deps returns correct dependencies") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Deps(nodeId), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.DepsResult(_, deps)) =>
        val expectedDeps = state.trace.getDependencies(nodeId).map(_.id).toSet
        deps.map(_.id).toSet == expectedDeps
      case _ => false
    }
  }

  property("deps with invalid node fails") = forAll(daemonState) { state =>
    val invalidId = NodeId("nonexistent")
    val result = CommandHandler.handle(DaemonCommand.Deps(invalidId), state)
    result.response match {
      case DaemonResponse.Error(msg) => msg.contains("not found")
      case _ => false
    }
  }

  // =========================================================================
  // Usages command tests
  // =========================================================================

  property("usages with valid node succeeds") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Usages(nodeId), state)
    result.response.isSuccess
  }

  property("usages returns correct usages") = forAll(daemonState) { state =>
    val nodeId = state.trace.nodes.keys.headOption.getOrElse(state.trace.resultNodeId)
    val result = CommandHandler.handle(DaemonCommand.Usages(nodeId), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.UsagesResult(_, usages)) =>
        val expectedUsages = state.trace.getUsages(nodeId).map(_.id).toSet
        usages.map(_.id).toSet == expectedUsages
      case _ => false
    }
  }

  // =========================================================================
  // Focus command tests
  // =========================================================================

  property("focus updates state") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Focus(nodeId), state)
    result.response.isSuccess && result.newState.focusNodeId == Some(nodeId)
  }

  property("unfocus clears focus") = forAll(daemonState) { state =>
    val focusedState = state.withFocus(state.trace.resultNodeId)
    val result = CommandHandler.handle(DaemonCommand.Unfocus, focusedState)
    result.response.isSuccess && result.newState.focusNodeId.isEmpty
  }

  property("focus with invalid node fails") = forAll(daemonState) { state =>
    val invalidId = NodeId("nonexistent")
    val result = CommandHandler.handle(DaemonCommand.Focus(invalidId), state)
    result.response match {
      case DaemonResponse.Error(msg) => msg.contains("not found")
      case _ => false
    }
  }

  // =========================================================================
  // Path command tests
  // =========================================================================

  property("path with valid node succeeds") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Path(nodeId), state)
    result.response.isSuccess
  }

  property("path includes target node") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Path(nodeId), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.PathResult(path)) =>
        // Path should include the target or be empty if unreachable
        path.isEmpty || path.exists(_.id == nodeId)
      case _ => false
    }
  }

  // =========================================================================
  // Value command tests
  // =========================================================================

  property("value with valid node returns value") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Value(nodeId), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.ValueResult(id, value, _)) =>
        id == nodeId && state.trace.getNode(nodeId).exists(_.value == value)
      case _ => false
    }
  }

  // =========================================================================
  // Source command tests
  // =========================================================================

  property("source with valid node succeeds") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Source(nodeId), state)
    result.response.isSuccess
  }

  property("source returns correct location") = forAll(daemonState) { state =>
    val nodeId = state.trace.resultNodeId
    val result = CommandHandler.handle(DaemonCommand.Source(nodeId), state)
    result.response match {
      case DaemonResponse.Success(ResponseData.SourceResult(id, loc)) =>
        id == nodeId && loc == state.trace.getNode(nodeId).flatMap(_.location)
      case _ => false
    }
  }

  // =========================================================================
  // Status command tests
  // =========================================================================

  property("status returns correct info") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.Status, state)
    result.response match {
      case DaemonResponse.Success(ResponseData.StatusResult(running, traceFile, nodeCount, focus, resultId, _)) =>
        running &&
          traceFile == state.traceFile &&
          nodeCount == state.trace.nodeCount &&
          focus == state.focusNodeId &&
          resultId == state.trace.resultNodeId
      case _ => false
    }
  }

  // =========================================================================
  // Shutdown command tests
  // =========================================================================

  property("shutdown sets shouldShutdown flag") = forAll(daemonState) { state =>
    val result = CommandHandler.handle(DaemonCommand.Shutdown, state)
    result.response.isSuccess && result.shouldShutdown
  }

  // =========================================================================
  // General properties
  // =========================================================================

  property("non-shutdown commands don't set shutdown flag") = forAll(daemonState) { state =>
    val commands = List(
      DaemonCommand.List(),
      DaemonCommand.Explain(None),
      DaemonCommand.Status,
      DaemonCommand.Unfocus
    )
    commands.forall { cmd =>
      val result = CommandHandler.handle(cmd, state)
      !result.shouldShutdown
    }
  }

  property("read-only commands preserve state") = forAll(daemonState) { state =>
    val readOnlyCommands = List(
      DaemonCommand.List(),
      DaemonCommand.Explain(None),
      DaemonCommand.Status,
      DaemonCommand.Deps(state.trace.resultNodeId),
      DaemonCommand.Usages(state.trace.resultNodeId),
      DaemonCommand.Path(state.trace.resultNodeId),
      DaemonCommand.Value(state.trace.resultNodeId),
      DaemonCommand.Source(state.trace.resultNodeId)
    )
    readOnlyCommands.forall { cmd =>
      val result = CommandHandler.handle(cmd, state)
      result.newState == state
    }
  }
}
