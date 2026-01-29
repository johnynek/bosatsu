package dev.bosatsu.permissions

import cats.data.NonEmptyList
import cats.implicits._
import dev.bosatsu.{Identifier, Matchless, PackageName}
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.graph.Toposort

/**
 * Permission propagation through call graphs.
 *
 * When function A calls function B, A inherits B's permission requirements.
 * This module computes the transitive closure of permissions through the
 * call graph.
 *
 * Example:
 *   - getUserEmail requires 'users:read'
 *   - sendWelcome calls getUserEmail
 *   - Therefore sendWelcome also requires 'users:read'
 *
 * The algorithm:
 * 1. Build the call graph from expression analysis
 * 2. Topologically sort to process callees before callers
 * 3. Propagate permissions bottom-up through the graph
 */
object CallGraphPermissions {

  /**
   * A node in the call graph, representing a function/binding.
   */
  case class CallNode(
      pkg: PackageName,
      name: Bindable,
      directPermissions: List[Permission],
      calls: Set[(PackageName, Bindable)]
  ) {
    def id: (PackageName, Bindable) = (pkg, name)

    def isEmpty: Boolean = directPermissions.isEmpty && calls.isEmpty
  }

  object CallNode {
    def fromAnalysis(
        pkg: PackageName,
        name: Bindable,
        result: PermissionAnalyzer.AnalysisResult
    ): CallNode = {
      CallNode(
        pkg = pkg,
        name = name,
        directPermissions = result.staticRequirements,
        calls = result.calledFunctions
      )
    }
  }

  /**
   * The complete call graph for a set of packages.
   */
  case class CallGraph(
      nodes: Map[(PackageName, Bindable), CallNode],
      edges: Map[(PackageName, Bindable), Set[(PackageName, Bindable)]]
  ) {
    def nodeIds: Set[(PackageName, Bindable)] = nodes.keySet

    def callers(id: (PackageName, Bindable)): Set[(PackageName, Bindable)] = {
      edges.filter(_._2.contains(id)).keySet
    }

    def callees(id: (PackageName, Bindable)): Set[(PackageName, Bindable)] = {
      edges.getOrElse(id, Set.empty)
    }

    def node(id: (PackageName, Bindable)): Option[CallNode] = nodes.get(id)

    def isEmpty: Boolean = nodes.isEmpty
  }

  object CallGraph {
    val empty: CallGraph = CallGraph(Map.empty, Map.empty)

    def fromNodes(nodes: List[CallNode]): CallGraph = {
      val nodeMap = nodes.map(n => n.id -> n).toMap
      val edges = nodes.map(n => n.id -> n.calls).toMap
      CallGraph(nodeMap, edges)
    }
  }

  /**
   * Result of permission propagation.
   */
  case class PropagatedPermissions(
      permissions: Map[(PackageName, Bindable), List[Permission]],
      cycles: List[List[(PackageName, Bindable)]]
  ) {
    def get(pkg: PackageName, name: Bindable): List[Permission] =
      permissions.getOrElse((pkg, name), Nil)

    def hasCycles: Boolean = cycles.nonEmpty

    def allPermissions: List[Permission] =
      permissions.values.flatten.toList.distinct
  }

  /**
   * Build a call graph from analyzed bindings across packages.
   */
  def buildCallGraph[A](
      packages: Map[PackageName, List[(Bindable, Matchless.Expr[A])]],
      annotations: PermissionAnalyzer.PermissionAnnotations
  ): CallGraph = {
    val nodes = packages.flatMap { case (pkg, bindings) =>
      bindings.map { case (name, expr) =>
        val result = PermissionAnalyzer.analyze(expr, annotations)
        CallNode.fromAnalysis(pkg, name, result)
      }
    }.toList

    CallGraph.fromNodes(nodes)
  }

  /**
   * Propagate permissions through the call graph.
   *
   * Uses topological sort to ensure callees are processed before callers,
   * then propagates permissions bottom-up.
   */
  def propagatePermissions(graph: CallGraph): PropagatedPermissions = {
    if (graph.isEmpty) {
      return PropagatedPermissions(Map.empty, Nil)
    }

    // Build adjacency list for toposort
    val dependencies: Map[(PackageName, Bindable), List[(PackageName, Bindable)]] =
      graph.edges.map { case (k, v) => k -> v.toList }

    // Toposort to get processing order (callees before callers)
    val sortResult = toposortWithCycles(graph.nodeIds.toList, dependencies)

    // Initialize with direct permissions
    var permissions: Map[(PackageName, Bindable), List[Permission]] =
      graph.nodes.map { case (id, node) => id -> node.directPermissions }

    // Process in topological order, propagating permissions
    sortResult.sorted.foreach { id =>
      val callees = graph.callees(id)
      val calleePerms = callees.toList.flatMap(cid => permissions.getOrElse(cid, Nil))
      val currentPerms = permissions.getOrElse(id, Nil)
      permissions = permissions + (id -> (currentPerms ++ calleePerms).distinct)
    }

    PropagatedPermissions(permissions, sortResult.cycles)
  }

  /**
   * Simple topological sort with cycle detection.
   */
  private case class ToposortResult(
      sorted: List[(PackageName, Bindable)],
      cycles: List[List[(PackageName, Bindable)]]
  )

  private def toposortWithCycles(
      nodes: List[(PackageName, Bindable)],
      edges: Map[(PackageName, Bindable), List[(PackageName, Bindable)]]
  ): ToposortResult = {
    var visited = Set.empty[(PackageName, Bindable)]
    var inStack = Set.empty[(PackageName, Bindable)]
    var result = List.empty[(PackageName, Bindable)]
    var cycles = List.empty[List[(PackageName, Bindable)]]

    def visit(node: (PackageName, Bindable), path: List[(PackageName, Bindable)]): Unit = {
      if (inStack.contains(node)) {
        // Found a cycle - extract from path (node is already in path at cycleStart)
        val cycleStart = path.indexOf(node)
        if (cycleStart >= 0) {
          // path is [most recent, ..., cycleStart node], reverse to get proper order
          cycles = path.take(cycleStart + 1).reverse :: cycles
        }
        return
      }

      if (visited.contains(node)) {
        return
      }

      inStack = inStack + node
      val deps = edges.getOrElse(node, Nil)
      deps.foreach(dep => visit(dep, node :: path))
      inStack = inStack - node
      visited = visited + node
      result = node :: result
    }

    nodes.foreach(n => if (!visited.contains(n)) visit(n, Nil))

    ToposortResult(result.reverse, cycles)
  }

  /**
   * Find all entry points in the call graph (functions not called by others).
   */
  def findEntryPoints(graph: CallGraph): Set[(PackageName, Bindable)] = {
    val allCallees = graph.edges.values.flatten.toSet
    graph.nodeIds -- allCallees
  }

  /**
   * Find the permission frontier - the minimal set of permission checks
   * needed to cover all entry points.
   *
   * This is an optimization: instead of checking permissions at every call,
   * check at entry points. If entry point has all required permissions,
   * no further checks needed.
   */
  def findPermissionFrontier(graph: CallGraph): Map[(PackageName, Bindable), List[Permission]] = {
    val propagated = propagatePermissions(graph)
    val entryPoints = findEntryPoints(graph)

    entryPoints.map { ep =>
      ep -> propagated.get(ep._1, ep._2)
    }.toMap
  }

  /**
   * Check if all callers of a function have the same or broader permissions.
   *
   * This is used to determine if a permission check can be hoisted to
   * the caller level.
   */
  def canHoistPermission(
      graph: CallGraph,
      propagated: PropagatedPermissions,
      functionId: (PackageName, Bindable),
      permission: Permission
  ): Boolean = {
    val callers = graph.callers(functionId)
    if (callers.isEmpty) {
      // Entry point - can't hoist further
      false
    } else {
      callers.forall { callerId =>
        propagated.get(callerId._1, callerId._2).contains(permission)
      }
    }
  }

  /**
   * Generate a permission checking plan that minimizes runtime checks.
   *
   * Returns a map of function -> permissions to check at that function.
   * Permissions that can be hoisted are not checked at inner functions.
   */
  def generateCheckingPlan(
      graph: CallGraph,
      propagated: PropagatedPermissions
  ): Map[(PackageName, Bindable), List[Permission]] = {
    var plan = Map.empty[(PackageName, Bindable), List[Permission]]

    // Start with entry points - they must check all their permissions
    val entryPoints = findEntryPoints(graph)
    entryPoints.foreach { ep =>
      plan = plan + (ep -> propagated.get(ep._1, ep._2))
    }

    // For non-entry functions, only check permissions not already checked by callers
    graph.nodeIds.diff(entryPoints).foreach { nodeId =>
      val allNeeded = propagated.get(nodeId._1, nodeId._2)
      val alreadyChecked = graph.callers(nodeId).flatMap { callerId =>
        plan.getOrElse(callerId, Nil)
      }
      val toCheck = allNeeded.filterNot(alreadyChecked.contains)
      if (toCheck.nonEmpty) {
        plan = plan + (nodeId -> toCheck)
      }
    }

    plan
  }
}
