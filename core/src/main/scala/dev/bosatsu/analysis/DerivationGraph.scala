package dev.bosatsu.analysis

/**
 * A derivation graph captures the complete provenance of a computation.
 *
 * Given any value in the computation, you can:
 * - Trace backwards through `dependencies` to see where it came from
 * - Trace forwards through `usages` to see where it's used
 * - Access the original AST via `nodes` for full type/region info
 *
 * This enables "spreadsheet-style" debugging: click any value to see
 * its complete derivation chain back to inputs.
 *
 * @tparam T The tag type of the AST nodes (typically Declaration or Region)
 */
final case class DerivationGraph[T](
    nodes: Map[Long, ProvenanceNode[T]],
    dependencies: Map[Long, Set[Long]], // node -> nodes it depends on
    usages: Map[Long, Set[Long]],       // node -> nodes that use it
    roots: Set[Long]                    // entry points (exported bindings)
) {

  /**
   * Get all nodes that this node directly depends on.
   */
  def directDependencies(id: Long): Set[Long] =
    dependencies.getOrElse(id, Set.empty)

  /**
   * Get all nodes that directly use this node.
   */
  def directUsages(id: Long): Set[Long] =
    usages.getOrElse(id, Set.empty)

  /**
   * Get all nodes that this node transitively depends on (full derivation).
   */
  def allDependencies(id: Long): Set[Long] = {
    def go(current: Set[Long], visited: Set[Long]): Set[Long] = {
      val unvisited = current -- visited
      if (unvisited.isEmpty) visited
      else {
        val next = unvisited.flatMap(directDependencies)
        go(next, visited ++ unvisited)
      }
    }
    go(Set(id), Set.empty) - id
  }

  /**
   * Get all nodes that transitively use this node.
   */
  def allUsages(id: Long): Set[Long] = {
    def go(current: Set[Long], visited: Set[Long]): Set[Long] = {
      val unvisited = current -- visited
      if (unvisited.isEmpty) visited
      else {
        val next = unvisited.flatMap(directUsages)
        go(next, visited ++ unvisited)
      }
    }
    go(Set(id), Set.empty) - id
  }

  /**
   * Build the derivation chain from a node back to roots.
   * Returns a list of (nodeId, depth) pairs in breadth-first order.
   */
  def derivationChain(id: Long): List[(Long, Int)] = {
    def go(
        current: List[(Long, Int)],
        visited: Set[Long],
        acc: List[(Long, Int)]
    ): List[(Long, Int)] = {
      current match {
        case Nil => acc.reverse
        case (nodeId, depth) :: rest =>
          if (visited.contains(nodeId)) go(rest, visited, acc)
          else {
            val deps = directDependencies(nodeId).toList.map((_, depth + 1))
            go(rest ++ deps, visited + nodeId, (nodeId, depth) :: acc)
          }
      }
    }
    go(List((id, 0)), Set.empty, Nil)
  }

  /**
   * Find all root nodes (nodes with no usages).
   * These are typically exported bindings.
   */
  def findRoots: Set[Long] =
    nodes.keySet.filter(id => directUsages(id).isEmpty)

  /**
   * Find all leaf nodes (nodes with no dependencies).
   * These are typically literals or external references.
   */
  def findLeaves: Set[Long] =
    nodes.keySet.filter(id => directDependencies(id).isEmpty)

  /**
   * Merge two derivation graphs.
   * Used when combining analysis from multiple expressions.
   */
  def ++(other: DerivationGraph[T]): DerivationGraph[T] = {
    val mergedDeps = (dependencies.keySet ++ other.dependencies.keySet)
      .map { k =>
        k -> (dependencies.getOrElse(k, Set.empty) ++
          other.dependencies.getOrElse(k, Set.empty))
      }
      .toMap

    val mergedUsages = (usages.keySet ++ other.usages.keySet)
      .map { k =>
        k -> (usages.getOrElse(k, Set.empty) ++
          other.usages.getOrElse(k, Set.empty))
      }
      .toMap

    DerivationGraph(
      nodes ++ other.nodes,
      mergedDeps,
      mergedUsages,
      roots ++ other.roots
    )
  }

  /**
   * Filter the graph to only include nodes reachable from the given roots.
   */
  def reachableFrom(nodeIds: Set[Long]): DerivationGraph[T] = {
    val reachable = nodeIds.flatMap(id => allDependencies(id) + id)
    DerivationGraph(
      nodes.filter { case (id, _) => reachable.contains(id) },
      dependencies.filter { case (id, _) => reachable.contains(id) },
      usages.filter { case (id, _) => reachable.contains(id) },
      nodeIds.intersect(nodes.keySet)
    )
  }
}

object DerivationGraph {

  /**
   * An empty derivation graph.
   */
  def empty[T]: DerivationGraph[T] = DerivationGraph(
    Map.empty,
    Map.empty,
    Map.empty,
    Set.empty
  )

  /**
   * Builder for constructing a derivation graph incrementally.
   */
  final class Builder[T] {
    private var nextId: Long = 0L
    private var nodes: Map[Long, ProvenanceNode[T]] = Map.empty
    private var deps: Map[Long, Set[Long]] = Map.empty
    private var usages: Map[Long, Set[Long]] = Map.empty
    private var roots: Set[Long] = Set.empty

    def freshId(): Long = {
      val id = nextId
      nextId += 1
      id
    }

    def addNode(node: ProvenanceNode[T]): Unit = {
      nodes = nodes + (node.id -> node)
    }

    def addDependency(from: Long, to: Long): Unit = {
      deps = deps.updatedWith(from)(existing =>
        Some(existing.getOrElse(Set.empty) + to)
      )
      usages = usages.updatedWith(to)(existing =>
        Some(existing.getOrElse(Set.empty) + from)
      )
    }

    def addDependencies(from: Long, tos: Iterable[Long]): Unit = {
      tos.foreach(to => addDependency(from, to))
    }

    def addRoot(id: Long): Unit = {
      roots = roots + id
    }

    def build(): DerivationGraph[T] = DerivationGraph(nodes, deps, usages, roots)
  }
}
