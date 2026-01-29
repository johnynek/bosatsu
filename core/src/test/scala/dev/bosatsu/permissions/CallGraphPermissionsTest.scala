package dev.bosatsu.permissions

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Gen
import dev.bosatsu.{Identifier, PackageName}
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.MonadGen.genMonad
import cats.implicits._

object CallGraphPermissionsTest extends Properties("CallGraphPermissions") {
  import PermissionGen._
  import CallGraphPermissions._

  // Generate a simple package name
  val genPackageName: Gen[PackageName] = for {
    part1 <- Gen.alphaUpperChar.map(_.toString)
    part2 <- Gen.alphaLowerStr.suchThat(_.nonEmpty)
  } yield PackageName.parts(part1, part2)

  // Generate a bindable name
  val genBindable: Gen[Bindable] = Gen.alphaLowerStr.suchThat(_.nonEmpty).map(Identifier.Name(_))

  // Generate a function ID
  val genFunctionId: Gen[(PackageName, Bindable)] = for {
    pkg <- genPackageName
    name <- genBindable
  } yield (pkg, name)

  // Generate a CallNode
  def genCallNode(ids: List[(PackageName, Bindable)]): Gen[CallNode] = for {
    id <- Gen.oneOf(ids)
    permCount <- Gen.choose(0, 3)
    perms <- Gen.listOfN(permCount, permission)
    callCount <- Gen.choose(0, math.min(3, ids.length - 1))
    calls <- Gen.pick(callCount, ids.filterNot(_ == id)).map(_.toSet)
  } yield CallNode(id._1, id._2, perms, calls)

  // Generate a simple acyclic call graph
  def genAcyclicCallGraph(size: Int): Gen[CallGraph] = for {
    ids <- Gen.listOfN(size, genFunctionId)
    uniqueIds = ids.distinctBy(id => (id._1.asString, id._2.sourceCodeRepr))
    nodes <- uniqueIds.zipWithIndex.traverse { case ((pkg, name), idx) =>
      for {
        permCount <- Gen.choose(0, 2)
        perms <- Gen.listOfN(permCount, permission)
        // Only call functions with lower indices to avoid cycles
        callCount <- Gen.choose(0, math.min(2, idx))
        callIndices <- Gen.pick(callCount, 0 until idx).map(_.toList)
        calls = callIndices.map(uniqueIds(_)).toSet
      } yield CallNode(pkg, name, perms, calls)
    }
  } yield CallGraph.fromNodes(nodes)

  property("empty call graph has no permissions") = {
    val graph = CallGraph.empty
    val result = propagatePermissions(graph)
    result.permissions.isEmpty && result.cycles.isEmpty
  }

  property("single node graph preserves direct permissions") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val name = Identifier.Name("fn")
    val node = CallNode(pkg, name, List(perm), Set.empty)
    val graph = CallGraph.fromNodes(List(node))

    val result = propagatePermissions(graph)
    result.get(pkg, name).contains(perm)
  }

  property("permissions propagate through calls") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val callee = Identifier.Name("callee")
    val caller = Identifier.Name("caller")

    val calleeNode = CallNode(pkg, callee, List(perm), Set.empty)
    val callerNode = CallNode(pkg, caller, Nil, Set((pkg, callee)))
    val graph = CallGraph.fromNodes(List(calleeNode, callerNode))

    val result = propagatePermissions(graph)
    // Callee has original permission
    result.get(pkg, callee).contains(perm) &&
    // Caller inherits callee's permission
    result.get(pkg, caller).contains(perm)
  }

  property("entry points have no callers") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val entry = Identifier.Name("entry")
    val inner = Identifier.Name("inner")

    val innerNode = CallNode(pkg, inner, Nil, Set.empty)
    val entryNode = CallNode(pkg, entry, Nil, Set((pkg, inner)))
    val graph = CallGraph.fromNodes(List(innerNode, entryNode))

    val entries = findEntryPoints(graph)
    entries.contains((pkg, entry)) && !entries.contains((pkg, inner))
  }

  property("permission frontier includes entry point permissions") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val callee = Identifier.Name("callee")
    val entry = Identifier.Name("entry")

    val calleeNode = CallNode(pkg, callee, List(perm), Set.empty)
    val entryNode = CallNode(pkg, entry, Nil, Set((pkg, callee)))
    val graph = CallGraph.fromNodes(List(calleeNode, entryNode))

    val frontier = findPermissionFrontier(graph)
    frontier.get((pkg, entry)).exists(_.contains(perm))
  }

  property("canHoistPermission returns false for entry points") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val entry = Identifier.Name("entry")

    val node = CallNode(pkg, entry, List(perm), Set.empty)
    val graph = CallGraph.fromNodes(List(node))
    val propagated = propagatePermissions(graph)

    !canHoistPermission(graph, propagated, (pkg, entry), perm)
  }

  property("canHoistPermission returns true if all callers have permission") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val callee = Identifier.Name("callee")
    val caller = Identifier.Name("caller")

    val calleeNode = CallNode(pkg, callee, List(perm), Set.empty)
    val callerNode = CallNode(pkg, caller, List(perm), Set((pkg, callee)))
    val graph = CallGraph.fromNodes(List(calleeNode, callerNode))
    val propagated = propagatePermissions(graph)

    canHoistPermission(graph, propagated, (pkg, callee), perm)
  }

  property("checking plan includes entry point permissions") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val entry = Identifier.Name("entry")

    val node = CallNode(pkg, entry, List(perm), Set.empty)
    val graph = CallGraph.fromNodes(List(node))
    val propagated = propagatePermissions(graph)

    val plan = generateCheckingPlan(graph, propagated)
    plan.get((pkg, entry)).exists(_.contains(perm))
  }

  property("CallNode.isEmpty is true when no permissions and no calls") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val name = Identifier.Name("fn")
    val node = CallNode(pkg, name, Nil, Set.empty)
    node.isEmpty
  }

  property("CallNode.isEmpty is false with permissions") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val name = Identifier.Name("fn")
    val node = CallNode(pkg, name, List(perm), Set.empty)
    !node.isEmpty
  }

  property("CallNode.isEmpty is false with calls") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val name = Identifier.Name("fn")
    val callee = Identifier.Name("callee")
    val node = CallNode(pkg, name, Nil, Set((pkg, callee)))
    !node.isEmpty
  }

  property("CallGraph.isEmpty is true for empty graph") = {
    CallGraph.empty.isEmpty
  }

  property("CallGraph.isEmpty is false for non-empty graph") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val name = Identifier.Name("fn")
    val node = CallNode(pkg, name, Nil, Set.empty)
    !CallGraph.fromNodes(List(node)).isEmpty
  }

  property("CallGraph callers returns nodes that call a given node") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val callee = Identifier.Name("callee")
    val caller1 = Identifier.Name("caller1")
    val caller2 = Identifier.Name("caller2")

    val calleeNode = CallNode(pkg, callee, Nil, Set.empty)
    val caller1Node = CallNode(pkg, caller1, Nil, Set((pkg, callee)))
    val caller2Node = CallNode(pkg, caller2, Nil, Set((pkg, callee)))
    val graph = CallGraph.fromNodes(List(calleeNode, caller1Node, caller2Node))

    val callers = graph.callers((pkg, callee))
    callers.contains((pkg, caller1)) && callers.contains((pkg, caller2))
  }

  property("CallGraph callees returns nodes called by a given node") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val caller = Identifier.Name("caller")
    val callee1 = Identifier.Name("callee1")
    val callee2 = Identifier.Name("callee2")

    val callee1Node = CallNode(pkg, callee1, Nil, Set.empty)
    val callee2Node = CallNode(pkg, callee2, Nil, Set.empty)
    val callerNode = CallNode(pkg, caller, Nil, Set((pkg, callee1), (pkg, callee2)))
    val graph = CallGraph.fromNodes(List(callee1Node, callee2Node, callerNode))

    val callees = graph.callees((pkg, caller))
    callees.contains((pkg, callee1)) && callees.contains((pkg, callee2))
  }

  property("PropagatedPermissions allPermissions contains all unique permissions") = forAll(permission, permission) { (perm1, perm2) =>
    val pkg = PackageName.parts("Test", "Pkg")
    val fn1: Bindable = Identifier.Name("fn1")
    val fn2: Bindable = Identifier.Name("fn2")

    val perms: Map[(PackageName, Bindable), List[Permission]] = Map(
      (pkg, fn1) -> List(perm1),
      (pkg, fn2) -> List(perm2)
    )
    val result = PropagatedPermissions(perms, Nil)

    result.allPermissions.contains(perm1) && result.allPermissions.contains(perm2)
  }

  property("PropagatedPermissions hasCycles is true when cycles exist") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val fn = Identifier.Name("fn")
    val cycle = List((pkg, fn))
    val result = PropagatedPermissions(Map.empty, List(cycle))
    result.hasCycles
  }

  property("PropagatedPermissions hasCycles is false when no cycles") = {
    val result = PropagatedPermissions(Map.empty, Nil)
    !result.hasCycles
  }

  property("toposort detects cycles") = {
    val pkg = PackageName.parts("Test", "Pkg")
    val a = Identifier.Name("a")
    val b = Identifier.Name("b")

    // a calls b, b calls a - cycle!
    val nodeA = CallNode(pkg, a, Nil, Set((pkg, b)))
    val nodeB = CallNode(pkg, b, Nil, Set((pkg, a)))
    val graph = CallGraph.fromNodes(List(nodeA, nodeB))

    val result = propagatePermissions(graph)
    result.hasCycles
  }

  property("permissions deduplicated after propagation") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Test", "Pkg")
    val callee = Identifier.Name("callee")
    val caller = Identifier.Name("caller")

    // Both caller and callee have the same permission
    val calleeNode = CallNode(pkg, callee, List(perm), Set.empty)
    val callerNode = CallNode(pkg, caller, List(perm), Set((pkg, callee)))
    val graph = CallGraph.fromNodes(List(calleeNode, callerNode))

    val result = propagatePermissions(graph)
    // Caller should only have the permission once (deduplicated)
    result.get(pkg, caller).count(_ == perm) == 1
  }
}
