package dev.bosatsu.daemon

import org.scalacheck.{Arbitrary, Gen}
import cats.implicits._
import dev.bosatsu.MonadGen.genMonad

/**
 * ScalaCheck generators for daemon types.
 */
object DaemonGen {

  // Node ID generator
  val genNodeId: Gen[NodeId] = for {
    num <- Gen.choose(0, 1000)
  } yield NodeId(s"n$num")

  // Alias for backward compatibility
  val nodeId: Gen[NodeId] = genNodeId

  // Source location generator
  val genSourceLocation: Gen[SourceLocation] = for {
    file <- Gen.alphaLowerStr.suchThat(_.nonEmpty).map(_ + ".bosatsu")
    line <- Gen.choose(1, 1000)
    column <- Gen.choose(1, 80)
  } yield SourceLocation(file, line, column)

  // Alias for backward compatibility
  val sourceLocation: Gen[SourceLocation] = genSourceLocation

  // Source type generators
  val sourcePure: Gen[SourceType.Pure] = for {
    expr <- Gen.option(Gen.alphaNumStr.suchThat(_.nonEmpty))
  } yield SourceType.Pure(expr)

  val sourceOperation: Gen[SourceType.Operation] = for {
    iface <- Gen.oneOf("db", "api", "cache", "auth", "logger")
    method <- Gen.oneOf("get", "set", "delete", "query", "validate")
  } yield SourceType.Operation(iface, method)

  val sourceBinding: Gen[SourceType.Binding] = for {
    name <- Gen.alphaLowerStr.suchThat(_.nonEmpty)
  } yield SourceType.Binding(name)

  val sourceType: Gen[SourceType] = Gen.frequency(
    (3, sourcePure),
    (3, sourceOperation),
    (2, sourceBinding),
    (1, Gen.const(SourceType.Parameter)),
    (1, Gen.const(SourceType.Literal))
  )

  // Value generators (JSON-like strings)
  val jsonValue: Gen[String] = Gen.frequency(
    (2, Gen.const("null")),
    (2, Gen.oneOf("true", "false")),
    (3, Gen.choose(-1000, 1000).map(_.toString)),
    (3, Gen.alphaNumStr.map(s => s"\"$s\"")),
    (1, Gen.const("[]")),
    (1, Gen.const("{}"))
  )

  // Trace node generator (without dependencies to avoid cycles)
  def traceNode(id: NodeId): Gen[TraceNode] = for {
    value <- jsonValue
    source <- sourceType
    bindingName <- Gen.option(Gen.alphaLowerStr.suchThat(_.nonEmpty))
    location <- Gen.option(sourceLocation)
  } yield TraceNode(id, value, source, bindingName, location, Nil, Nil)

  // Generate a simple trace with connected nodes
  def simpleTrace(nodeCount: Int): Gen[ProvenanceTrace] = {
    require(nodeCount > 0, "nodeCount must be positive")

    for {
      // Generate node IDs
      ids <- Gen.const((0 until nodeCount).map(i => NodeId(s"n$i")).toList)

      // Generate nodes with dependencies (each node depends on previous nodes)
      nodes <- ids.zipWithIndex.traverse { case (id, idx) =>
        for {
          baseNode <- traceNode(id)
          // Dependencies are from earlier nodes only (to avoid cycles)
          depCount <- Gen.choose(0, math.min(2, idx))
          deps <- Gen.pick(depCount, ids.take(idx)).map(_.toList)
        } yield baseNode.copy(dependencies = deps)
      }

      // Build usedBy from dependencies
      nodesWithUsedBy = {
        val nodeMap = nodes.map(n => n.id -> n).toMap
        val usedByMap = nodes.foldLeft(Map.empty[NodeId, List[NodeId]]) { (acc, node) =>
          node.dependencies.foldLeft(acc) { (acc2, depId) =>
            acc2.updated(depId, node.id :: acc2.getOrElse(depId, Nil))
          }
        }
        nodes.map { node =>
          node.copy(usedBy = usedByMap.getOrElse(node.id, Nil))
        }
      }

      sourceFile <- Gen.alphaLowerStr.suchThat(_.nonEmpty).map(_ + ".bosatsu")
    } yield ProvenanceTrace(
      nodes = nodesWithUsedBy.map(n => n.id -> n).toMap,
      resultNodeId = ids.last,
      sourceFile = sourceFile
    )
  }

  // Daemon state generator
  val daemonState: Gen[DaemonState] = for {
    nodeCount <- Gen.choose(1, 10)
    trace <- simpleTrace(nodeCount)
    focusNodeId <- Gen.option(Gen.oneOf(trace.nodes.keys.toSeq))
    traceFile <- Gen.alphaLowerStr.suchThat(_.nonEmpty).map("/tmp/" + _ + ".json")
  } yield DaemonState(trace, focusNodeId, traceFile)

  // Command generators
  val listCommand: Gen[DaemonCommand.List] = for {
    showValues <- Gen.oneOf(true, false)
  } yield DaemonCommand.List(showValues)

  val explainCommand: Gen[DaemonCommand.Explain] = for {
    nodeId <- Gen.option(nodeId)
  } yield DaemonCommand.Explain(nodeId)

  val findCommand: Gen[DaemonCommand.Find] = for {
    value <- jsonValue
  } yield DaemonCommand.Find(value)

  def nodeIdCommand(state: DaemonState): Gen[NodeId] =
    if (state.trace.nodes.isEmpty) nodeId
    else Gen.oneOf(state.trace.nodes.keys.toSeq)

  def depsCommand(state: DaemonState): Gen[DaemonCommand.Deps] =
    nodeIdCommand(state).map(id => DaemonCommand.Deps(id))

  def usagesCommand(state: DaemonState): Gen[DaemonCommand.Usages] =
    nodeIdCommand(state).map(id => DaemonCommand.Usages(id))

  def focusCommand(state: DaemonState): Gen[DaemonCommand.Focus] =
    nodeIdCommand(state).map(id => DaemonCommand.Focus(id))

  def pathCommand(state: DaemonState): Gen[DaemonCommand.Path] =
    nodeIdCommand(state).map(id => DaemonCommand.Path(id))

  def valueCommand(state: DaemonState): Gen[DaemonCommand.Value] =
    nodeIdCommand(state).map(id => DaemonCommand.Value(id))

  def sourceCommand(state: DaemonState): Gen[DaemonCommand.Source] =
    nodeIdCommand(state).map(id => DaemonCommand.Source(id))

  def snippetCommand(state: DaemonState): Gen[DaemonCommand.Snippet] = for {
    id <- nodeIdCommand(state)
    context <- Gen.choose(1, 5)
  } yield DaemonCommand.Snippet(id, context)

  def evalCommand(state: DaemonState): Gen[DaemonCommand.Eval] = for {
    id <- nodeIdCommand(state)
    expr <- Gen.alphaNumStr.suchThat(_.nonEmpty)
  } yield DaemonCommand.Eval(id, expr)

  val statusCommand: Gen[DaemonCommand.Status.type] = Gen.const(DaemonCommand.Status)

  val shutdownCommand: Gen[DaemonCommand.Shutdown.type] = Gen.const(DaemonCommand.Shutdown)

  val unfocusCommand: Gen[DaemonCommand.Unfocus.type] = Gen.const(DaemonCommand.Unfocus)

  // Any command (for a given state)
  def anyCommand(state: DaemonState): Gen[DaemonCommand] = Gen.frequency(
    (2, listCommand),
    (2, explainCommand),
    (2, findCommand),
    (2, depsCommand(state)),
    (2, usagesCommand(state)),
    (2, focusCommand(state)),
    (1, unfocusCommand),
    (2, pathCommand(state)),
    (2, valueCommand(state)),
    (2, sourceCommand(state)),
    (2, snippetCommand(state)),
    (2, evalCommand(state)),
    (1, statusCommand),
    (1, shutdownCommand)
  )

  // State-independent command generator (for JSON serialization tests)
  val genDaemonCommand: Gen[DaemonCommand] = Gen.frequency(
    (2, listCommand),
    (2, explainCommand),
    (2, findCommand),
    (2, genNodeId.map(DaemonCommand.Deps(_))),
    (2, genNodeId.map(DaemonCommand.Usages(_))),
    (2, genNodeId.map(DaemonCommand.Focus(_))),
    (1, unfocusCommand),
    (2, genNodeId.map(DaemonCommand.Path(_))),
    (2, genNodeId.map(DaemonCommand.Value(_))),
    (2, genNodeId.map(DaemonCommand.Source(_))),
    (2, for {
      id <- genNodeId
      context <- Gen.choose(1, 10)
    } yield DaemonCommand.Snippet(id, context)),
    (2, for {
      id <- genNodeId
      expr <- Gen.alphaNumStr.suchThat(_.nonEmpty)
    } yield DaemonCommand.Eval(id, expr)),
    (1, statusCommand),
    (1, shutdownCommand)
  )

  // Arbitraries
  implicit val arbNodeId: Arbitrary[NodeId] = Arbitrary(genNodeId)
  implicit val arbSourceLocation: Arbitrary[SourceLocation] = Arbitrary(genSourceLocation)
  implicit val arbSourceType: Arbitrary[SourceType] = Arbitrary(sourceType)
  implicit val arbDaemonState: Arbitrary[DaemonState] = Arbitrary(daemonState)
  implicit val arbDaemonCommand: Arbitrary[DaemonCommand] = Arbitrary(genDaemonCommand)
}
