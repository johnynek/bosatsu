package dev.bosatsu.daemon

import dev.bosatsu._
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.analysis.{DerivationGraph, ProvenanceNode, ProvenanceAnalyzer}
import dev.bosatsu.rankn.Type
import cats.data.Validated

/**
 * Generates ProvenanceTrace from Bosatsu source files.
 *
 * This bridges the static analysis (ProvenanceAnalyzer producing DerivationGraph)
 * with the daemon infrastructure (ProvenanceTrace for interactive debugging).
 */
object TraceGenerator {

  /**
   * Generate a ProvenanceTrace from a Bosatsu source file.
   *
   * @param source The source code string
   * @param sourceFile The source file path (for display)
   * @param defaultPackageName Default package name if not specified in source
   * @return Either an error message or the generated trace
   */
  def generateFromSource(
      source: String,
      sourceFile: String,
      defaultPackageName: PackageName = PackageName.parts("Trace")
  ): Either[String, ProvenanceTrace] = {
    given HasRegion[Declaration] = HasRegion.instance(_.region)

    val locationMap = LocationMap(source)

    // Parse the full file (including package declaration)
    Parser.parse(Package.parser(Some(defaultPackageName)), source) match {
      case Validated.Invalid(errs) =>
        Left(s"Parse error: ${errs.toList.map(_.showContext(LocationMap.Colorize.None).render(80)).mkString("\n")}")

      case Validated.Valid((_, pkg)) =>
        val actualPackageName = pkg.name
        val statements = pkg.program

        // Type check to get TypedExpr - use inferBodyUnopt to preserve original structure
        // (inferBody applies TypedExprNormalization which inlines constants)
        Package.inferBodyUnopt(actualPackageName, Nil, statements).strictToValidated match {
          case Validated.Invalid(typeErrs) =>
            val packMap = Map((actualPackageName, (locationMap, source)))
            val errMsg = typeErrs.toList.map { err =>
              err.message(packMap, LocationMap.Colorize.None)
            }.mkString("\n")
            Left(s"Type error: $errMsg")

          case Validated.Valid((_, program)) =>
            // Analyze all bindings, passing package name for intra-package dependency tracking
            val bindings = program.lets.map { case (name, _, expr) => (name, expr) }
            val graph = ProvenanceAnalyzer.analyzeBindings(bindings, Some(actualPackageName))

            // Convert to ProvenanceTrace
            Right(graphToTrace(graph, sourceFile, locationMap)(using HasRegion.instance(_.region)))
        }
    }
  }

  /**
   * Convert a DerivationGraph to a ProvenanceTrace.
   */
  def graphToTrace[T](
      graph: DerivationGraph[T],
      sourceFile: String,
      locationMap: LocationMap
  )(implicit hr: HasRegion[T]): ProvenanceTrace = {
    // Find the result node (first root)
    val resultId = graph.roots.headOption.getOrElse(0L)

    // Convert each node
    val traceNodes = graph.nodes.map { case (id, node) =>
      val nodeId = NodeId(s"n$id")
      val deps = graph.directDependencies(id).map(d => NodeId(s"n$d")).toList
      val uses = graph.directUsages(id).map(u => NodeId(s"n$u")).toList

      nodeId -> TraceNode(
        id = nodeId,
        value = extractValue(node),
        source = extractSourceType(node),
        bindingName = extractBindingName(node),
        location = extractLocation(node, locationMap)(using hr),
        dependencies = deps,
        usedBy = uses
      )
    }

    ProvenanceTrace(
      nodes = traceNodes.toMap,
      resultNodeId = NodeId(s"n$resultId"),
      sourceFile = sourceFile
    )
  }

  /**
   * Extract a string representation of the value from a ProvenanceNode.
   */
  private def extractValue[T](node: ProvenanceNode[T]): String = {
    node.expr match {
      case lit: TypedExpr.Literal[_] =>
        lit.lit match {
          case Lit.Integer(i) => i.toString
          case Lit.Str(s) => s"\"$s\""
          case Lit.Chr(c) => s"'$c'"
        }
      case local: TypedExpr.Local[_] =>
        s"<local:${local.name.asString}>"
      case global: TypedExpr.Global[_] =>
        s"<global:${global.name.asString}>"
      case _: TypedExpr.App[_] =>
        "<application>"
      case l: TypedExpr.Let[_] =>
        s"<let:${l.arg.asString}>"
      case _: TypedExpr.AnnotatedLambda[_] =>
        "<lambda>"
      case _: TypedExpr.Match[_] =>
        "<match>"
      case _: TypedExpr.Generic[_] =>
        "<generic>"
      case _: TypedExpr.Annotation[_] =>
        "<annotation>"
    }
  }

  /**
   * Extract the SourceType from a ProvenanceNode.
   */
  private def extractSourceType[T](node: ProvenanceNode[T]): SourceType = {
    node.expr match {
      case _: TypedExpr.Literal[_] =>
        SourceType.Literal

      case local: TypedExpr.Local[_] =>
        SourceType.Binding(local.name.asString)

      case global: TypedExpr.Global[_] =>
        SourceType.Operation(
          global.pack.asString,
          global.name.asString
        )

      case app: TypedExpr.App[_] =>
        app.fn match {
          case global: TypedExpr.Global[_] =>
            SourceType.Operation(
              global.pack.asString,
              global.name.asString
            )
          case _ =>
            SourceType.Pure(Some(s"application"))
        }

      case l: TypedExpr.Let[_] =>
        SourceType.Binding(l.arg.asString)

      case _: TypedExpr.AnnotatedLambda[_] =>
        SourceType.Pure(Some("lambda"))

      case _: TypedExpr.Match[_] =>
        SourceType.Pure(Some("match"))

      case _: TypedExpr.Generic[_] =>
        SourceType.Pure(Some("generic"))

      case _: TypedExpr.Annotation[_] =>
        SourceType.Pure(Some("annotation"))
    }
  }

  /**
   * Extract the binding name if this node represents a named binding.
   */
  private def extractBindingName[T](node: ProvenanceNode[T]): Option[String] = {
    node.expr match {
      case l: TypedExpr.Let[_] => Some(l.arg.asString)
      case local: TypedExpr.Local[_] => Some(local.name.asString)
      case _ => None
    }
  }

  /**
   * Extract source location from a ProvenanceNode.
   */
  private def extractLocation[T](
      node: ProvenanceNode[T],
      locationMap: LocationMap
  )(implicit hr: HasRegion[T]): Option[SourceLocation] = {
    val region = node.region
    locationMap.toLineCol(region.start).map { case (line, col) =>
      SourceLocation(
        file = "<source>", // Would be set from actual file path
        line = line,
        column = col
      )
    }
  }
}
