package dev.bosatsu.analysis

import dev.bosatsu._
import dev.bosatsu.tool.Output
import dev.bosatsu.IorMethods.IorExtension
import org.typelevel.paiges.Doc
import cats.data.Validated

/**
 * CLI support for provenance analysis commands.
 *
 * Provides functionality for:
 * - `provenance analyze <file>` - Build and display derivation graph
 * - `provenance explain <file> <node-id>` - Trace value origin
 *
 * Designed to integrate with Bosatsu's existing MainModule pattern.
 */
object ProvenanceCLI {

  /**
   * Analyze a Bosatsu source file and return the provenance graph as Doc output.
   *
   * @param source The source code string
   * @param packageName The package name for the source
   * @return Either an error message or the formatted provenance graph
   */
  def analyzeSource(
      source: String,
      packageName: PackageName = PackageName.parts("Provenance")
  ): Either[String, (DerivationGraph, Doc)] = {
    given HasRegion[Declaration] = HasRegion.instance(_.region)

    val locationMap = LocationMap(source)
    val mapper = new SourceMapper(locationMap)

    // Parse the source
    Parser.parse(Statement.parser, source) match {
      case Validated.Invalid(errs) =>
        Left(s"Parse error: ${errs.toList.map(_.showContext(LocationMap.Colorize.None).render(80)).mkString("\n")}")

      case Validated.Valid((_, statements)) =>
        // Type check to get TypedExpr
        Package.inferBody(packageName, Nil, statements).strictToValidated match {
          case Validated.Invalid(errs) =>
            val packMap = Map((packageName, (locationMap, source)))
            val errMsg = errs.toList.map { err =>
              err.message(packMap, LocationMap.Colorize.None)
            }.mkString("\n")
            Left(s"Type error: $errMsg")

          case Validated.Valid(prog) =>
            // Analyze all bindings
            val graph = ProvenanceAnalyzer.analyzeBindings(
              prog.lets.map { case (name, _, expr) => (name, expr) }
            )

            // Format the graph
            val doc = formatGraph(graph, mapper)
            Right((graph, doc))
        }
    }
  }

  /**
   * Explain the derivation of a specific node in the graph.
   */
  def explainNode(
      source: String,
      nodeId: Long,
      packageName: PackageName = PackageName.parts("Provenance")
  ): Either[String, Doc] = {
    analyzeSource(source, packageName).flatMap { case (graph, _) =>
      graph.nodes.get(nodeId) match {
        case None =>
          Left(s"Node $nodeId not found in graph. Available nodes: ${graph.nodes.keys.toList.sorted.mkString(", ")}")

        case Some(_) =>
          val locationMap = LocationMap(source)
          val mapper = new SourceMapper(locationMap)
          Right(mapper.explainValue(nodeId, graph))
      }
    }
  }

  /**
   * Format a derivation graph for display.
   */
  def formatGraph(graph: DerivationGraph, mapper: SourceMapper): Doc = {
    val header = Doc.text(s"Derivation Graph (${graph.nodes.size} nodes)") +
      Doc.hardLine + Doc.text("=" * 40) + Doc.hardLine + Doc.hardLine

    // Group nodes by type
    val byType = graph.nodes.values.toList.groupBy(_.nodeType)

    val sections = byType.toList.sortBy(_._1).map { case (nodeType, nodes) =>
      val sectionHeader = Doc.text(s"$nodeType (${nodes.size}):") + Doc.hardLine

      val nodesDocs = nodes.sortBy(_.id).map { node =>
        val deps = graph.directDependencies(node.id).flatMap(graph.nodes.get)
        mapper.formatNode(node, deps)
      }

      sectionHeader + Doc.intercalate(Doc.hardLine + Doc.hardLine, nodesDocs)
    }

    val body = Doc.intercalate(Doc.hardLine + Doc.hardLine, sections)

    // Summary
    val roots = graph.findRoots
    val leaves = graph.findLeaves
    val summary = Doc.hardLine + Doc.hardLine +
      Doc.text("=" * 40) + Doc.hardLine +
      Doc.text(s"Summary:") + Doc.hardLine +
      Doc.text(s"  Roots (entry points): ${roots.size} - ${roots.toList.sorted.mkString(", ")}") + Doc.hardLine +
      Doc.text(s"  Leaves (primitives): ${leaves.size} - ${leaves.toList.sorted.mkString(", ")}")

    header + body + summary
  }

  /**
   * Create an Output for the provenance analysis result.
   */
  def toOutput[Path](doc: Doc, outputPath: Option[Path]): Output[Path] =
    Output.Basic(doc, outputPath)

  /**
   * Quick analysis for testing - returns just the graph.
   */
  def quickAnalyze(source: String): Either[String, DerivationGraph] =
    analyzeSource(source).map(_._1)
}
