package dev.bosatsu.analysis

import dev.bosatsu.{Region, LocationMap, TypedExpr, HasRegion}
import org.typelevel.paiges.Doc

/**
 * Maps provenance nodes to source code locations.
 *
 * Given a Region from the AST, this provides human-readable
 * line/column information and source code snippets for debugging
 * and provenance explanation.
 */
class SourceMapper(locationMap: LocationMap) {

  /**
   * A source location with line/column information and a snippet.
   *
   * Lines and columns are 1-indexed for display purposes.
   */
  case class SourceLocation(
      line: Int,
      column: Int,
      endLine: Int,
      endColumn: Int,
      snippet: String
  ) {
    def isSingleLine: Boolean = line == endLine

    def span: String =
      if (isSingleLine) s"$line:$column-$endColumn"
      else s"$line:$column-$endLine:$endColumn"

    override def toString: String = s"[$span] $snippet"
  }

  /**
   * Convert a Region to a SourceLocation with line/column info.
   */
  def locate(region: Region): Option[SourceLocation] = {
    for {
      (startLine, startCol) <- locationMap.toLineCol(region.start)
      // Use end - 1 to get the last character position (not one past)
      (endLine, endCol) <- locationMap.toLineCol(math.max(region.end - 1, region.start))
    } yield {
      val snippet = locationMap
        .showRegion(region, 0, LocationMap.Colorize.None)
        .map(_.render(80))
        .getOrElse("")
        .trim

      // toLineCol returns 0-indexed, convert to 1-indexed for display
      SourceLocation(
        startLine + 1,
        startCol + 1,
        endLine + 1,
        endCol + 1,
        snippet
      )
    }
  }

  /**
   * Format a provenance node with its source location and dependencies.
   */
  def formatNode[T](
      node: ProvenanceNode[T],
      deps: Set[ProvenanceNode[T]],
      colorize: LocationMap.Colorize = LocationMap.Colorize.None
  )(implicit hr: HasRegion[T]): Doc = {
    val locOpt = locate(node.region)
    val locStr = locOpt.map(_.span).getOrElse("unknown")

    val header = Doc.text(s"[${node.id}] ${node.nodeType} at $locStr")

    val snippet = locOpt.map { loc =>
      Doc.line + Doc.text(loc.snippet).indent(2)
    }.getOrElse(Doc.empty)

    val depList = if (deps.nonEmpty) {
      Doc.line + Doc.text("Dependencies:") + Doc.line +
        Doc.intercalate(
          Doc.line,
          deps.toList.sortBy(_.id).map { d =>
            val dLocStr = locate(d.region).map(_.span).getOrElse("unknown")
            Doc.text(s"  -> [${d.id}] ${d.nodeType} at $dLocStr")
          }
        )
    } else Doc.empty

    header + snippet + depList
  }

  /**
   * Get a brief description of a node for chain display.
   */
  def briefDescription[T](node: ProvenanceNode[T]): String = {
    node.expr match {
      case lit: TypedExpr.Literal[_] =>
        val repr = lit.lit.repr
        s"literal: ${repr.take(20)}${if (repr.length > 20) "..." else ""}"
      case local: TypedExpr.Local[_] =>
        s"local: ${local.name.sourceCodeRepr}"
      case global: TypedExpr.Global[_] =>
        s"global: ${global.pack.asString}::${global.name.sourceCodeRepr}"
      case _: TypedExpr.App[_] =>
        "application"
      case let: TypedExpr.Let[_] =>
        s"let: ${let.arg.sourceCodeRepr}"
      case lam: TypedExpr.AnnotatedLambda[_] =>
        s"lambda(${lam.args.toList.map(_._1.sourceCodeRepr).mkString(", ")})"
      case m: TypedExpr.Match[_] =>
        s"match (${m.branches.size} branches)"
      case _: TypedExpr.Generic[_] =>
        "generic"
      case ann: TypedExpr.Annotation[_] =>
        s"annotation: ${ann.coerce}"
    }
  }

  /**
   * Format a derivation chain from a node back to its leaves.
   *
   * @param chain List of (nodeId, depth) pairs from derivationChain
   * @param graph The derivation graph containing the nodes
   */
  def formatDerivationChain[T](
      chain: List[(Long, Int)],
      graph: DerivationGraph[T]
  )(implicit hr: HasRegion[T]): Doc = {
    val lines = chain.map { case (nodeId, depth) =>
      val nodeOpt = graph.nodes.get(nodeId)
      nodeOpt match {
        case Some(node) =>
          val indent = "  " * depth
          val locStr = locate(node.region).map(_.span).getOrElse("unknown")
          val brief = briefDescription(node)
          Doc.text(s"$indent[$nodeId] $brief at $locStr")
        case None =>
          Doc.text(s"[$nodeId] <unknown node>")
      }
    }
    Doc.intercalate(Doc.hardLine, lines)
  }

  /**
   * Get a human-readable explanation for why a value has its current derivation.
   */
  def explainValue[T](nodeId: Long, graph: DerivationGraph[T])(implicit hr: HasRegion[T]): Doc = {
    graph.nodes.get(nodeId) match {
      case None =>
        Doc.text(s"Unknown node: $nodeId")

      case Some(node) =>
        val chain = graph.derivationChain(nodeId)
        val header = Doc.text(s"Derivation of [$nodeId]:") + Doc.hardLine
        val chainDoc = formatDerivationChain(chain, graph)

        val leaves = graph.findLeaves.intersect(chain.map(_._1).toSet)
        val leafDocs = if (leaves.nonEmpty) {
          Doc.hardLine + Doc.hardLine +
            Doc.text(s"Leaf values (${leaves.size}):") + Doc.hardLine +
            Doc.intercalate(
              Doc.hardLine,
              leaves.toList.sorted.flatMap { leafId =>
                graph.nodes.get(leafId).map { leaf =>
                  val locStr = locate(leaf.region).map(_.span).getOrElse("unknown")
                  Doc.text(s"  [$leafId] ${leaf.nodeType} at $locStr")
                }
              }
            )
        } else Doc.empty

        header + chainDoc + leafDocs
    }
  }
}

object SourceMapper {

  /**
   * Create a SourceMapper from source code string.
   */
  def fromSource(source: String): SourceMapper =
    new SourceMapper(LocationMap(source))
}

/**
 * Enhanced source mapper that provides bidirectional mapping between
 * source code positions and TypedExpr nodes.
 *
 * This enables:
 * - Finding the expression at a given source location (for IDE hover/click)
 * - Getting the original source text for an expression (for "Why?" explanations)
 */
class ExpressionMapper[T] private (
    val sourceMapper: SourceMapper,
    val source: String,
    expressionIndex: Map[Region, TypedExpr[T]]
)(implicit hr: HasRegion[T]) {

  /**
   * Find the smallest expression containing the given source position.
   *
   * @param line 1-indexed line number
   * @param col 1-indexed column number
   * @return The most specific expression at that location, or None
   */
  def expressionAt(line: Int, col: Int): Option[TypedExpr[T]] = {
    // Convert 1-indexed line/col to 0-indexed offset
    val offsetOpt = sourceMapper.locate(Region(0, source.length)).flatMap { _ =>
      // Find offset by converting back from line/col
      val lineIdx = line - 1
      val colIdx = col - 1
      val lines = source.split('\n')
      if (lineIdx >= 0 && lineIdx < lines.length) {
        val offset = lines.take(lineIdx).map(_.length + 1).sum + colIdx
        Some(offset)
      } else None
    }

    offsetOpt.flatMap { offset =>
      // Find smallest region containing this offset
      val containing = expressionIndex.toList.filter { case (region, _) =>
        region.start <= offset && offset < region.end
      }

      // Return the smallest (most specific) expression
      containing.minByOption { case (region, _) => region.end - region.start }.map(_._2)
    }
  }

  /**
   * Get the original source text for an expression.
   *
   * @param expr The typed expression
   * @return The source code substring, or None if region is invalid
   */
  def expressionText(expr: TypedExpr[T]): Option[String] = {
    val region = hr.region(expr.tag)
    substring(region)
  }

  /**
   * Get source text for a region.
   */
  def substring(region: Region): Option[String] = {
    if (region.start >= 0 && region.end <= source.length && region.start < region.end) {
      Some(source.substring(region.start, region.end))
    } else None
  }

  /**
   * Look up a TypedExpr by its region.
   */
  def expressionForRegion(region: Region): Option[TypedExpr[T]] =
    expressionIndex.get(region)

  /**
   * Delegate to SourceMapper for location info.
   */
  def locate(region: Region): Option[SourceMapper#SourceLocation] =
    sourceMapper.locate(region)

  /**
   * Delegate to SourceMapper for location info.
   */
  def locate(expr: TypedExpr[T]): Option[SourceMapper#SourceLocation] =
    sourceMapper.locate(hr.region(expr.tag))
}

object ExpressionMapper {
  import scala.collection.mutable

  /**
   * Build an ExpressionMapper from a TypedExpr tree.
   *
   * @param expr The root typed expression
   * @param source The original source code string
   */
  def fromTypedExpr[T: HasRegion](expr: TypedExpr[T], source: String): ExpressionMapper[T] = {
    val hr = implicitly[HasRegion[T]]
    val index = mutable.Map[Region, TypedExpr[T]]()

    def visit(e: TypedExpr[T]): Unit = {
      index(hr.region(e.tag)) = e
      e match {
        case TypedExpr.Let(_, value, in, _, _) =>
          visit(value)
          visit(in)
        case TypedExpr.AnnotatedLambda(_, body, _) =>
          visit(body)
        case TypedExpr.App(fn, args, _, _) =>
          visit(fn)
          args.toList.foreach(visit)
        case TypedExpr.Match(arg, branches, _) =>
          visit(arg)
          branches.toList.foreach { case (_, branchExpr) => visit(branchExpr) }
        case TypedExpr.Generic(_, in) =>
          visit(in)
        case TypedExpr.Annotation(term, _) =>
          visit(term)
        case _: TypedExpr.Literal[_] | _: TypedExpr.Local[_] | _: TypedExpr.Global[_] =>
          // Leaf nodes - no children
          ()
      }
    }

    visit(expr)
    new ExpressionMapper(
      new SourceMapper(LocationMap(source)),
      source,
      index.toMap
    )(using hr)
  }
}
