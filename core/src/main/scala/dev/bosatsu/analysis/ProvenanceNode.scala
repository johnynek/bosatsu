package dev.bosatsu.analysis

import dev.bosatsu.{Region, TypedExpr, HasRegion}

/**
 * A node in the provenance derivation graph.
 *
 * Simply wraps a TypedExpr with an ID for graph edges.
 * The TypedExpr is immutable so we just store a reference -
 * no need to copy any data out of it.
 *
 * @param id Unique identifier for this node in the graph
 * @param expr Reference to the immutable AST node
 */
final case class ProvenanceNode[T](
    id: Long,
    expr: TypedExpr[T]
) {
  /** Get the region from the expression's tag */
  def region(implicit hr: HasRegion[T]): Region = hr.region(expr.tag)

  /** Node type derived from the expression variant */
  def nodeType: String = expr match {
    case _: TypedExpr.Literal[_]         => "literal"
    case _: TypedExpr.Local[_]           => "local"
    case _: TypedExpr.Global[_]          => "global"
    case _: TypedExpr.App[_]             => "application"
    case _: TypedExpr.Let[_]             => "let"
    case _: TypedExpr.AnnotatedLambda[_] => "lambda"
    case _: TypedExpr.Match[_]           => "match"
    case _: TypedExpr.Generic[_]         => "generic"
    case _: TypedExpr.Annotation[_]      => "annotation"
  }
}
