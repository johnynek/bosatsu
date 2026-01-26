package dev.bosatsu.analysis

import dev.bosatsu.{Region, Identifier}
import cats.data.NonEmptyList

/**
 * A node in the provenance derivation graph.
 *
 * Each node represents a value in the computation and tracks:
 * - Where in the source code it was defined (Region)
 * - What other nodes it depends on (for "where did this come from?" queries)
 * - What nodes use this value (for "where is this used?" queries)
 *
 * The provenance graph enables "spreadsheet-style" debugging where
 * users can click any value and see its complete derivation chain.
 */
sealed trait ProvenanceNode {
  def id: Long
  def region: Region
  def nodeType: String
}

object ProvenanceNode {

  /**
   * A literal value from source code.
   * e.g., `42`, `"hello"`, `True`
   */
  final case class Literal(
      id: Long,
      region: Region,
      repr: String
  ) extends ProvenanceNode {
    val nodeType: String = "literal"
  }

  /**
   * A reference to a local variable.
   * Tracks where the variable was defined.
   */
  final case class LocalVar(
      id: Long,
      region: Region,
      name: Identifier.Bindable,
      definedAt: Option[Long]
  ) extends ProvenanceNode {
    val nodeType: String = "local"
  }

  /**
   * A reference to a global (top-level or imported) binding.
   */
  final case class GlobalRef(
      id: Long,
      region: Region,
      packageName: String,
      name: Identifier
  ) extends ProvenanceNode {
    val nodeType: String = "global"
  }

  /**
   * A function application.
   * Tracks the function and all arguments.
   */
  final case class Application(
      id: Long,
      region: Region,
      functionId: Long,
      argumentIds: NonEmptyList[Long]
  ) extends ProvenanceNode {
    val nodeType: String = "application"
  }

  /**
   * A let binding.
   * Tracks the bound expression and the body where it's used.
   */
  final case class LetBinding(
      id: Long,
      region: Region,
      name: Identifier.Bindable,
      boundExprId: Long,
      bodyId: Long
  ) extends ProvenanceNode {
    val nodeType: String = "let"
  }

  /**
   * A lambda expression.
   * Tracks the body and parameter names.
   */
  final case class Lambda(
      id: Long,
      region: Region,
      params: NonEmptyList[Identifier.Bindable],
      bodyId: Long
  ) extends ProvenanceNode {
    val nodeType: String = "lambda"
  }

  /**
   * A pattern match expression.
   * Tracks the scrutinee and which branch was taken.
   */
  final case class Match(
      id: Long,
      region: Region,
      scrutineeId: Long,
      branches: NonEmptyList[MatchBranch]
  ) extends ProvenanceNode {
    val nodeType: String = "match"
  }

  /**
   * A single branch in a pattern match.
   */
  final case class MatchBranch(
      pattern: String,
      bodyId: Long
  )

  /**
   * An if-then-else expression (desugared match on Bool).
   */
  final case class IfThenElse(
      id: Long,
      region: Region,
      conditionId: Long,
      thenId: Long,
      elseId: Long
  ) extends ProvenanceNode {
    val nodeType: String = "if"
  }

  /**
   * A struct construction.
   */
  final case class StructConstruction(
      id: Long,
      region: Region,
      structName: String,
      fieldIds: List[(Identifier.Bindable, Long)]
  ) extends ProvenanceNode {
    val nodeType: String = "struct"
  }

  /**
   * A type annotation (does not change the value, but good for provenance).
   */
  final case class Annotation(
      id: Long,
      region: Region,
      exprId: Long,
      annotationType: String
  ) extends ProvenanceNode {
    val nodeType: String = "annotation"
  }
}
