package dev.bosatsu.service

import dev.bosatsu.Identifier.Bindable

/**
 * Protocol types for Bosatsu service handlers.
 *
 * A "service handler" is a Bosatsu function that takes interface parameters
 * (like DB, Cache, API) and performs external operations through them.
 *
 * Example:
 * {{{
 * def get_user(db: DB, user_id: String) -> User:
 *   db.get("users", user_id)
 * }}}
 */

/**
 * Represents an external operation (method call on an interface).
 */
case class ServiceOperation(
  interface: String,
  method: String,
  kind: OperationKind,
  batchable: Boolean,
  batchMethod: Option[String] = None
) derives CanEqual

/**
 * Classification of operation effects.
 */
sealed trait OperationKind derives CanEqual
object OperationKind {
  case object Read extends OperationKind
  case object Write extends OperationKind
  case object Unknown extends OperationKind
}

/**
 * A group of operations that can be batched together.
 */
case class BatchGroup(
  interface: String,
  method: String,
  batchMethod: String,
  operations: List[ServiceOperation],
  queriesSaved: Int
) derives CanEqual

/**
 * Result of analyzing a service handler.
 */
case class ServiceAnalysis(
  handlerName: String,
  sourceFile: String,
  operations: List[ServiceOperation],
  batchGroups: List[BatchGroup],
  canBatch: Boolean,
  totalQueries: Int,
  batchedQueries: Int,
  queriesSaved: Int
) derives CanEqual {
  def batchingEfficiency: String = {
    if (totalQueries > 0)
      f"${queriesSaved.toDouble / totalQueries * 100}%.0f%%"
    else
      "0%"
  }
}

/**
 * Configuration for batching operations.
 */
case class BatchConfig(
  batchableMethods: Map[String, String] = Map(
    "get" -> "getMany",
    "fetch" -> "fetchMany"
  ),
  readMethods: Set[String] = Set("get", "fetch", "find", "query", "read", "load"),
  writeMethods: Set[String] = Set("set", "put", "save", "write", "insert", "update", "delete")
) derives CanEqual

object BatchConfig {
  val default: BatchConfig = BatchConfig()
}

/**
 * Handler parameter information.
 */
case class HandlerParam(
  name: Bindable,
  isInterface: Boolean,
  interfaceMethods: Set[String] = Set.empty
) derives CanEqual

/**
 * Compiled handler ready for execution.
 */
case class CompiledHandler(
  name: String,
  params: List[HandlerParam],
  jsCode: String,
  analysis: ServiceAnalysis
) derives CanEqual

/**
 * Result of executing a handler.
 */
case class HandlerResult(
  value: String,  // JSON string of result
  provenance: Option[String] = None  // Optional provenance trace JSON
) derives CanEqual

// Note: ServiceCommand was removed - CLI uses ServiceAction types directly

/**
 * Deployment target for build command.
 */
sealed trait BuildTarget derives CanEqual
object BuildTarget {
  case object Standalone extends BuildTarget
  case object Vercel extends BuildTarget
  case object AwsLambda extends BuildTarget
}

/**
 * Service response for HTTP endpoints.
 */
sealed trait ServiceResponse derives CanEqual

object ServiceResponse {
  case class Success(
    handler: String,
    result: String,  // JSON
    provenance: Option[String] = None
  ) extends ServiceResponse

  case class Error(
    handler: String,
    error: String,
    code: Int = 500
  ) extends ServiceResponse
}

/**
 * Build result containing generated code.
 */
case class BuildResult(
  handlers: List[String],
  jsCode: String,
  target: BuildTarget
) derives CanEqual
