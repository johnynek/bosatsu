package dev.bosatsu.service

import dev.bosatsu._
import dev.bosatsu.Identifier.Bindable
import cats.data.NonEmptyList

/**
 * Analyzes Bosatsu code to find external operations (I/O).
 *
 * External operations are method calls on interface parameters.
 * For example, in:
 * {{{
 * def get_user(db: DB, user_id: String) -> User:
 *   db.get("users", user_id)
 * }}}
 *
 * The operation is `db.get` which is an external operation on interface `DB`.
 */
object ServiceAnalyzer {

  /**
   * Analyze a single handler expression.
   */
  def analyzeHandler(
    name: String,
    sourceFile: String,
    expr: TypedExpr[Any],
    config: BatchConfig
  ): ServiceAnalysis = {
    // Extract operations from the expression
    val operations = extractOperations(expr, config)

    // Group batchable operations
    val batchGroups = groupBatchableOperations(operations, config)

    val totalQueries = operations.size
    val batchedQueries = batchGroups.flatMap(_.operations).size
    val queriesSaved = batchGroups.map(_.queriesSaved).sum

    ServiceAnalysis(
      handlerName = name,
      sourceFile = sourceFile,
      operations = operations,
      batchGroups = batchGroups,
      canBatch = batchGroups.nonEmpty,
      totalQueries = totalQueries,
      batchedQueries = batchedQueries,
      queriesSaved = queriesSaved
    )
  }

  /**
   * Extract external operations from an expression.
   *
   * Looks for patterns like:
   * - App(App(Local(interface), method), args) for method calls
   */
  private def extractOperations(
    expr: TypedExpr[Any],
    config: BatchConfig
  ): List[ServiceOperation] = {
    expr match {
      case TypedExpr.App(fn, args, _, _) =>
        // Check if this is a method call on an interface
        // For simplicity, just check the first arg
        val argOps = args.toList.flatMap(a => extractOperations(a, config))
        extractMethodCall(fn, args, config) match {
          case Some(op) => op :: argOps
          case None => extractOperations(fn, config) ++ argOps
        }

      case TypedExpr.Let(_, value, body, _, _) =>
        extractOperations(value, config) ++ extractOperations(body, config)

      case TypedExpr.Match(scrutinee, branches, _) =>
        extractOperations(scrutinee, config) ++
          branches.toList.flatMap { case (_, e) => extractOperations(e, config) }

      case TypedExpr.AnnotatedLambda(_, body, _) =>
        extractOperations(body, config)

      case TypedExpr.Generic(_, inner) =>
        extractOperations(inner, config)

      case _ =>
        Nil
    }
  }

  /**
   * Try to extract a method call from a function application.
   */
  private def extractMethodCall(
    fn: TypedExpr[Any],
    args: NonEmptyList[TypedExpr[Any]],
    config: BatchConfig
  ): Option[ServiceOperation] = {
    fn match {
      // Pattern: interface.method(args)
      // In Bosatsu this would be App(Local(interface), [method_name_literal])
      case TypedExpr.Local(interface, _, _) =>
        // Check if first arg is a string literal (method name)
        args.head match {
          case TypedExpr.Literal(Lit.Str(method), _, _) =>
            val kind = classifyMethod(method, config)
            val batchable = config.batchableMethods.contains(method)
            val batchMethod = config.batchableMethods.get(method)
            Some(ServiceOperation(interface.asString, method, kind, batchable, batchMethod))
          case _ => None
        }

      // Pattern: App(interface, method)(args)
      case TypedExpr.App(TypedExpr.Local(interface, _, _), methodArgs, _, _) =>
        methodArgs.head match {
          case TypedExpr.Literal(Lit.Str(method), _, _) =>
            val kind = classifyMethod(method, config)
            val batchable = config.batchableMethods.contains(method)
            val batchMethod = config.batchableMethods.get(method)
            Some(ServiceOperation(interface.asString, method, kind, batchable, batchMethod))
          case _ => None
        }

      case _ => None
    }
  }

  /**
   * Classify a method as read, write, or unknown.
   */
  private def classifyMethod(method: String, config: BatchConfig): OperationKind = {
    if (config.readMethods.contains(method)) OperationKind.Read
    else if (config.writeMethods.contains(method)) OperationKind.Write
    else OperationKind.Unknown
  }

  /**
   * Group batchable operations together.
   */
  private def groupBatchableOperations(
    operations: List[ServiceOperation],
    config: BatchConfig
  ): List[BatchGroup] = {
    operations
      .filter(_.batchable)
      .groupBy(op => (op.interface, op.method))
      .filter(_._2.size > 1) // Only group if there are multiple
      .map { case ((interface, method), ops) =>
        val batchMethod = ops.head.batchMethod.getOrElse(method + "Many")
        BatchGroup(
          interface = interface,
          method = method,
          batchMethod = batchMethod,
          operations = ops,
          queriesSaved = ops.size - 1 // N queries become 1
        )
      }
      .toList
  }
}
