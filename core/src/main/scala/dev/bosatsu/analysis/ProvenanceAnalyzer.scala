package dev.bosatsu.analysis

import dev.bosatsu._
import dev.bosatsu.rankn.Type
import cats.data.State
import cats.implicits._

/**
 * Static analyzer that builds a derivation graph from a TypedExpr.
 *
 * The derivation graph tracks where every value comes from, enabling
 * "spreadsheet-style" debugging where users can click any value and
 * see its complete derivation chain.
 *
 * This is a purely static analysis - no runtime instrumentation needed.
 * Because Bosatsu is purely functional, every value has a clear origin.
 */
object ProvenanceAnalyzer {

  /**
   * Analyze a TypedExpr and build its derivation graph.
   *
   * @param expr The type-checked expression to analyze
   * @param tag Extracts the Region from the tag type
   * @return A DerivationGraph showing all value derivations with source locations
   */
  def analyze[T](expr: TypedExpr[T])(implicit tag: HasRegion[T]): DerivationGraph[T] = {
    val initialState = AnalysisState.empty[T]
    val (finalState, rootId) = analyzeExpr(expr, Map.empty).run(initialState).value
    finalState.builder.addRoot(rootId)
    finalState.builder.build()
  }

  /**
   * Analyze multiple top-level bindings.
   */
  def analyzeBindings[T](
      bindings: List[(Identifier.Bindable, TypedExpr[T])]
  )(implicit tag: HasRegion[T]): DerivationGraph[T] = {
    val initialState = AnalysisState.empty[T]

    val (finalState, _) = bindings.foldLeft((initialState, Map.empty[Identifier.Bindable, Long])) {
      case ((state, env), (name, expr)) =>
        val (newState, exprId) = analyzeExpr(expr, env).run(state).value
        newState.builder.addRoot(exprId)
        (newState, env + (name -> exprId))
    }

    finalState.builder.build()
  }

  // Internal state for the analysis
  private case class AnalysisState[T](
      builder: DerivationGraph.Builder[T]
  )

  private object AnalysisState {
    def empty[T]: AnalysisState[T] = AnalysisState(new DerivationGraph.Builder[T])
  }

  private type Analysis[T, A] = State[AnalysisState[T], A]

  private def freshId[T]: Analysis[T, Long] =
    State(s => (s, s.builder.freshId()))

  private def addNode[T](node: ProvenanceNode[T]): Analysis[T, Unit] =
    State.modify(s => { s.builder.addNode(node); s })

  private def addDep[T](from: Long, to: Long): Analysis[T, Unit] =
    State.modify(s => { s.builder.addDependency(from, to); s })

  private def addDeps[T](from: Long, tos: Iterable[Long]): Analysis[T, Unit] =
    State.modify(s => { s.builder.addDependencies(from, tos); s })

  /**
   * Analyze a TypedExpr and return the node ID representing its value.
   *
   * @param expr The expression to analyze
   * @param env Maps local variable names to their definition node IDs
   * @param tag Extracts Region from the expression's tag
   */
  private def analyzeExpr[T](
      expr: TypedExpr[T],
      env: Map[Identifier.Bindable, Long]
  )(implicit tag: HasRegion[T]): Analysis[T, Long] = {
    // Helper to recursively analyze with the same HasRegion instance
    def recurse(e: TypedExpr[T], newEnv: Map[Identifier.Bindable, Long]): Analysis[T, Long] =
      analyzeExpr(e, newEnv)(using tag)

    expr match {
      case lit: TypedExpr.Literal[T] =>
        for {
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, lit))
        } yield id

      case local: TypedExpr.Local[T] =>
        for {
          id <- freshId[T]
          definedAt = env.get(local.name)
          _ <- addNode(ProvenanceNode(id, local))
          _ <- definedAt.traverse_(defId => addDep(id, defId))
        } yield id

      case global: TypedExpr.Global[T] =>
        for {
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, global))
        } yield id

      case app: TypedExpr.App[T] =>
        for {
          fnId <- recurse(app.fn, env)
          argIds <- app.args.traverse(arg => recurse(arg, env))
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, app))
          _ <- addDep(id, fnId)
          _ <- addDeps(id, argIds.toList)
        } yield id

      case let: TypedExpr.Let[T] =>
        for {
          // For recursive lets, we need to add the binding to env BEFORE analyzing the expr
          // so self-references can be tracked
          isRec = let.recursive.isRecursive
          preId <- if (isRec) freshId[T] else State.pure[AnalysisState[T], Long](0L)
          preEnv = if (isRec) env + (let.arg -> preId) else env
          boundId <- recurse(let.expr, preEnv)
          // For recursive bindings, add dependency from the pre-allocated ID to the actual expr
          _ <- if (isRec) addDep(preId, boundId) else State.pure[AnalysisState[T], Unit](())
          // Use the bound ID (or pre-allocated ID for recursive) for the environment
          actualBoundId = if (isRec) preId else boundId
          newEnv = env + (let.arg -> actualBoundId)
          inId <- recurse(let.in, newEnv)
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, let))
          _ <- addDep(id, inId) // The let's value is the body's value
        } yield id

      case lam: TypedExpr.AnnotatedLambda[T] =>
        for {
          // Create IDs for parameters and add nodes for them (they're inputs, not derived)
          // We use the lambda's tag for parameter nodes since parameters don't have their own TypedExpr
          paramIds <- lam.args.traverse { case (name, tpe) =>
            for {
              paramId <- freshId[T]
              // Add a node for the parameter - use a Local reference with the param's type
              paramNode = ProvenanceNode(paramId, TypedExpr.Local[T](name, tpe, lam.tag))
              _ <- addNode(paramNode)
            } yield (name, paramId)
          }
          // Extend environment with parameters
          newEnv = env ++ paramIds.toList.toMap
          bodyId <- recurse(lam.expr, newEnv)
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, lam))
          _ <- addDep(id, bodyId)
        } yield id

      case m: TypedExpr.Match[T] =>
        for {
          scrutId <- recurse(m.arg, env)
          branchIds <- m.branches.traverse { case (pattern, branchExpr) =>
            // Use Pattern.envOf to get precise types for each binding
            // After type checking, pattern vars are wrapped in Annotation with their actual type
            val bindingTypes: Map[Identifier.Bindable, Type] =
              Pattern.envOf(pattern, Map.empty[Identifier.Bindable, Type])(
                _.asInstanceOf[Identifier.Bindable]
              )
            for {
              patternEnv <- bindingTypes.toList.traverse { case (name, bindingType) =>
                for {
                  bindingId <- freshId[T]
                  // Create a node with the precise type from Pattern.envOf
                  bindingNode = ProvenanceNode(bindingId, TypedExpr.Local[T](name, bindingType, m.tag))
                  _ <- addNode(bindingNode)
                  _ <- addDep(bindingId, scrutId) // Pattern binding depends on scrutinee
                } yield (name, bindingId)
              }
              branchId <- recurse(branchExpr, env ++ patternEnv.toMap)
            } yield branchId
          }
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, m))
          _ <- addDep(id, scrutId)
          _ <- addDeps(id, branchIds.toList)
        } yield id

      case gen: TypedExpr.Generic[T] =>
        // Generic just adds type quantification, the value is the inner expression
        recurse(gen.in, env)

      case ann: TypedExpr.Annotation[T] =>
        for {
          innerId <- recurse(ann.term, env)
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, ann))
          _ <- addDep(id, innerId)
        } yield id
    }
  }
}
