package dev.bosatsu.analysis

import dev.bosatsu._
import cats.data.{NonEmptyList, State}
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
  def analyze[T](expr: TypedExpr[T])(implicit tag: HasRegion[T]): DerivationGraph = {
    val initialState = AnalysisState.empty
    val (finalState, rootId) = analyzeExpr(expr, Map.empty).run(initialState).value
    finalState.builder.addRoot(rootId)
    finalState.builder.build()
  }

  /**
   * Analyze multiple top-level bindings.
   */
  def analyzeBindings[T](
      bindings: List[(Identifier.Bindable, TypedExpr[T])]
  )(implicit tag: HasRegion[T]): DerivationGraph = {
    val initialState = AnalysisState.empty

    val (finalState, _) = bindings.foldLeft((initialState, Map.empty[Identifier.Bindable, Long])) {
      case ((state, env), (name, expr)) =>
        val (newState, exprId) = analyzeExpr(expr, env).run(state).value
        newState.builder.addRoot(exprId)
        (newState, env + (name -> exprId))
    }

    finalState.builder.build()
  }

  // Internal state for the analysis
  private case class AnalysisState(
      builder: DerivationGraph.Builder
  )

  private object AnalysisState {
    def empty: AnalysisState = AnalysisState(new DerivationGraph.Builder)
  }

  private type Analysis[A] = State[AnalysisState, A]

  private def freshId: Analysis[Long] =
    State(s => (s, s.builder.freshId()))

  private def addNode(node: ProvenanceNode): Analysis[Unit] =
    State.modify(s => { s.builder.addNode(node); s })

  private def addDep(from: Long, to: Long): Analysis[Unit] =
    State.modify(s => { s.builder.addDependency(from, to); s })

  private def addDeps(from: Long, tos: Iterable[Long]): Analysis[Unit] =
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
  )(implicit tag: HasRegion[T]): Analysis[Long] = {
    val region = tag.region(expr.tag)
    // Helper to recursively analyze with the same HasRegion instance
    def recurse(e: TypedExpr[T], newEnv: Map[Identifier.Bindable, Long]): Analysis[Long] =
      analyzeExpr(e, newEnv)(using tag)

    expr match {
      case TypedExpr.Literal(lit, _, _) =>
        for {
          id <- freshId
          _ <- addNode(ProvenanceNode.Literal(id, region, lit.repr))
        } yield id

      case TypedExpr.Local(name, _, _) =>
        for {
          id <- freshId
          definedAt = env.get(name)
          _ <- addNode(ProvenanceNode.LocalVar(id, region, name, definedAt))
          _ <- definedAt.traverse_(defId => addDep(id, defId))
        } yield id

      case TypedExpr.Global(pack, name, _, _) =>
        for {
          id <- freshId
          _ <- addNode(ProvenanceNode.GlobalRef(id, region, pack.asString, name))
        } yield id

      case TypedExpr.App(fn, args, _, _) =>
        for {
          fnId <- recurse(fn, env)
          argIds <- args.traverse(arg => recurse(arg, env))
          id <- freshId
          _ <- addNode(ProvenanceNode.Application(id, region, fnId, argIds))
          _ <- addDep(id, fnId)
          _ <- addDeps(id, argIds.toList)
        } yield id

      case TypedExpr.Let(name, boundExpr, inExpr, _, _) =>
        for {
          boundId <- recurse(boundExpr, env)
          // Extend environment with the new binding
          newEnv = env + (name -> boundId)
          inId <- recurse(inExpr, newEnv)
          id <- freshId
          _ <- addNode(ProvenanceNode.LetBinding(id, region, name, boundId, inId))
          _ <- addDep(id, inId) // The let's value is the body's value
        } yield id

      case TypedExpr.AnnotatedLambda(args, body, _) =>
        for {
          // Create placeholder IDs for parameters
          paramIds <- args.traverse { case (name, _) =>
            for {
              paramId <- freshId
              _ <- addNode(ProvenanceNode.LocalVar(paramId, region, name, None))
            } yield (name, paramId)
          }
          // Extend environment with parameters
          newEnv = env ++ paramIds.toList.toMap
          bodyId <- recurse(body, newEnv)
          id <- freshId
          _ <- addNode(ProvenanceNode.Lambda(id, region, args.map(_._1), bodyId))
          _ <- addDep(id, bodyId)
        } yield id

      case TypedExpr.Match(scrutinee, branches, _) =>
        for {
          scrutId <- recurse(scrutinee, env)
          branchResults <- branches.traverse { case (pattern, branchExpr) =>
            // Extract names bound by the pattern
            val patternBindings = pattern.names
            // Create placeholder IDs for pattern bindings
            val patternEnv = patternBindings.map(name => name -> -1L).toMap
            for {
              branchId <- recurse(branchExpr, env ++ patternEnv)
            } yield ProvenanceNode.MatchBranch(pattern.toString, branchId)
          }
          id <- freshId
          _ <- addNode(ProvenanceNode.Match(id, region, scrutId, branchResults))
          _ <- addDep(id, scrutId)
          _ <- addDeps(id, branchResults.toList.map(_.bodyId))
        } yield id

      case TypedExpr.Generic(_, inner) =>
        // Generic just adds type quantification, the value is the inner expression
        recurse(inner, env)

      case TypedExpr.Annotation(inner, tpe) =>
        for {
          innerId <- recurse(inner, env)
          id <- freshId
          _ <- addNode(ProvenanceNode.Annotation(
            id,
            region,
            innerId,
            tpe.toString
          ))
          _ <- addDep(id, innerId)
        } yield id
    }
  }
}

/**
 * Typeclass for extracting Region from a tag type.
 */
trait HasRegion[T] {
  def region(t: T): Region
}

object HasRegion {
  def apply[T](implicit ev: HasRegion[T]): HasRegion[T] = ev

  def instance[T](f: T => Region): HasRegion[T] = new HasRegion[T] {
    def region(t: T): Region = f(t)
  }

  // Common instances
  implicit val regionHasRegion: HasRegion[Region] = instance(identity)

  implicit val declarationHasRegion: HasRegion[Declaration] = instance { decl =>
    decl.region
  }
}
