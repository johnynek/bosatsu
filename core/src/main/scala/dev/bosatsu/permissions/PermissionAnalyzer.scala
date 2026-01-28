package dev.bosatsu.permissions

import cats.data.{Chain, NonEmptyList}
import cats.implicits._
import dev.bosatsu.{Identifier, Matchless, PackageName}
import dev.bosatsu.Identifier.Bindable

/**
 * Static analyzer for extracting permission requirements from Matchless IR.
 *
 * This analyzes function calls to identify what permissions are required
 * before execution. Permissions can be:
 *
 * 1. Static - known at compile time (most common)
 * 2. Dynamic - depend on runtime values (e.g., '{doc.ownerId}')
 * 3. Conditional - depend on control flow
 *
 * The analyzer traverses the expression tree and:
 * - Identifies calls to known permission-requiring functions
 * - Propagates permissions through function calls
 * - Handles conditional branches (both branches' permissions are required)
 */
object PermissionAnalyzer {

  /**
   * Permission annotations for known external functions.
   *
   * This maps package/function pairs to their required permissions.
   * These would typically be loaded from configuration or generated
   * from service definitions.
   */
  type PermissionAnnotations = Map[(PackageName, Bindable), PermissionConfig]

  /**
   * Result of analyzing an expression for permissions.
   */
  case class AnalysisResult(
      staticRequirements: List[Permission],
      dynamicRequirements: List[PermissionTemplate],
      conditionalRequirements: List[(String, List[Permission])],
      calledFunctions: Set[(PackageName, Bindable)]
  ) {
    def merge(other: AnalysisResult): AnalysisResult =
      AnalysisResult(
        staticRequirements = (staticRequirements ++ other.staticRequirements).distinct,
        dynamicRequirements = (dynamicRequirements ++ other.dynamicRequirements).distinct,
        conditionalRequirements = conditionalRequirements ++ other.conditionalRequirements,
        calledFunctions = calledFunctions ++ other.calledFunctions
      )

    def toManifest(name: String): PermissionManifest =
      PermissionManifest(
        name = name,
        staticPermissions = staticRequirements,
        dynamicPermissions = dynamicRequirements,
        conditionalPermissions = conditionalRequirements
      )

    def isEmpty: Boolean =
      staticRequirements.isEmpty &&
        dynamicRequirements.isEmpty &&
        conditionalRequirements.isEmpty
  }

  object AnalysisResult {
    val empty: AnalysisResult = AnalysisResult(Nil, Nil, Nil, Set.empty)

    def static(perms: List[Permission]): AnalysisResult =
      AnalysisResult(perms, Nil, Nil, Set.empty)

    def calledFunction(pkg: PackageName, name: Bindable): AnalysisResult =
      AnalysisResult(Nil, Nil, Nil, Set((pkg, name)))

    implicit val analysisResultMonoid: cats.Monoid[AnalysisResult] =
      new cats.Monoid[AnalysisResult] {
        def empty: AnalysisResult = AnalysisResult.empty
        def combine(x: AnalysisResult, y: AnalysisResult): AnalysisResult = x.merge(y)
      }
  }

  /**
   * Analyze a Matchless expression for permission requirements.
   *
   * @param expr The expression to analyze
   * @param annotations Known permission annotations for external functions
   * @return Analysis result with all discovered permissions
   */
  def analyze[A](
      expr: Matchless.Expr[A],
      annotations: PermissionAnnotations
  ): AnalysisResult = {
    analyzeExpr(expr, annotations)
  }

  private def analyzeExpr[A](
      expr: Matchless.Expr[A],
      annotations: PermissionAnnotations
  ): AnalysisResult = {
    import Matchless._

    expr match {
      case Global(_, pkg, name) =>
        // Check if this global has permission annotations
        val config = annotations.getOrElse((pkg, name), PermissionConfig.empty)
        val result = AnalysisResult.calledFunction(pkg, name)
        if (config.isEmpty) result
        else result.merge(AnalysisResult.static(config.requires))

      case Local(_) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) | Literal(_) =>
        AnalysisResult.empty

      case Lambda(captures, _, _, body) =>
        val captureResults = captures.map(analyzeExpr(_, annotations))
        val bodyResult = analyzeExpr(body, annotations)
        captureResults.foldLeft(bodyResult)(_.merge(_))

      case App(fn, args) =>
        val fnResult = analyzeExpr(fn, annotations)
        val argResults = args.toList.map(analyzeExpr(_, annotations))
        (fnResult :: argResults).reduce(_.merge(_))

      case Let(_, value, body) =>
        val valueResult = analyzeExpr(value, annotations)
        val bodyResult = analyzeExpr(body, annotations)
        valueResult.merge(bodyResult)

      case LetMut(_, span) =>
        analyzeExpr(span, annotations)

      case If(cond, thenE, elseE) =>
        val condResult = analyzeBoolExpr(cond, annotations)
        val thenResult = analyzeExpr(thenE, annotations)
        val elseResult = analyzeExpr(elseE, annotations)
        // Both branches' permissions are required (conservative analysis)
        condResult.merge(thenResult).merge(elseResult)

      case Always(cond, result) =>
        val condResult = analyzeBoolExpr(cond, annotations)
        val resultExpr = analyzeExpr(result, annotations)
        condResult.merge(resultExpr)

      case GetEnumElement(e, _, _, _) =>
        analyzeExpr(e, annotations)

      case GetStructElement(e, _, _) =>
        analyzeExpr(e, annotations)

      case PrevNat(of) =>
        analyzeExpr(of, annotations)

      // ConsExpr types (MakeEnum, MakeStruct, ZeroNat, SuccNat) are values, not expressions with subexpressions
      case MakeEnum(_, _, _) | MakeStruct(_) | ZeroNat | SuccNat =>
        AnalysisResult.empty

      case WhileExpr(cond, effect, _) =>
        val condResult = analyzeBoolExpr(cond, annotations)
        val effectResult = analyzeExpr(effect, annotations)
        condResult.merge(effectResult)
    }
  }

  private def analyzeBoolExpr[A](
      expr: Matchless.BoolExpr[A],
      annotations: PermissionAnnotations
  ): AnalysisResult = {
    import Matchless._

    expr match {
      case EqualsLit(e, _) =>
        analyzeExpr(e, annotations)

      case EqualsNat(e, _) =>
        analyzeExpr(e, annotations)

      case And(e1, e2) =>
        val r1 = analyzeBoolExpr(e1, annotations)
        val r2 = analyzeBoolExpr(e2, annotations)
        r1.merge(r2)

      case CheckVariant(e, _, _, _) =>
        analyzeExpr(e, annotations)

      case MatchString(arg, _, _, _) =>
        analyzeExpr(arg, annotations)

      case SetMut(_, e) =>
        analyzeExpr(e, annotations)

      case TrueConst =>
        AnalysisResult.empty

      case LetBool(_, value, inExpr) =>
        val valueResult = analyzeExpr(value, annotations)
        val inResult = analyzeBoolExpr(inExpr, annotations)
        valueResult.merge(inResult)

      case LetMutBool(_, span) =>
        analyzeBoolExpr(span, annotations)
    }
  }

  /**
   * Analyze multiple bindings and build a permission manifest for each.
   */
  def analyzeBindings[A](
      bindings: List[(Bindable, Matchless.Expr[A])],
      annotations: PermissionAnnotations
  ): Map[Bindable, AnalysisResult] = {
    bindings.map { case (name, expr) =>
      name -> analyze(expr, annotations)
    }.toMap
  }

  /**
   * Extract all unique permissions required by a set of expressions.
   */
  def extractAllPermissions[A](
      exprs: List[Matchless.Expr[A]],
      annotations: PermissionAnnotations
  ): List[Permission] = {
    exprs
      .map(analyze(_, annotations))
      .flatMap(_.staticRequirements)
      .distinct
  }

  /**
   * Check if an expression requires any permissions.
   */
  def requiresPermissions[A](
      expr: Matchless.Expr[A],
      annotations: PermissionAnnotations
  ): Boolean = {
    !analyze(expr, annotations).isEmpty
  }

  /**
   * Infer permission kind from function name patterns.
   *
   * This provides default permission inference for common patterns:
   * - get*, find*, query* -> resource:read
   * - create*, add*, insert* -> resource:create
   * - update*, modify*, set* -> resource:update
   * - delete*, remove* -> resource:delete
   */
  def inferPermissionFromName(
      resource: String,
      functionName: String
  ): Option[Permission] = {
    val name = functionName.toLowerCase
    val action = if (name.startsWith("get") || name.startsWith("find") || name.startsWith("query") || name.startsWith("list")) {
      Some("read")
    } else if (name.startsWith("create") || name.startsWith("add") || name.startsWith("insert")) {
      Some("create")
    } else if (name.startsWith("update") || name.startsWith("modify") || name.startsWith("set")) {
      Some("update")
    } else if (name.startsWith("delete") || name.startsWith("remove")) {
      Some("delete")
    } else {
      None
    }

    action.map(a => Permission.simple(resource, a))
  }
}
