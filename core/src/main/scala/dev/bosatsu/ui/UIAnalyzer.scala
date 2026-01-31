package dev.bosatsu.ui

import dev.bosatsu.{TypedExpr, Identifier, PackageName, Lit, Pattern}
import dev.bosatsu.Identifier.{Bindable, Constructor}
import dev.bosatsu.rankn.Type
import scala.collection.immutable.SortedSet

/**
 * DOM Binding Analyzer for BosatsuUI
 *
 * Extracts fine-grained DOM bindings from TypedExpr AST.
 * This is a port of burritoUI's analyze.ts adapted for Bosatsu's type system.
 *
 * The key insight: by analyzing the TypedExpr chain, we can determine exactly
 * which state paths flow into which DOM properties at compile time.
 *
 * Example:
 *   IO.read(["user", "name"]) → h("span", ..., [text(name)])
 *   produces binding: { elementId: "bosatsu-1", property: textContent, statePath: ["user", "name"] }
 *
 * Advantages over burritoUI's Effect analysis:
 * - TypedExpr has type information → can optimize binding updates
 * - Existing freeVarsSet → dependency tracking is built in
 * - Matchless IR → could analyze at IR level for more precision
 */
object UIAnalyzer {

  // ---------------------------------------------------------------------------
  // Types - matching burritoUI's DOMBinding structure
  // ---------------------------------------------------------------------------

  /**
   * Supported DOM properties for binding.
   * When state changes, we update only the specific property.
   */
  sealed trait DOMProperty derives CanEqual
  object DOMProperty {
    case object TextContent extends DOMProperty
    case object ClassName extends DOMProperty
    final case class Style(cssProperty: String) extends DOMProperty
    case object Value extends DOMProperty // for inputs
    case object Checked extends DOMProperty // for checkboxes
    case object Disabled extends DOMProperty

    def fromString(s: String): Option[DOMProperty] = s match {
      case "textContent" => Some(TextContent)
      case "className"   => Some(ClassName)
      case "value"       => Some(Value)
      case "checked"     => Some(Checked)
      case "disabled"    => Some(Disabled)
      case s if s.startsWith("style.") =>
        Some(Style(s.stripPrefix("style.")))
      case _ => None
    }

    def toJsProperty(prop: DOMProperty): String = prop match {
      case TextContent  => "textContent"
      case ClassName    => "className"
      case Style(css)   => s"style.$css"
      case Value        => "value"
      case Checked      => "checked"
      case Disabled     => "disabled"
    }
  }

  /**
   * A binding from a state path to a DOM property.
   *
   * Following documented learning: Store reference to TypedExpr, not copies of fields.
   * The expr field allows extracting additional info via pattern matching.
   */
  final case class DOMBinding[A](
      elementId: String,         // data-bosatsu-id attribute value
      property: DOMProperty,     // Which DOM property to update
      statePath: List[String],   // Path into state: ["user", "name"]
      when: Option[BranchCondition], // Condition that activates this binding (None = always active)
      transform: Option[String], // Optional JS transform expression
      sourceExpr: TypedExpr[A]   // Reference to source expression (immutable)
  ) {
    /** Backwards compatibility: true if this binding is conditional */
    def conditional: Boolean = when.isDefined
  }

  /**
   * Condition for a conditional binding.
   * Used when binding is inside a match expression over a sum type.
   *
   * @param discriminant The state path being matched (e.g., ["status"])
   * @param tag The variant tag that activates this binding (e.g., "Success")
   * @param isTotal True for wildcard/catch-all patterns that match any variant
   */
  final case class BranchCondition(
      discriminant: List[String],
      tag: String,
      isTotal: Boolean = false
  )

  /**
   * Event binding extracted from UI code.
   */
  final case class EventBinding[A](
      elementId: String,
      eventType: String,       // "click", "input", "change", etc.
      handler: TypedExpr[A],   // The handler expression
      preventDefault: Boolean,
      stopPropagation: Boolean
  )

  /**
   * Complete analysis result for a UI component.
   */
  final case class UIAnalysis[A](
      stateReads: List[List[String]],     // All state paths read
      bindings: List[DOMBinding[A]],      // State → DOM bindings
      eventHandlers: List[EventBinding[A]] // Event handlers
  )

  object UIAnalysis {
    def empty[A]: UIAnalysis[A] = UIAnalysis(Nil, Nil, Nil)

    def combine[A](a: UIAnalysis[A], b: UIAnalysis[A]): UIAnalysis[A] =
      UIAnalysis(
        a.stateReads ++ b.stateReads,
        a.bindings ++ b.bindings,
        a.eventHandlers ++ b.eventHandlers
      )
  }

  // ---------------------------------------------------------------------------
  // Analysis Context - mutable state during traversal
  // ---------------------------------------------------------------------------

  /**
   * Context maintained during TypedExpr traversal.
   * Uses State monad pattern as per documented learning.
   */
  private final class AnalysisContext[A](
      var stateReads: List[List[String]] = Nil,
      var bindings: List[DOMBinding[A]] = Nil,
      var eventHandlers: List[EventBinding[A]] = Nil,
      var idCounter: Int = 0,
      var inConditional: Boolean = false,
      // Track which bindings map to which state paths
      var bindingToPath: Map[Bindable, List[String]] = Map.empty,
      // Track current parent element ID for text bindings
      var currentParentElementId: Option[String] = None,
      // Map function names to their bodies for resolving named handler references
      var functionBodies: Map[String, TypedExpr[A]] = Map.empty,
      // Current condition for bindings inside match branches
      var currentCondition: Option[BranchCondition] = None,
      // Current discriminant path for nested pattern bindings
      var currentDiscriminant: Option[List[String]] = None
  ) {
    def freshId(): String = {
      val id = s"bosatsu-$idCounter"
      idCounter += 1
      id
    }

    def withParentElementId[T](id: String)(f: => T): T = {
      val oldId = currentParentElementId
      currentParentElementId = Some(id)
      val result = f
      currentParentElementId = oldId
      result
    }

    def recordStateRead(path: List[String]): Unit =
      stateReads = path :: stateReads

    def recordBinding(binding: DOMBinding[A]): Unit =
      bindings = binding :: bindings

    def recordEventHandler(handler: EventBinding[A]): Unit =
      eventHandlers = handler :: eventHandlers

    def trackBinding(name: Bindable, path: List[String]): Unit =
      bindingToPath = bindingToPath + (name -> path)

    def getPath(name: Bindable): Option[List[String]] =
      bindingToPath.get(name)

    def toAnalysis: UIAnalysis[A] = UIAnalysis(
      stateReads.reverse.distinct,
      bindings.reverse,
      eventHandlers.reverse
    )
  }

  // ---------------------------------------------------------------------------
  // Main Analysis Entry Point
  // ---------------------------------------------------------------------------

  /**
   * Analyze a TypedExpr that produces UI to extract DOM bindings.
   *
   * @param expr The TypedExpr to analyze (typically a render function body)
   * @return Analysis result with state reads and DOM bindings
   */
  def analyze[A](expr: TypedExpr[A]): UIAnalysis[A] = {
    val ctx = new AnalysisContext[A]()
    analyzeExpr(expr, ctx)
    ctx.toAnalysis
  }

  /**
   * Analyze a TypedExpr with pre-known state bindings.
   * Use this when state variables are defined at package level.
   *
   * @param expr The TypedExpr to analyze
   * @param stateBindings Names of bindings that are state variables
   * @return Analysis result with state reads and DOM bindings
   */
  def analyzeWithStateBindings[A](
      expr: TypedExpr[A],
      stateBindings: List[Bindable]
  ): UIAnalysis[A] = {
    val ctx = new AnalysisContext[A]()
    // Pre-register all state bindings so they can be tracked during analysis
    stateBindings.foreach { name =>
      ctx.trackBinding(name, List(name.asString))
    }
    analyzeExpr(expr, ctx)
    ctx.toAnalysis
  }

  /**
   * Analyze a TypedExpr with state bindings AND function bodies.
   * This enables automatic binding detection by resolving named function references.
   *
   * @param expr The TypedExpr to analyze (typically the view/main binding)
   * @param stateBindings Names of bindings that are state variables
   * @param functions Map of function names to their TypedExpr bodies
   * @return Analysis result with state reads and DOM bindings
   */
  def analyzeWithFunctions[A](
      expr: TypedExpr[A],
      stateBindings: List[Bindable],
      functions: Map[String, TypedExpr[A]]
  ): UIAnalysis[A] = {
    val ctx = new AnalysisContext[A]()
    // Pre-register all state bindings
    stateBindings.foreach { name =>
      ctx.trackBinding(name, List(name.asString))
    }
    // Store function bodies for resolving named handler references
    ctx.functionBodies = functions
    analyzeExpr(expr, ctx)
    ctx.toAnalysis
  }

  /**
   * Check if an expression is a state creation (state(initial)).
   * Exposed so callers can scan for state bindings before analysis.
   */
  def isStateCreationExpr[A](expr: TypedExpr[A]): Boolean =
    isStateCreation(expr)

  /**
   * Analyze a function that produces UI (e.g., render function).
   * Extracts parameter bindings as potential state paths.
   */
  def analyzeFunction[A](funcExpr: TypedExpr[A]): UIAnalysis[A] = {
    funcExpr match {
      case TypedExpr.AnnotatedLambda(params, body, _) =>
        val ctx = new AnalysisContext[A]()
        // Parameters could be state bindings - track them
        params.toList.foreach { case (name, _) =>
          ctx.trackBinding(name, List(name.asString))
        }
        analyzeExpr(body, ctx)
        ctx.toAnalysis

      case _ =>
        analyze(funcExpr)
    }
  }

  // ---------------------------------------------------------------------------
  // TypedExpr Traversal
  // ---------------------------------------------------------------------------

  /**
   * Recursively analyze TypedExpr, looking for IO operations and VNode construction.
   * Returns the state path if this expression reads from state (for tracing value flow).
   */
  private def analyzeExpr[A](expr: TypedExpr[A], ctx: AnalysisContext[A]): Option[List[String]] = {
    expr match {
      // Let binding - track the binding and analyze value and body
      case TypedExpr.Let(name, value, body, _, _) =>
        // Check if this creates a state variable: state(initial)
        if (isStateCreation(value)) {
          // This binding IS a state variable - track it by its name
          ctx.trackBinding(name, List(name.asString))
        } else {
          // Analyze value and check if it derives from state
          val valuePath = analyzeExpr(value, ctx)
          valuePath.foreach { path =>
            ctx.recordStateRead(path)
            ctx.trackBinding(name, path)
          }
        }
        analyzeExpr(body, ctx)
        // Let itself doesn't produce a state path (the body does)
        None

      // Function application - check for state reads, UI construction, or transforms
      case app: TypedExpr.App[A @unchecked] =>
        // First check if this is a read() call
        extractStateRead(app) match {
          case Some(path) =>
            ctx.recordStateRead(path)
            Some(path)
          case None =>
            // Check if this is h() - need special handling for parent element context
            app.fn match {
              case TypedExpr.Global(pack, name, _, _)
                  if isUIPackage(pack) && name.asString == "h" =>
                // Extract element bindings and set parent context for children analysis
                val args = app.args.toList
                val explicitId = if (args.length >= 2) extractExplicitId(args(1)) else None
                val elementId = explicitId.getOrElse(ctx.freshId())
                // Process props
                if (args.length >= 2) extractPropsBindings(args(1), elementId, ctx)
                // Process children WITH parent context set
                if (args.length >= 3) {
                  ctx.withParentElementId(elementId) {
                    analyzeExpr(args(2), ctx)
                  }
                }
                None

              case _ =>
                // Regular app - check for text, event handlers, etc.
                extractUIConstruction(app, ctx)
                // Analyze arguments and propagate state path through transforms
                analyzeExpr(app.fn, ctx)
                val argPaths = app.args.toList.map(arg => analyzeExpr(arg, ctx))
                // If any argument reads from state, this expression depends on state
                argPaths.flatten.headOption
            }
        }

      // Match expression - extract pattern info for conditional bindings
      case TypedExpr.Match(arg, branches, _) =>
        // Analyze the match argument first
        analyzeExpr(arg, ctx)

        // Extract the discriminant path (the state path being matched)
        val discriminant = extractDiscriminantPath(arg, ctx)

        // Save state to restore after analyzing branches
        val savedBindingToPath = ctx.bindingToPath
        val wasConditional = ctx.inConditional
        val savedCondition = ctx.currentCondition
        val savedDiscriminant = ctx.currentDiscriminant

        ctx.inConditional = true
        ctx.currentDiscriminant = discriminant

        branches.toList.foreach { case (pattern, branchExpr) =>
          // Extract variant tag from pattern (e.g., "Success", "Loading")
          val variantTag = extractVariantTag(pattern)

          // Create BranchCondition for this branch
          val condition = (discriminant, variantTag) match {
            case (Some(d), Some(t)) => Some(BranchCondition(d, t))
            case (Some(d), None) => Some(BranchCondition(d, "_", isTotal = true))
            case _ => None
          }

          // Add pattern-bound variables to context
          // e.g., Success(user) makes "user" available as ["status", "Success", "0"]
          (discriminant, variantTag) match {
            case (Some(d), Some(t)) =>
              addPatternBindings(pattern, d, t, ctx)
            case _ => ()
          }

          // Set current condition for bindings in this branch
          ctx.currentCondition = condition

          // Analyze the branch body with pattern bindings in scope
          analyzeExpr(branchExpr, ctx)

          // Restore binding context for next branch (pattern bindings are branch-local)
          ctx.bindingToPath = savedBindingToPath
        }

        // Restore all saved state
        ctx.inConditional = wasConditional
        ctx.currentCondition = savedCondition
        ctx.currentDiscriminant = savedDiscriminant
        None

      // Lambda - analyze body but don't propagate path
      case TypedExpr.AnnotatedLambda(_, body, _) =>
        analyzeExpr(body, ctx)
        None

      // Annotation wrapper
      case TypedExpr.Annotation(inner, _) =>
        analyzeExpr(inner, ctx)

      // Generic wrapper
      case TypedExpr.Generic(_, inner) =>
        analyzeExpr(inner, ctx)

      // Local variable reference - return tracked path if any
      case TypedExpr.Local(name, _, _) =>
        ctx.getPath(name)

      // Global reference - could be a state variable at package level
      case TypedExpr.Global(_, name, _, _) =>
        name.toBindable.flatMap(ctx.getPath)

      // Literal - no state dependency
      case TypedExpr.Literal(_, _, _) =>
        None
    }
  }

  // ---------------------------------------------------------------------------
  // State read Detection
  // ---------------------------------------------------------------------------

  /**
   * Check if an expression is a state read operation and extract the state identifier.
   *
   * Supports both patterns:
   *   - IO.read(["path", "to", "state"]) - old IO pattern
   *   - read(stateVar) - new Bosatsu/UI pattern
   *
   * For Bosatsu/UI, the state variable name becomes the path.
   */
  private def extractStateRead[A](expr: TypedExpr[A]): Option[List[String]] = {
    expr match {
      case TypedExpr.App(fn, args, _, _) =>
        fn match {
          // Check for Bosatsu/UI::read(state)
          case TypedExpr.Global(pack, name, _, _)
              if isUIPackage(pack) && name.asString == "read" =>
            // Extract state identifier from first argument
            args.head match {
              case TypedExpr.Local(stateName, _, _) =>
                // State variable name becomes the path
                Some(List(stateName.asString))
              case TypedExpr.Global(_, stateName, _, _) =>
                // Global state reference
                Some(List(stateName.asString))
              case _ =>
                None
            }

          // Check for IO.read or Bosatsu/IO::read (legacy pattern)
          case TypedExpr.Global(pack, name, _, _)
              if isIOPackage(pack) && name.asString == "read" =>
            // Extract path from first argument
            args.head match {
              case TypedExpr.App(listFn, listArgs, _, _) =>
                // List constructor: [a, b, c]
                val path = listArgs.toList.flatMap(extractStringLiteral)
                if (path.nonEmpty) Some(path) else None
              case other =>
                // Could be a variable holding the path
                None
            }
          case _ => None
        }
      case _ => None
    }
  }

  private def isIOPackage(pack: PackageName): Boolean =
    pack.asString == "Bosatsu/IO" || pack.asString == "IO"

  /**
   * Check if an expression is a state creation: state(initial)
   * Handles annotation and generic wrappers around the function.
   */
  private def isStateCreation[A](expr: TypedExpr[A]): Boolean = {
    expr match {
      case TypedExpr.App(fn, _, _, _) =>
        isStateFunction(fn)
      case _ => false
    }
  }

  /**
   * Check if a function expression is the UI state constructor.
   * Unwraps annotations and generics to find the actual function.
   */
  private def isStateFunction[A](fn: TypedExpr[A]): Boolean = {
    fn match {
      case TypedExpr.Global(pack, name, _, _) =>
        isUIPackage(pack) && name.asString == "state"
      case TypedExpr.Annotation(inner, _) =>
        isStateFunction(inner)
      case TypedExpr.Generic(_, inner) =>
        isStateFunction(inner)
      case _ => false
    }
  }

  private def extractStringLiteral[A](expr: TypedExpr[A]): Option[String] =
    expr match {
      case TypedExpr.Literal(Lit.Str(s), _, _) => Some(s)
      case _ => None
    }

  // ---------------------------------------------------------------------------
  // UI Construction Detection (h, text, etc.)
  // ---------------------------------------------------------------------------

  /**
   * Extract UI construction patterns from function applications.
   * Looks for text() and event handler patterns.
   * Note: h() is handled specially in analyzeExpr to set parent context before analyzing children.
   */
  private def extractUIConstruction[A](
      app: TypedExpr.App[A],
      ctx: AnalysisContext[A]
  ): Unit = {
    app.fn match {
      case TypedExpr.Global(pack, name, _, _) if isUIPackage(pack) =>
        name.asString match {
          case "h" =>
            // h() is now handled in analyzeExpr to properly set parent context
            // Don't duplicate processing here
            ()

          case "text" =>
            // text(content) - text node, parent will handle binding
            extractTextBinding(app, ctx)

          case "on_click" =>
            // on_click(handler) - extract event handler
            extractEventHandler(app, "click", ctx)

          case "on_input" =>
            // on_input(handler) - extract event handler
            extractEventHandler(app, "input", ctx)

          case "on_change" =>
            // on_change(handler) - extract event handler
            extractEventHandler(app, "change", ctx)

          case _ =>
            // Other UI functions
            ()
        }

      case _ =>
        // Not a recognized UI function
        ()
    }
  }

  private def isUIPackage(pack: PackageName): Boolean =
    pack.asString == "Bosatsu/UI" || pack.asString == "UI"

  /**
   * Extract explicit ID from props list.
   * Props are typically [("key", "value"), ...] tuples in Bosatsu list format.
   */
  private def extractExplicitId[A](propsExpr: TypedExpr[A]): Option[String] = {
    // Walk the list looking for ("id", "...") tuple
    extractIdFromPropsList(propsExpr)
  }

  /**
   * Recursively walk a Bosatsu list looking for an ("id", value) tuple.
   * List structure: NonEmptyList(head, tail) or EmptyList
   */
  private def extractIdFromPropsList[A](expr: TypedExpr[A]): Option[String] = {
    expr match {
      // NonEmptyList(head, tail)
      case TypedExpr.App(fn, args, _, _) =>
        val fnName = getFunctionName(fn)
        if (fnName == "NonEmptyList" || fnName == "Cons") {
          val argsList = args.toList
          if (argsList.length >= 2) {
            val head = argsList.head
            val tail = argsList(1)
            // Check if head is ("id", value) tuple
            extractPropTuple(head) match {
              case Some(("id", value)) => Some(value)
              case _ => extractIdFromPropsList(tail) // Continue with tail
            }
          } else None
        } else {
          None
        }

      // EmptyList or annotation wrapper
      case TypedExpr.Annotation(inner, _) =>
        extractIdFromPropsList(inner)

      case TypedExpr.Global(_, name, _, _) if name.asString == "EmptyList" =>
        None

      case _ => None
    }
  }

  /**
   * Get the function name from a possibly-wrapped function expression.
   */
  private def getFunctionName[A](fn: TypedExpr[A]): String = {
    fn match {
      case TypedExpr.Global(_, name, _, _) => name.asString
      case TypedExpr.Annotation(inner, _) => getFunctionName(inner)
      case TypedExpr.Generic(_, inner) => getFunctionName(inner)
      case _ => ""
    }
  }

  /**
   * Unwrap Annotation and Generic wrappers to get the underlying expression.
   */
  private def unwrapFunction[A](expr: TypedExpr[A]): TypedExpr[A] = {
    expr match {
      case TypedExpr.Annotation(inner, _) => unwrapFunction(inner)
      case TypedExpr.Generic(_, inner) => unwrapFunction(inner)
      case other => other
    }
  }

  /**
   * Extract a (key, value) tuple from a TypedExpr.
   * Handles Tuple2(key, value) pattern with possible annotation wrappers.
   */
  private def extractPropTuple[A](expr: TypedExpr[A]): Option[(String, String)] = {
    expr match {
      case TypedExpr.App(fn, args, _, _) =>
        val fnName = getFunctionName(fn)
        if (fnName == "Tuple2" || fnName == "") {
          // Tuple construction: Tuple2(key, value) or direct (key, value)
          val argsList = args.toList
          if (argsList.length >= 2) {
            val key = extractStringLiteralDeep(argsList.head)
            val value = extractStringLiteralDeep(argsList(1))
            (key, value) match {
              case (Some(k), Some(v)) => Some((k, v))
              case _ => None
            }
          } else None
        } else None
      case _ => None
    }
  }

  /**
   * Extract string literal, unwrapping annotations if needed.
   */
  private def extractStringLiteralDeep[A](expr: TypedExpr[A]): Option[String] = {
    expr match {
      case TypedExpr.Literal(Lit.Str(s), _, _) => Some(s)
      case TypedExpr.Annotation(inner, _) => extractStringLiteralDeep(inner)
      case TypedExpr.Generic(_, inner) => extractStringLiteralDeep(inner)
      case _ => None
    }
  }

  /**
   * Extract bindings from props object.
   * Also handles on_click handlers to create automatic className bindings.
   */
  private def extractPropsBindings[A](
      propsExpr: TypedExpr[A],
      elementId: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    // Props is typically a list containing tuples and on_click handlers
    // Process the props list to find event handlers
    extractEventHandlersFromProps(propsExpr, elementId, ctx)

    // Props would be a struct/record construction
    // For now, we look for specific patterns
    propsExpr match {
      case TypedExpr.App(_, args, _, _) =>
        // Could be record construction - analyze each arg
        args.toList.foreach { arg =>
          arg match {
            case TypedExpr.Local(name, _, tag) =>
              ctx.getPath(name).foreach { path =>
                // This local is bound to a state path
                // Determine property from context (would need more info)
                ctx.recordBinding(DOMBinding(
                  elementId = elementId,
                  property = DOMProperty.ClassName, // Default, would need inference
                  statePath = path,
                  when = ctx.currentCondition,
                  transform = None,
                  sourceExpr = arg
                ))
              }
            case _ => ()
          }
        }
      case _ => ()
    }
  }

  /**
   * Extract event handlers from a props list and create bindings.
   * Props is typically: [("class", "x"), ("id", "y"), on_click(handler)]
   */
  private def extractEventHandlersFromProps[A](
      propsExpr: TypedExpr[A],
      elementId: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    propsExpr match {
      // NonEmptyList(head, tail) or list construction
      case TypedExpr.App(fn, args, _, _) =>
        val fnName = getFunctionName(fn)
        if (fnName == "NonEmptyList" || fnName == "Cons") {
          val argsList = args.toList
          if (argsList.length >= 2) {
            // Check head for on_click
            extractSingleEventHandler(argsList.head, elementId, ctx)
            // Recurse on tail
            extractEventHandlersFromProps(argsList(1), elementId, ctx)
          }
        } else {
          // Could be on_click directly or other App
          extractSingleEventHandler(propsExpr, elementId, ctx)
          // Also check nested args
          args.toList.foreach { arg =>
            extractEventHandlersFromProps(arg, elementId, ctx)
          }
        }

      case TypedExpr.Annotation(inner, _) =>
        extractEventHandlersFromProps(inner, elementId, ctx)

      case TypedExpr.Generic(_, inner) =>
        extractEventHandlersFromProps(inner, elementId, ctx)

      case _ => ()
    }
  }

  /**
   * Check if an expression is an on_click/on_input/on_change call
   * and extract the handler with the element ID.
   */
  private def extractSingleEventHandler[A](
      expr: TypedExpr[A],
      elementId: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    expr match {
      case TypedExpr.App(fn, args, _, _) =>
        fn match {
          case TypedExpr.Global(pack, name, _, _) if isUIPackage(pack) =>
            val eventType = name.asString match {
              case "on_click" => Some("click")
              case "on_input" => Some("input")
              case "on_change" => Some("change")
              case _ => None
            }
            eventType.foreach { evtType =>
              val handler = args.head
              extractEventHandlerWithElement(handler, evtType, elementId, ctx)
            }
          case _ => ()
        }
      case _ => ()
    }
  }

  /**
   * Extract text binding from a text() call.
   * Creates a DOM binding if the text content depends on state.
   * Uses the parent element ID if available (text nodes are children of elements).
   */
  private def extractTextBinding[A](
      app: TypedExpr.App[A],
      ctx: AnalysisContext[A]
  ): Unit = {
    val contentExpr = app.args.head

    // Trace the content expression to see if it reads from state
    val statePath = traceStateDependency(contentExpr, ctx)

    statePath.foreach { path =>
      ctx.recordStateRead(path)

      // Use parent element ID if available, otherwise generate a new one
      // Text nodes should use their parent element's ID for binding updates
      val elementId = ctx.currentParentElementId.getOrElse(ctx.freshId())

      // Extract transform function if content goes through a transformation
      val transform = extractTransform(contentExpr)

      ctx.recordBinding(DOMBinding(
        elementId = elementId,
        property = DOMProperty.TextContent,
        statePath = path,
        when = ctx.currentCondition,
        transform = transform,
        sourceExpr = contentExpr
      ))
    }
  }

  /**
   * Trace an expression to find if it depends on state.
   * Returns the state path if found.
   */
  private def traceStateDependency[A](
      expr: TypedExpr[A],
      ctx: AnalysisContext[A]
  ): Option[List[String]] = {
    expr match {
      // Direct state read: read(state)
      case app: TypedExpr.App[A @unchecked] =>
        extractStateRead(app) match {
          case Some(path) => Some(path)
          case None =>
            // Function application - check if any argument reads state
            // This handles transforms like int_to_String(read(count))
            app.args.toList.flatMap(arg => traceStateDependency(arg, ctx)).headOption
        }

      // Local variable that's tracked
      case TypedExpr.Local(name, _, _) =>
        ctx.getPath(name)

      // Global variable that's tracked
      case TypedExpr.Global(_, name, _, _) =>
        name.toBindable.flatMap(ctx.getPath)

      // Annotation wrapper
      case TypedExpr.Annotation(inner, _) =>
        traceStateDependency(inner, ctx)

      // Generic wrapper
      case TypedExpr.Generic(_, inner) =>
        traceStateDependency(inner, ctx)

      case _ => None
    }
  }

  /**
   * Extract transform function name from an expression.
   * e.g., int_to_String(x) -> Some("_int_to_String")
   */
  private def extractTransform[A](expr: TypedExpr[A]): Option[String] = {
    expr match {
      case TypedExpr.App(fn, _, _, _) =>
        fn match {
          case TypedExpr.Global(pack, name, _, _) =>
            // Check if this is a known transform function
            val nameStr = name.asString
            if (nameStr == "int_to_String" || nameStr == "int_to_string") {
              Some("_int_to_String")
            } else if (nameStr.endsWith("_to_String") || nameStr.endsWith("_to_string")) {
              Some(s"_$nameStr")
            } else {
              None // Not a transform, it might be read() itself
            }
          case _ => None
        }
      case _ => None
    }
  }

  // ---------------------------------------------------------------------------
  // Pattern Matching Analysis (for conditional rendering)
  // ---------------------------------------------------------------------------

  /**
   * Extract the variant tag from a pattern.
   * Returns the constructor name for PositionalStruct patterns.
   *
   * @param pattern The pattern from a match branch
   * @return Some(tag) for constructor patterns, None for wildcards/variables
   */
  private def extractVariantTag(
      pattern: Pattern[(PackageName, Constructor), Type]
  ): Option[String] = {
    pattern match {
      case Pattern.PositionalStruct((_, cons), _) =>
        Some(cons.asString)
      case Pattern.Named(_, inner) =>
        extractVariantTag(inner)
      case Pattern.Annotation(inner, _) =>
        extractVariantTag(inner)
      case Pattern.WildCard | Pattern.Var(_) =>
        None // Total match - no specific tag
      case Pattern.Literal(lit) =>
        // Literal patterns - use the literal value as the "tag"
        Some(lit.unboxToAny.toString)
      case Pattern.Union(_, _) =>
        None // Complex - not supported for now
      case _ =>
        None
    }
  }

  /**
   * Extract the discriminant path from a match argument expression.
   * This is the state path being matched against.
   *
   * @param expr The argument expression in TypedExpr.Match
   * @param ctx The analysis context
   * @return Some(path) if the expression is a simple state reference
   */
  private def extractDiscriminantPath[A](
      expr: TypedExpr[A],
      ctx: AnalysisContext[A]
  ): Option[List[String]] = {
    expr match {
      // Local variable - check if tracked, otherwise use name
      case TypedExpr.Local(name, _, _) =>
        ctx.getPath(name).orElse(Some(List(name.asString)))

      // Global variable - use the name
      case TypedExpr.Global(_, name, _, _) =>
        Some(List(name.asString))

      // State read: read(stateVar)
      case TypedExpr.App(fn, args, _, _) =>
        fn match {
          case TypedExpr.Global(pack, name, _, _)
              if isUIPackage(pack) && name.asString == "read" =>
            // Extract state path from the read argument
            args.head match {
              case TypedExpr.Local(stateName, _, _) =>
                Some(List(stateName.asString))
              case TypedExpr.Global(_, stateName, _, _) =>
                Some(List(stateName.asString))
              case _ => None
            }
          case _ => None
        }

      // Unwrap annotations and generics
      case TypedExpr.Annotation(inner, _) =>
        extractDiscriminantPath(inner, ctx)
      case TypedExpr.Generic(_, inner) =>
        extractDiscriminantPath(inner, ctx)

      case _ => None // Complex expressions not supported
    }
  }

  /**
   * Add pattern-bound variables to the binding context.
   * When matching `Success(user)`, `user` becomes available as a state path.
   *
   * @param pattern The pattern being matched
   * @param discriminant The discriminant path (e.g., ["status"])
   * @param tag The variant tag (e.g., "Success")
   * @param ctx The analysis context
   */
  private def addPatternBindings[A](
      pattern: Pattern[(PackageName, Constructor), Type],
      discriminant: List[String],
      tag: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    pattern match {
      case Pattern.PositionalStruct(_, params) =>
        // Each parameter is bound to discriminant ++ [tag, fieldIndex]
        params.zipWithIndex.foreach { case (param, idx) =>
          param match {
            case Pattern.Var(name) =>
              val path = discriminant ++ List(tag, idx.toString)
              ctx.trackBinding(name, path)
            case Pattern.Named(name, inner) =>
              val path = discriminant ++ List(tag, idx.toString)
              ctx.trackBinding(name, path)
              // Recursively process inner pattern
              addPatternBindings(inner, path, "", ctx)
            case Pattern.Annotation(inner, _) =>
              addPatternBindings(inner, discriminant, tag, ctx)
            case _ => ()
          }
        }

      case Pattern.Named(name, inner) =>
        // Named pattern binds the whole matched value
        ctx.trackBinding(name, discriminant)
        extractVariantTag(inner).foreach { innerTag =>
          addPatternBindings(inner, discriminant, innerTag, ctx)
        }

      case Pattern.Annotation(inner, _) =>
        addPatternBindings(inner, discriminant, tag, ctx)

      case _ => ()
    }
  }

  // ---------------------------------------------------------------------------
  // Event Handler Detection
  // ---------------------------------------------------------------------------

  /**
   * Extract event handler from on_click/on_input/on_change calls.
   */
  private def extractEventHandler[A](
      app: TypedExpr.App[A],
      eventType: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    // The first argument is the handler function
    val handler = app.args.head
    val elementId = ctx.freshId()

    ctx.recordEventHandler(EventBinding(
      elementId = elementId,
      eventType = eventType,
      handler = handler,
      preventDefault = eventType == "click", // Prevent default for clicks
      stopPropagation = false
    ))
  }

  /**
   * Extract event handler with a known element ID (for automatic binding detection).
   * Creates className bindings only for checkbox-style elements (ID ends in "-checkbox").
   *
   * This is selective because:
   * - Checkbox elements: toggle state writes should update className (checked/unchecked)
   * - Button elements: state writes update a display elsewhere, not the button's className
   */
  private def extractEventHandlerWithElement[A](
      handler: TypedExpr[A],
      eventType: String,
      elementId: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    ctx.recordEventHandler(EventBinding(
      elementId = elementId,
      eventType = eventType,
      handler = handler,
      preventDefault = eventType == "click",
      stopPropagation = false
    ))

    // Only create className bindings for checkbox-style elements
    // (identified by ID ending in "-checkbox")
    if (elementId.endsWith("-checkbox")) {
      val stateWrites = extractStateWrites(handler, ctx)
      stateWrites.foreach { stateName =>
        ctx.recordBinding(DOMBinding(
          elementId = elementId,
          property = DOMProperty.ClassName,
          statePath = List(stateName),
          when = None, // Checkbox bindings are always active (not conditional)
          transform = None,
          sourceExpr = handler
        ))
      }
    }
  }

  /**
   * Extract state variable names that a handler writes to.
   * Looks for write(stateVar, value) patterns in the expression.
   */
  private def extractStateWrites[A](
      expr: TypedExpr[A],
      ctx: AnalysisContext[A]
  ): List[String] = {
    expr match {
      // write(state, value) call - unwrap Annotation/Generic to find the function
      case TypedExpr.App(fn, args, _, _) =>
        val unwrappedFn = unwrapFunction(fn)
        unwrappedFn match {
          case TypedExpr.Global(pack, name, _, _)
              if isUIPackage(pack) && name.asString == "write" =>
            // First arg is the state variable - also need to unwrap it
            val stateNames = unwrapFunction(args.head) match {
              case TypedExpr.Local(stateName, _, _) =>
                List(stateName.asString)
              case TypedExpr.Global(_, stateName, _, _) =>
                List(stateName.asString)
              case _ =>
                Nil
            }
            // Also check other args for nested writes
            stateNames ++ args.toList.tail.flatMap(arg => extractStateWrites(arg, ctx))

          case TypedExpr.Global(_, _, _, _) =>
            // Other global function - check all arguments for state writes
            args.toList.flatMap(arg => extractStateWrites(arg, ctx))

          case _ =>
            // Other function type - check all arguments for state writes
            args.toList.flatMap(arg => extractStateWrites(arg, ctx))
        }

      // Lambda body - analyze the body
      case TypedExpr.AnnotatedLambda(_, body, _) =>
        extractStateWrites(body, ctx)

      // Let expression - check both value and body
      case TypedExpr.Let(_, value, body, _, _) =>
        extractStateWrites(value, ctx) ++ extractStateWrites(body, ctx)

      // Match expression - check all branches
      case TypedExpr.Match(_, branches, _) =>
        branches.toList.flatMap { case (_, branchExpr) =>
          extractStateWrites(branchExpr, ctx)
        }

      // Local reference to a function - look it up in function bodies
      case TypedExpr.Local(name, _, _) =>
        // The handler might be a reference to a named function
        ctx.functionBodies.get(name.asString) match {
          case Some(funcBody) => extractStateWrites(funcBody, ctx)
          case None => Nil
        }

      // Global reference - could be a toggle function defined at package level
      case TypedExpr.Global(_, name, _, _) =>
        // Look up the function body in the provided map
        ctx.functionBodies.get(name.asString) match {
          case Some(funcBody) => extractStateWrites(funcBody, ctx)
          case None => Nil
        }

      // Annotation/Generic wrappers
      case TypedExpr.Annotation(inner, _) =>
        extractStateWrites(inner, ctx)

      case TypedExpr.Generic(_, inner) =>
        extractStateWrites(inner, ctx)

      case _ => Nil
    }
  }

  // ---------------------------------------------------------------------------
  // Utilities
  // ---------------------------------------------------------------------------

  /**
   * Create a binding map for efficient runtime lookups.
   * Maps state path (as "." joined string) to array of bindings.
   */
  def createBindingMap[A](bindings: List[DOMBinding[A]]): Map[String, List[DOMBinding[A]]] = {
    bindings.groupBy(b => b.statePath.mkString("."))
  }

  /**
   * Get all unique state paths from an analysis.
   */
  def getStatePaths[A](analysis: UIAnalysis[A]): List[List[String]] = {
    analysis.stateReads.distinct
  }

  /**
   * Generate JavaScript object representation of bindings.
   * Used for embedding in generated code.
   *
   * Each binding now includes a `when` clause for conditional rendering:
   * - when: null for unconditional bindings
   * - when: { discriminant: ["path"], tag: "VariantName", isTotal: false } for conditional
   */
  def bindingsToJs[A](bindings: List[DOMBinding[A]]): String = {
    val entries = createBindingMap(bindings).map { case (pathKey, bs) =>
      val bindingArrays = bs.map { b =>
        val whenJs = b.when match {
          case Some(cond) =>
            val discPath = cond.discriminant.map(s => s""""$s"""").mkString("[", ", ", "]")
            s"""{"discriminant": $discPath, "tag": "${cond.tag}", "isTotal": ${cond.isTotal}}"""
          case None =>
            "null"
        }
        val props = List(
          s""""elementId": "${b.elementId}"""",
          s""""property": "${DOMProperty.toJsProperty(b.property)}"""",
          s""""when": $whenJs"""
        ) ++ b.transform.map(t => s""""transform": "$t"""")
        s"{${props.mkString(", ")}}"
      }
      s""""$pathKey": [${bindingArrays.mkString(", ")}]"""
    }
    s"{${entries.mkString(",\n  ")}}"
  }
}
