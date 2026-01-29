package dev.bosatsu.ui

import dev.bosatsu.{TypedExpr, Identifier, PackageName, Lit}
import dev.bosatsu.Identifier.Bindable
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
      elementId: String,        // data-bosatsu-id attribute value
      property: DOMProperty,    // Which DOM property to update
      statePath: List[String],  // Path into state: ["user", "name"]
      conditional: Boolean,     // Is this inside a conditional?
      transform: Option[String], // Optional JS transform expression
      sourceExpr: TypedExpr[A]  // Reference to source expression (immutable)
  )

  /**
   * Condition for a conditional binding.
   * Used when binding is inside a match expression.
   */
  final case class BranchCondition(
      path: List[String],
      pattern: String
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
      var bindingToPath: Map[Bindable, List[String]] = Map.empty
  ) {
    def freshId(): String = {
      val id = s"bosatsu-$idCounter"
      idCounter += 1
      id
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
   */
  private def analyzeExpr[A](expr: TypedExpr[A], ctx: AnalysisContext[A]): Unit = {
    expr match {
      // Let binding - track the binding and analyze value and body
      case TypedExpr.Let(name, value, body, _, _) =>
        // Check if this is an IO.read operation
        extractStateRead(value) match {
          case Some(path) =>
            ctx.recordStateRead(path)
            ctx.trackBinding(name, path)
          case None =>
            // Check if value depends on tracked bindings
            val freeVars = TypedExpr.freeVarsSet(List(value))
            freeVars.headOption.flatMap(ctx.getPath).foreach { path =>
              ctx.trackBinding(name, path)
            }
        }
        analyzeExpr(value, ctx)
        analyzeExpr(body, ctx)

      // Function application - check for h(), text(), or event handlers
      case app: TypedExpr.App[A @unchecked] =>
        extractUIConstruction(app, ctx)
        // Also analyze arguments
        analyzeExpr(app.fn, ctx)
        app.args.toList.foreach(arg => analyzeExpr(arg, ctx))

      // Match expression - mark as conditional
      case TypedExpr.Match(arg, branches, _) =>
        analyzeExpr(arg, ctx)
        val wasConditional = ctx.inConditional
        ctx.inConditional = true
        branches.toList.foreach { case (_, branchExpr) =>
          analyzeExpr(branchExpr, ctx)
        }
        ctx.inConditional = wasConditional

      // Lambda - analyze body
      case TypedExpr.AnnotatedLambda(_, body, _) =>
        analyzeExpr(body, ctx)

      // Annotation wrapper
      case TypedExpr.Annotation(inner, _) =>
        analyzeExpr(inner, ctx)

      // Generic wrapper
      case TypedExpr.Generic(_, inner) =>
        analyzeExpr(inner, ctx)

      // Local variable reference - check if it's a tracked binding
      case TypedExpr.Local(name, _, _) =>
        // Just reading a local - will be used by parent
        ()

      // Global reference
      case TypedExpr.Global(_, _, _, _) =>
        // Global - could be a predef function
        ()

      // Literal
      case TypedExpr.Literal(_, _, _) =>
        // Constant value
        ()
    }
  }

  // ---------------------------------------------------------------------------
  // IO.read Detection
  // ---------------------------------------------------------------------------

  /**
   * Check if an expression is an IO.read operation and extract the state path.
   *
   * Looks for patterns like:
   *   IO.read(["path", "to", "state"])
   *   Bosatsu/IO::read(["path"])
   */
  private def extractStateRead[A](expr: TypedExpr[A]): Option[List[String]] = {
    expr match {
      case TypedExpr.App(fn, args, _, _) =>
        fn match {
          // Check for IO.read or Bosatsu/IO::read
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
   * Looks for h(), text(), and event handler patterns.
   */
  private def extractUIConstruction[A](
      app: TypedExpr.App[A],
      ctx: AnalysisContext[A]
  ): Unit = {
    app.fn match {
      case TypedExpr.Global(pack, name, _, _) if isUIPackage(pack) =>
        name.asString match {
          case "h" =>
            // h(tag, props, children) - element construction
            extractElementBindings(app, ctx)

          case "text" =>
            // text(content) - text node, parent will handle binding
            extractTextBinding(app, ctx)

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
   * Extract bindings from an h() element construction.
   *
   * Looks at children for text nodes that reference state.
   * Looks at props for className, value, checked, disabled bindings.
   */
  private def extractElementBindings[A](
      app: TypedExpr.App[A],
      ctx: AnalysisContext[A]
  ): Unit = {
    val args = app.args.toList

    // Generate element ID for this element
    val elementId = ctx.freshId()

    // Check props (second argument) for state bindings
    if (args.length >= 2) {
      extractPropsBindings(args(1), elementId, ctx)
    }

    // Check children (third argument) for text bindings
    if (args.length >= 3) {
      extractChildrenBindings(args(2), elementId, ctx)
    }
  }

  /**
   * Extract bindings from props object.
   */
  private def extractPropsBindings[A](
      propsExpr: TypedExpr[A],
      elementId: String,
      ctx: AnalysisContext[A]
  ): Unit = {
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
                  conditional = ctx.inConditional,
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
   * Extract bindings from children array.
   */
  private def extractChildrenBindings[A](
      childrenExpr: TypedExpr[A],
      parentElementId: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    // Children is typically a list construction
    childrenExpr match {
      case TypedExpr.App(fn, args, _, _) =>
        // Process each child
        args.toList.foreach { child =>
          extractTextBindingFromChild(child, parentElementId, ctx)
        }
      case _ => ()
    }
  }

  /**
   * Extract text binding from a child node.
   */
  private def extractTextBindingFromChild[A](
      childExpr: TypedExpr[A],
      parentElementId: String,
      ctx: AnalysisContext[A]
  ): Unit = {
    childExpr match {
      // text(localVar) where localVar is bound to state
      case TypedExpr.App(fn, args, _, _) =>
        fn match {
          case TypedExpr.Global(pack, name, _, _)
              if isUIPackage(pack) && name.asString == "text" =>
            args.head match {
              case local @ TypedExpr.Local(name, _, _) =>
                ctx.getPath(name).foreach { path =>
                  ctx.recordBinding(DOMBinding(
                    elementId = parentElementId,
                    property = DOMProperty.TextContent,
                    statePath = path,
                    conditional = ctx.inConditional,
                    transform = None,
                    sourceExpr = local
                  ))
                }
              case _ => ()
            }
          case _ => ()
        }
      case _ => ()
    }
  }

  /**
   * Extract text binding from a text() call.
   */
  private def extractTextBinding[A](
      app: TypedExpr.App[A],
      ctx: AnalysisContext[A]
  ): Unit = {
    // text(content) - check if content is a tracked binding
    app.args.head match {
      case TypedExpr.Local(name, _, _) =>
        ctx.getPath(name).foreach { path =>
          ctx.recordStateRead(path)
        }
      case _ => ()
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
   */
  def bindingsToJs[A](bindings: List[DOMBinding[A]]): String = {
    val entries = createBindingMap(bindings).map { case (pathKey, bs) =>
      val bindingArrays = bs.map { b =>
        val props = List(
          s""""elementId": "${b.elementId}"""",
          s""""property": "${DOMProperty.toJsProperty(b.property)}"""",
          s""""conditional": ${b.conditional}"""
        ) ++ b.transform.map(t => s""""transform": "$t"""")
        s"{${props.mkString(", ")}}"
      }
      s""""$pathKey": [${bindingArrays.mkString(", ")}]"""
    }
    s"{${entries.mkString(",\n  ")}}"
  }
}
