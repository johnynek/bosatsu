package dev.bosatsu.ui

import dev.bosatsu.{TypedExpr, Identifier, PackageName}

/**
 * Event system for BosatsuUI.
 *
 * Defines types for event handlers and actions that can be triggered
 * from UI elements. Mirrors burritoUI's events.ts patterns.
 *
 * The key insight: Event handlers are compiled to state update functions.
 * When an event fires:
 * 1. The handler expression is evaluated
 * 2. State is updated via the produced action
 * 3. Bindings are automatically updated via the runtime
 */
object Events {

  // ---------------------------------------------------------------------------
  // Event Types
  // ---------------------------------------------------------------------------

  /**
   * Supported DOM event types.
   */
  sealed trait EventType derives CanEqual {
    def name: String
  }

  object EventType {
    case object Click extends EventType { val name = "click" }
    case object Input extends EventType { val name = "input" }
    case object Change extends EventType { val name = "change" }
    case object Submit extends EventType { val name = "submit" }
    case object Focus extends EventType { val name = "focus" }
    case object Blur extends EventType { val name = "blur" }
    case object KeyDown extends EventType { val name = "keydown" }
    case object KeyUp extends EventType { val name = "keyup" }
    case object MouseEnter extends EventType { val name = "mouseenter" }
    case object MouseLeave extends EventType { val name = "mouseleave" }

    def fromString(s: String): Option[EventType] = s.toLowerCase match {
      case "click"      => Some(Click)
      case "input"      => Some(Input)
      case "change"     => Some(Change)
      case "submit"     => Some(Submit)
      case "focus"      => Some(Focus)
      case "blur"       => Some(Blur)
      case "keydown"    => Some(KeyDown)
      case "keyup"      => Some(KeyUp)
      case "mouseenter" => Some(MouseEnter)
      case "mouseleave" => Some(MouseLeave)
      case _            => None
    }

    val all: List[EventType] = List(
      Click, Input, Change, Submit, Focus, Blur, KeyDown, KeyUp, MouseEnter, MouseLeave
    )
  }

  // ---------------------------------------------------------------------------
  // Actions
  // ---------------------------------------------------------------------------

  /**
   * An action that can be triggered by an event handler.
   * Actions describe state mutations in a declarative way.
   */
  sealed trait Action[+A] derives CanEqual

  object Action {
    /**
     * Set state at a path to a value.
     * @param path Path into state
     * @param value Expression producing the new value
     */
    final case class SetState[A](
        path: List[String],
        value: TypedExpr[A]
    ) extends Action[A]

    /**
     * Update state at a path using a function.
     * @param path Path into state
     * @param fn Function (old -> new) to apply
     */
    final case class UpdateState[A](
        path: List[String],
        fn: TypedExpr[A]
    ) extends Action[A]

    /**
     * Batch multiple actions together.
     * All actions are applied atomically with a single DOM update.
     */
    final case class Batch[A](actions: List[Action[A]]) extends Action[A]

    /**
     * Dispatch a named action (for reducer-style state management).
     * @param actionType The action type name
     * @param payload Optional payload data
     */
    final case class Dispatch[A](
        actionType: String,
        payload: Option[TypedExpr[A]]
    ) extends Action[A]

    /**
     * No-op action - useful for conditional handlers.
     */
    case object NoOp extends Action[Nothing]
  }

  // ---------------------------------------------------------------------------
  // Event Handler Types
  // ---------------------------------------------------------------------------

  /**
   * Configuration for an event handler.
   *
   * @param eventType The DOM event type to listen for
   * @param action The action to perform when the event fires
   * @param preventDefault Whether to call event.preventDefault()
   * @param stopPropagation Whether to call event.stopPropagation()
   */
  final case class HandlerConfig[A](
      eventType: EventType,
      action: Action[A],
      preventDefault: Boolean = false,
      stopPropagation: Boolean = false
  )

  // ---------------------------------------------------------------------------
  // Extraction from TypedExpr
  // ---------------------------------------------------------------------------

  /**
   * Extract event handler from a props expression.
   *
   * Looks for patterns like:
   *   { "onclick": IO.write(["count"], add(count, 1)) }
   *   { "onsubmit": IO.batch([...]) }
   */
  def extractEventHandlers[A](propsExpr: TypedExpr[A]): List[HandlerConfig[A]] = {
    // This would need to walk the props structure looking for "on*" keys
    // and extract the handler expressions
    // For now, return empty - implementation depends on exact Bosatsu syntax
    Nil
  }

  /**
   * Extract SetState action from an IO.write call.
   */
  def extractSetState[A](expr: TypedExpr[A]): Option[Action.SetState[A]] = {
    expr match {
      case TypedExpr.App(fn, args, _, _) =>
        fn match {
          case TypedExpr.Global(pack, name, _, _)
              if isIOPackage(pack) && name.asString == "write" =>
            val path = extractPathFromArg(args.head)
            path.map(p => Action.SetState(p, args.toList(1)))
          case _ => None
        }
      case _ => None
    }
  }

  private def isIOPackage(pack: PackageName): Boolean =
    pack.asString == "Bosatsu/IO" || pack.asString == "IO"

  private def extractPathFromArg[A](expr: TypedExpr[A]): Option[List[String]] = {
    // Extract ["path", "components"] from list literal
    // Implementation depends on how lists are represented
    None
  }

  // ---------------------------------------------------------------------------
  // Code Generation Helpers
  // ---------------------------------------------------------------------------

  /**
   * Generate JavaScript for an event handler.
   */
  def generateHandlerJs[A](config: HandlerConfig[A], handlerCode: String): String = {
    val modifiers = List(
      if (config.preventDefault) "e.preventDefault();" else "",
      if (config.stopPropagation) "e.stopPropagation();" else ""
    ).filter(_.nonEmpty).mkString("\n    ")

    s"""function(e) {
    $modifiers
    $handlerCode
  }"""
  }

  /**
   * Generate JavaScript for setting up event listeners.
   *
   * @param handlers Map of elementId -> List of handlers
   * @return JavaScript code for addEventListener calls
   */
  def generateEventSetup(handlers: Map[String, List[(String, String)]]): String = {
    val setupCode = handlers.map { case (elementId, eventHandlers) =>
      val selector = if (elementId.startsWith("#") || elementId.startsWith("["))
        elementId
      else
        s"""[data-bosatsu-id="$elementId"]"""

      val listeners = eventHandlers.map { case (eventType, handlerCode) =>
        s"""  _findElement('$selector').addEventListener('$eventType', $handlerCode);"""
      }.mkString("\n")

      s"""// Event listeners for $elementId
$listeners"""
    }.mkString("\n\n")

    s"""// Set up event listeners
(function() {
  function _findElement(sel) {
    return document.querySelector(sel);
  }

$setupCode
})();"""
  }
}
