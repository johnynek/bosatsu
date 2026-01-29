package dev.bosatsu.ui

import dev.bosatsu.{Identifier, TypedExpr}

/**
 * Virtual DOM representation for BosatsuUI.
 *
 * VNode is an immutable tree structure representing DOM elements.
 * It serves as an intermediate representation between Bosatsu UI
 * expressions and actual DOM manipulation.
 *
 * Design follows React/Elm patterns with three node types:
 * - Element: HTML elements with tag, attributes, and children
 * - Text: Plain text content
 * - Component: Reusable UI components with props and render function
 */
sealed abstract class VNode extends Product with Serializable derives CanEqual {
  /** Get optional key for reconciliation */
  def key: Option[String]
}

object VNode {

  /**
   * HTML element node.
   *
   * @param tag HTML tag name (e.g., "div", "span", "button")
   * @param attributes Element attributes (e.g., "class" -> "btn", "id" -> "main")
   * @param eventHandlers Event handlers keyed by event type (e.g., "click", "input")
   * @param children Child nodes
   * @param key Optional key for efficient list reconciliation
   */
  final case class Element(
      tag: String,
      attributes: Map[String, AttributeValue],
      eventHandlers: Map[String, EventHandler],
      children: List[VNode],
      key: Option[String]
  ) extends VNode

  /**
   * Plain text node.
   *
   * @param content Text content (will be escaped when rendered)
   */
  final case class Text(content: String) extends VNode {
    def key: Option[String] = None
  }

  /**
   * Component node - a reusable UI building block.
   *
   * Components encapsulate rendering logic and can receive props.
   * They are expanded to their rendered VNode tree during reconciliation.
   *
   * @param name Component name for debugging and dev tools
   * @param props Properties passed to the component
   * @param render Function that produces the component's VNode tree
   * @param key Optional key for efficient list reconciliation
   */
  final case class Component(
      name: String,
      props: Map[String, AttributeValue],
      render: () => VNode,
      key: Option[String]
  ) extends VNode

  // Smart constructors for common patterns

  /** Create an element with just tag and children */
  def el(tag: String, children: VNode*): Element =
    Element(tag, Map.empty, Map.empty, children.toList, None)

  /** Create an element with attributes and children */
  def el(tag: String, attrs: Map[String, AttributeValue], children: VNode*): Element =
    Element(tag, attrs, Map.empty, children.toList, None)

  /** Create a text node */
  def text(content: String): Text = Text(content)

  /** Create a keyed element */
  def keyedEl(tag: String, key: String, children: VNode*): Element =
    Element(tag, Map.empty, Map.empty, children.toList, Some(key))

  // Common HTML elements

  def div(children: VNode*): Element = el("div", children*)
  def span(children: VNode*): Element = el("span", children*)
  def p(children: VNode*): Element = el("p", children*)
  def h1(children: VNode*): Element = el("h1", children*)
  def h2(children: VNode*): Element = el("h2", children*)
  def h3(children: VNode*): Element = el("h3", children*)
  def button(children: VNode*): Element = el("button", children*)
  def input(): Element = el("input")
  def ul(children: VNode*): Element = el("ul", children*)
  def li(children: VNode*): Element = el("li", children*)
}

/**
 * Attribute value for VNode elements.
 *
 * Attributes can be strings, numbers, booleans, or null (for removal).
 */
sealed abstract class AttributeValue extends Product with Serializable derives CanEqual

object AttributeValue {
  final case class StringValue(value: String) extends AttributeValue
  final case class IntValue(value: Int) extends AttributeValue
  final case class DoubleValue(value: Double) extends AttributeValue
  final case class BoolValue(value: Boolean) extends AttributeValue
  case object NullValue extends AttributeValue

  // Implicit conversions for ergonomic API
  implicit def fromString(s: String): AttributeValue = StringValue(s)
  implicit def fromInt(i: Int): AttributeValue = IntValue(i)
  implicit def fromDouble(d: Double): AttributeValue = DoubleValue(d)
  implicit def fromBoolean(b: Boolean): AttributeValue = BoolValue(b)

  /** Render attribute value to string for HTML */
  def render(av: AttributeValue): Option[String] = av match {
    case StringValue(s) => Some(s)
    case IntValue(i)    => Some(i.toString)
    case DoubleValue(d) => Some(d.toString)
    case BoolValue(b)   => if (b) Some("") else None // Boolean attributes
    case NullValue      => None
  }
}

/**
 * Event handler for DOM events.
 *
 * Wraps a Bosatsu expression that handles the event.
 * The handler receives event data and may produce state updates.
 */
final case class EventHandler(
    eventType: String,
    handler: TypedExpr[Any],
    preventDefault: Boolean = false,
    stopPropagation: Boolean = false
)

/**
 * Result of analyzing DOM bindings from a component.
 *
 * This is produced by static analysis of Bosatsu UI code to determine:
 * - Which state fields the component reads (for subscription optimization)
 * - Which state fields the component writes (for ownership tracking)
 * - What event handlers are attached
 * - Where the component renders
 */
final case class DOMBindings(
    stateReads: Set[Identifier.Bindable],
    stateWrites: Set[Identifier.Bindable],
    eventHandlers: List[EventHandler],
    renderTarget: Option[String]
)

object DOMBindings {
  val empty: DOMBindings = DOMBindings(Set.empty, Set.empty, Nil, None)
}
