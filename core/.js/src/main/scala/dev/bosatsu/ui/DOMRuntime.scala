package dev.bosatsu.ui

import scala.scalajs.js
import scala.scalajs.js.annotation._

/**
 * Scala.js runtime for DOM manipulation using raw JS interop.
 *
 * Provides type-safe wrappers around native DOM APIs for:
 * - Element creation and configuration
 * - Attribute manipulation
 * - Event handling
 * - Tree construction
 *
 * This runtime uses js.Dynamic to avoid external dependencies
 * like scalajs-dom while still providing DOM manipulation capabilities.
 */
object DOMRuntime {

  /** Reference to the global document object */
  private val document: js.Dynamic = js.Dynamic.global.document

  /** Type alias for DOM nodes (opaque wrapper) */
  type Node = js.Dynamic
  type Element = js.Dynamic
  type Text = js.Dynamic
  type Event = js.Dynamic

  /**
   * Create a DOM element with the given tag name.
   *
   * @param tag HTML tag name (e.g., "div", "span", "button")
   * @return The created DOM element
   */
  def createElement(tag: String): Element =
    document.createElement(tag)

  /**
   * Create a text node with the given content.
   *
   * @param content Text content
   * @return The created text node
   */
  def createTextNode(content: String): Text =
    document.createTextNode(content)

  /**
   * Set an attribute on an element.
   *
   * @param element The target element
   * @param name Attribute name
   * @param value Attribute value
   */
  def setAttribute(element: Element, name: String, value: String): Unit =
    element.setAttribute(name, value)

  /**
   * Remove an attribute from an element.
   *
   * @param element The target element
   * @param name Attribute name to remove
   */
  def removeAttribute(element: Element, name: String): Unit =
    element.removeAttribute(name)

  /**
   * Set multiple attributes on an element.
   *
   * @param element The target element
   * @param attrs Map of attribute names to values
   */
  def setAttributes(element: Element, attrs: Map[String, AttributeValue]): Unit =
    attrs.foreach { case (name, value) =>
      AttributeValue.render(value) match {
        case Some(v) => element.setAttribute(name, v)
        case None => element.removeAttribute(name)
      }
    }

  /**
   * Add an event listener to an element.
   *
   * @param element The target element
   * @param eventType Event type (e.g., "click", "input")
   * @param handler Event handler function
   * @return The wrapped JS handler, needed for removeEventListener
   */
  def addEventListener(element: Element, eventType: String, handler: Event => Unit): js.Function1[js.Dynamic, Unit] = {
    val jsHandler: js.Function1[js.Dynamic, Unit] = (e: js.Dynamic) => handler(e)
    element.addEventListener(eventType, jsHandler)
    jsHandler
  }

  /**
   * Remove an event listener from an element.
   * Note: The same handler reference must be used as was passed to addEventListener.
   *
   * @param element The target element
   * @param eventType Event type
   * @param handler Event handler to remove (wrapped as js.Function1)
   */
  def removeEventListener(element: Element, eventType: String, handler: js.Function1[js.Dynamic, Unit]): Unit =
    element.removeEventListener(eventType, handler)

  /**
   * Append a child node to a parent element.
   *
   * @param parent Parent element
   * @param child Child node to append
   */
  def appendChild(parent: Element, child: Node): Unit =
    parent.appendChild(child)

  /**
   * Remove a child node from a parent element.
   *
   * @param parent Parent element
   * @param child Child node to remove
   */
  def removeChild(parent: Element, child: Node): Unit =
    parent.removeChild(child)

  /**
   * Replace a child node with a new node.
   *
   * @param parent Parent element
   * @param newChild New child node
   * @param oldChild Old child node to replace
   */
  def replaceChild(parent: Element, newChild: Node, oldChild: Node): Unit =
    parent.replaceChild(newChild, oldChild)

  /**
   * Insert a node before another node.
   *
   * @param parent Parent element
   * @param newNode Node to insert
   * @param referenceNode Node to insert before (null to append)
   */
  def insertBefore(parent: Element, newNode: Node, referenceNode: Node): Unit =
    parent.insertBefore(newNode, referenceNode)

  /**
   * Set the text content of an element.
   *
   * @param element The target element
   * @param content Text content
   */
  def setTextContent(element: Element, content: String): Unit =
    element.textContent = content

  /**
   * Get the text content of an element.
   *
   * @param element The target element
   * @return Text content
   */
  def getTextContent(element: Element): String =
    element.textContent.asInstanceOf[String]

  /**
   * Set the inner HTML of an element.
   * Use with caution - can introduce XSS vulnerabilities.
   *
   * @param element The target element
   * @param html HTML content
   */
  def setInnerHTML(element: Element, html: String): Unit =
    element.innerHTML = html

  /**
   * Query for an element using a CSS selector.
   *
   * @param selector CSS selector
   * @return The first matching element, or null
   */
  def querySelector(selector: String): Element =
    document.querySelector(selector)

  /**
   * Query for all elements matching a CSS selector.
   *
   * @param selector CSS selector
   * @return List of matching elements
   */
  def querySelectorAll(selector: String): List[Element] = {
    val nodeList = document.querySelectorAll(selector)
    val length = nodeList.length.asInstanceOf[Int]
    (0 until length).map(i => nodeList(i).asInstanceOf[Element]).toList
  }

  /**
   * Get an element by its ID.
   *
   * @param id Element ID
   * @return The element, or null if not found
   */
  def getElementById(id: String): Element =
    document.getElementById(id)

  /**
   * Add a CSS class to an element.
   *
   * @param element The target element
   * @param className CSS class name
   */
  def addClass(element: Element, className: String): Unit =
    element.classList.add(className)

  /**
   * Remove a CSS class from an element.
   *
   * @param element The target element
   * @param className CSS class name
   */
  def removeClass(element: Element, className: String): Unit =
    element.classList.remove(className)

  /**
   * Toggle a CSS class on an element.
   *
   * @param element The target element
   * @param className CSS class name
   * @return true if class was added, false if removed
   */
  def toggleClass(element: Element, className: String): Boolean =
    element.classList.toggle(className).asInstanceOf[Boolean]

  /**
   * Check if an element has a CSS class.
   *
   * @param element The target element
   * @param className CSS class name
   * @return true if element has the class
   */
  def hasClass(element: Element, className: String): Boolean =
    element.classList.contains(className).asInstanceOf[Boolean]

  /**
   * Set a style property on an element.
   *
   * @param element The target element
   * @param property CSS property name
   * @param value CSS property value
   */
  def setStyle(element: Element, property: String, value: String): Unit =
    element.style.updateDynamic(property)(value)

  /**
   * Prevent the default action for an event.
   *
   * @param event The event
   */
  def preventDefault(event: Event): Unit =
    event.preventDefault()

  /**
   * Stop event propagation.
   *
   * @param event The event
   */
  def stopPropagation(event: Event): Unit =
    event.stopPropagation()

  /**
   * Render a VNode tree to the DOM.
   *
   * @param vnode The virtual node to render
   * @return The created DOM node
   */
  def render(vnode: VNode): Node = vnode match {
    case VNode.Element(tag, attrs, handlers, children, _) =>
      val element = createElement(tag)
      setAttributes(element, attrs)
      handlers.foreach { case (eventType, handler) =>
        addEventListener(element, eventType, (e: Event) => {
          if (handler.preventDefault) preventDefault(e)
          if (handler.stopPropagation) stopPropagation(e)
          // TODO: Execute Bosatsu handler expression
        })
      }
      children.foreach(child => appendChild(element, render(child)))
      element

    case VNode.Text(content) =>
      createTextNode(content)

    case VNode.Component(_, _, renderFn, _) =>
      render(renderFn())
  }

  /**
   * Mount a VNode tree to a container element.
   *
   * @param vnode The virtual node to mount
   * @param container The container element (CSS selector)
   */
  def mount(vnode: VNode, container: String): Unit = {
    val containerEl = querySelector(container)
    if (containerEl != null && !js.isUndefined(containerEl)) {
      // Clear existing content
      containerEl.innerHTML = ""
      appendChild(containerEl, render(vnode))
    }
  }

  /**
   * Mount a VNode tree to a container element by ID.
   *
   * @param vnode The virtual node to mount
   * @param containerId The container element ID
   */
  def mountById(vnode: VNode, containerId: String): Unit = {
    val containerEl = getElementById(containerId)
    if (containerEl != null && !js.isUndefined(containerEl)) {
      containerEl.innerHTML = ""
      appendChild(containerEl, render(vnode))
    }
  }
}
