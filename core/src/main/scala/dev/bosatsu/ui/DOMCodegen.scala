package dev.bosatsu.ui

import cats.data.NonEmptyList
import dev.bosatsu.codegen.js.Code
import dev.bosatsu.codegen.js.Code._
import org.typelevel.paiges.Doc

/**
 * Generates JavaScript DOM manipulation code from VNode trees.
 *
 * This codegen produces native JavaScript that directly creates and
 * manipulates DOM elements, avoiding the overhead of a virtual DOM
 * diffing algorithm at runtime.
 *
 * The generated code follows these patterns:
 * - document.createElement for elements
 * - element.setAttribute for attributes
 * - element.addEventListener for event handlers
 * - document.createTextNode for text content
 * - element.appendChild for building the tree
 */
object DOMCodegen {

  // Helper to create method calls
  private def methodCall(obj: Expression, method: String, args: List[Expression]): Call =
    Call(PropertyAccess(obj, method), args)

  /**
   * Generate JavaScript code to create a DOM element from a VNode.
   *
   * @param vnode The virtual node to render
   * @param varName Variable name to assign the created element
   * @return JavaScript statements that create and configure the element
   */
  def generate(vnode: VNode, varName: String): List[Statement] =
    vnode match {
      case VNode.Element(tag, attrs, handlers, children, _) =>
        generateElement(tag, attrs, handlers, children, varName)
      case VNode.Text(content) =>
        generateText(content, varName)
      case VNode.Component(name, props, render, _) =>
        // Components are expanded at codegen time
        generate(render(), varName)
    }

  /**
   * Generate code for an HTML element.
   */
  private def generateElement(
      tag: String,
      attrs: Map[String, AttributeValue],
      handlers: Map[String, EventHandler],
      children: List[VNode],
      varName: String
  ): List[Statement] = {
    // Create element: const varName = document.createElement(tag)
    val createElement = Const(
      varName,
      methodCall(Ident("document"), "createElement", List(StringLiteral(tag)))
    )

    // Set attributes: varName.setAttribute(name, value)
    val setAttrs = attrs.toList.flatMap { case (name, value) =>
      AttributeValue.render(value).map { v =>
        ExprStatement(
          methodCall(Ident(varName), "setAttribute", List(StringLiteral(name), StringLiteral(v)))
        )
      }.toList
    }

    // Add event listeners: varName.addEventListener(type, handler)
    val addListeners = handlers.toList.map { case (eventType, handler) =>
      val listenerBody: List[Statement] = {
        val preventDefaultStmt =
          if (handler.preventDefault)
            Some(ExprStatement(methodCall(Ident("event"), "preventDefault", Nil)))
          else None

        val stopPropStmt =
          if (handler.stopPropagation)
            Some(ExprStatement(methodCall(Ident("event"), "stopPropagation", Nil)))
          else None

        // TODO: Call the Bosatsu handler - placeholder for now
        val handlerCall = ExprStatement(UndefinedLiteral)

        (preventDefaultStmt.toList ++ stopPropStmt.toList :+ handlerCall)
      }

      val listenerFn = listenerBody match {
        case single :: Nil =>
          // Single statement can use expression form
          ArrowFunction(List("event"), UndefinedLiteral)
        case _ =>
          // Multiple statements need block form
          ArrowFunction(
            List("event"),
            Block(NonEmptyList.fromListUnsafe(listenerBody))
          )
      }

      ExprStatement(
        methodCall(Ident(varName), "addEventListener", List(StringLiteral(eventType), listenerFn))
      )
    }

    // Generate children and append them
    val childStatements = children.zipWithIndex.flatMap { case (child, idx) =>
      val childVar = s"${varName}_child$idx"
      val createChild = generate(child, childVar)
      val appendStmt = ExprStatement(
        methodCall(Ident(varName), "appendChild", List(Ident(childVar)))
      )
      createChild :+ appendStmt
    }

    createElement :: setAttrs ::: addListeners ::: childStatements
  }

  /**
   * Generate code for a text node.
   */
  private def generateText(content: String, varName: String): List[Statement] = {
    List(
      Const(
        varName,
        methodCall(Ident("document"), "createTextNode", List(StringLiteral(content)))
      )
    )
  }

  /**
   * Generate a complete render function that creates and mounts a VNode tree.
   *
   * @param vnode The root VNode to render
   * @param functionName Name of the generated function
   * @param mountPoint CSS selector for the mount point
   * @return A JavaScript const declaration with the render function
   */
  def generateRenderFunction(
      vnode: VNode,
      functionName: String,
      mountPoint: String
  ): Statement = {
    val rootVar = "root"
    val createStatements = generate(vnode, rootVar)

    val mountStatements = List(
      Const("container", methodCall(Ident("document"), "querySelector", List(StringLiteral(mountPoint)))),
      ExprStatement(methodCall(Ident("container"), "appendChild", List(Ident(rootVar))))
    )

    val allStatements = createStatements ::: mountStatements

    Const(
      functionName,
      Function(
        None,
        Nil,
        Block(NonEmptyList.fromListUnsafe(allStatements))
      )
    )
  }

  /**
   * Generate JavaScript module that exports a render function.
   *
   * @param vnode The root VNode
   * @param moduleName Name for the module (unused, kept for API compatibility)
   * @return Complete JavaScript module code as Doc
   */
  def generateModule(vnode: VNode, moduleName: String): Doc = {
    val renderFn = generateRenderFunction(vnode, "render", "#app")
    val exportStmt = Export("render")

    Code.toDoc(Statements(List(renderFn, exportStmt)))
  }

  /**
   * Render a VNode tree to an HTML string (for SSR or testing).
   *
   * @param vnode The VNode to render
   * @return HTML string representation
   */
  def renderToString(vnode: VNode): String = vnode match {
    case VNode.Element(tag, attrs, _, children, _) =>
      val attrStr = attrs.toList.flatMap { case (name, value) =>
        AttributeValue.render(value).map(v => s"""$name="${escapeAttr(v)}"""")
      }.mkString(" ")

      val childStr = children.map(renderToString).mkString

      if (voidElements.contains(tag)) {
        if (attrStr.isEmpty) s"<$tag>" else s"<$tag $attrStr>"
      } else {
        if (attrStr.isEmpty) s"<$tag>$childStr</$tag>"
        else s"<$tag $attrStr>$childStr</$tag>"
      }

    case VNode.Text(content) =>
      escapeHtml(content)

    case VNode.Component(_, _, render, _) =>
      renderToString(render())
  }

  // HTML void elements (self-closing)
  private val voidElements: Set[String] = Set(
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "link", "meta", "param", "source", "track", "wbr"
  )

  // Escape special HTML characters in text content
  private def escapeHtml(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")

  // Escape special characters in attribute values
  private def escapeAttr(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&#39;")
}
